# ============================================================
# Global Ecology Bluesky Digest Generator
# ------------------------------------------------------------
# Robust, SEO-friendly generator:
#   * Each digest lives at a stable permalink:
#       /archives/digest-<N>/    (canonical URL)
#   * Homepage is a stable landing page that previews the
#     latest digest, so its title/description don't churn
#     week-to-week.
#   * Front matter feeds jekyll-seo-tag, which emits
#     canonical, OpenGraph, Twitter Card, and JSON-LD
#     BlogPosting automatically.
#   * Atomic writes so a crash mid-run never leaves the
#     repo in a half-written state.
#   * Per-post try/catch -- one bad post can't kill the build.
#
# Required Jekyll plugins (add to _config.yml -- see the
# patch shipped alongside this file):
#   - jekyll-seo-tag
#   - jekyll-sitemap
#   - jekyll-feed
# ============================================================

# ---- Auth --------------------------------------------------
# Both keys come from the environment (Sys.setenv or .Renviron).
# Required: BLUESKY_PASS, ANTHROPIC_API_KEY (only if LLM titles
# are enabled in CONFIG below).
.bluesky_pass <- Sys.getenv("BLUESKY_PASS", unset = "")
if (!nzchar(.bluesky_pass)) {
  stop("BLUESKY_PASS env var is not set. Add it via Sys.setenv() or ~/.Renviron.")
}
bskyr::set_bluesky_user('nmouquet.bsky.social')
bskyr::set_bluesky_pass(.bluesky_pass)
rm(.bluesky_pass)

# ---- Config ------------------------------------------------
CONFIG <- list(
  site_url         = "https://globalecologybs.github.io",
  base_url         = "/feeddigest.github.io",
  site_title       = "Global Ecology Digest",
  site_tagline     = "Weekly curated digest of the Bluesky Global Ecology feed",
  social_image     = "https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology.jpg",
  banner_image     = "https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology_banner.png",

  # NULL = auto-detect next number from archives/.
  digest_number    = NULL,
  days_back        = 14,
  min_text_length  = 50,
  feed_limit       = 150,
  feed_uri         = 'at://did:plc:ppsghcl5bbpgjcljnhra353s/app.bsky.feed.generator/global.ecology',
  archives_dir     = "archives",
  data_dir         = "_data",         # Jekyll convention
  data_filename    = "digests.yml",   # used by the sidebar layout

  # Feature flags.
  # `enable_titles`     : pull paper title from the linked DOI/article page.
  # `enable_llm_titles` : if fetch fails or there is no link,
  #                       generate a scientific title from the post
  #                       text with an LLM. Both caches live under
  #                       archives/ so only first-seen posts cost time/money.
  enable_tags        = TRUE,
  enable_llm_tags    = TRUE,    # LLM classifier (beats regex on names like "Forest Isbell")
  enable_titles      = TRUE,
  enable_llm_titles  = TRUE,
  enable_summaries   = FALSE,

  # Maximum number of tags rendered per post (most-specific kept).
  max_tags_per_post  = 4,

  # Network safety for paper-title fetcher.
  fetch_timeout_s  = 10,

  # LLM config. Requires ANTHROPIC_API_KEY in the environment
  # (add `Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-...")` to pass.R,
  # or export it in your shell). 
  llm_provider  = "anthropic",
  llm_model     = "claude-sonnet-4-6"
)

# ---- Tiny helpers ------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

safe <- function(expr, default = NULL) {
  tryCatch(expr, error = function(e) default)
}

# Atomic write: never leaves a partial file on disk.
write_atomic <- function(text, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp")
  writeLines(text, tmp, useBytes = TRUE)
  file.rename(tmp, path)
}

next_digest_number <- function(archives_dir) {
  if (!dir.exists(archives_dir)) return(1L)
  files <- list.files(archives_dir, pattern = "^digest-\\d+\\.md$")
  if (length(files) == 0) return(1L)
  nums <- suppressWarnings(as.integer(gsub("^digest-(\\d+)\\.md$", "\\1", files)))
  max(nums, na.rm = TRUE) + 1L
}

list_digests <- function(archives_dir) {
  files <- list.files(archives_dir, pattern = "^digest-\\d+\\.md$")
  if (length(files) == 0) return(data.frame(num = integer(), file = character(), stringsAsFactors = FALSE))
  nums <- suppressWarnings(as.integer(gsub("^digest-(\\d+)\\.md$", "\\1", files)))
  out  <- data.frame(num = nums, file = files, stringsAsFactors = FALSE)
  out[order(out$num, decreasing = TRUE), ]
}

# ---- Topic tags --------------------------------------------
# Lightweight keyword classifier. Looks at the post text AND
# (when available) the resolved paper title, since titles are
# usually the cleanest topic signal. Ranking matters: each rule's
# match advances its score; the top `max_tags_per_post` tags are
# returned in priority order (specific subjects before broad ones).
classify_post <- function(text, paper_title = NULL,
                          max_tags = CONFIG$max_tags_per_post %||% 4) {
  combined <- paste(text, paper_title %||% "")
  t <- tolower(combined)
  # Title contributes extra weight if present.
  pt <- tolower(paper_title %||% "")
  pt_boost <- function(re) if (nzchar(pt) && grepl(re, pt)) 1L else 0L

  # Each rule -> (tag, score). Order = tiebreak priority.
  rules <- list(
    list("jobs",         "\\bjob\\b|\\bphd\\b|postdoc|fellowship|hiring|position open"),
    list("events",       "webinar|seminar|conference|workshop|symposium|deadline"),
    list("invasives",    "invasive|alien species|non[- ]indigenous|biofouling|bioinvasion"),
    list("marine",       "marine|ocean|\\bsea\\b|reef|coral|fish\\b|fisheries|kelp|seagrass|cetacean|plankton"),
    list("freshwater",   "freshwater|\\briver|\\blake|stream|wetland|estuar|riparian"),
    list("forest",       "forest|woodland|\\btree\\b|canopy|deforest|reforest|silvicult"),
    list("soil",         "\\bsoil|microbi|\\bfung|mycorrh|nematode"),
    list("climate",      "climate|warming|drought|heatwave|carbon sink|\\bco2\\b|greenhouse"),
    list("methods",      "\\bml\\b|machine learning|deep learning|\\bai\\b|simulation|\\bedna\\b|remote sensing|metabarcoding|\\br package|cran\\b|workflow"),
    list("policy",       "policy|governance|indigenous|equity|stewardship|protected area target"),
    list("conservation", "conservation|biodiversit|extinction|threatened|red list|protected area"),
    list("pollinator",   "pollinator|\\bbee\\b|bumblebee|hoverfly|pollination"),
    list("plants",       "\\bplant|flora|vegetation|grassland|savanna|herbacious"),
    list("animals",      "\\banimal|mammal|bird\\b|amphibian|reptile|insect"),
    list("microbiome",   "microbiome|bacteri|archaea|virus|virom")
  )

  scored <- lapply(rules, function(r) {
    tag <- r[[1]]; re <- r[[2]]
    if (grepl(re, t)) {
      list(tag = tag, score = 1L + pt_boost(re))
    } else NULL
  })
  scored <- Filter(Negate(is.null), scored)
  if (length(scored) == 0) return(character())

  # Sort by score desc, preserve rule order on ties
  scores <- vapply(scored, function(x) x$score, integer(1))
  ord    <- order(-scores, seq_along(scored))
  tags   <- vapply(scored[ord], function(x) x$tag, character(1))
  head(unique(tags), max_tags)
}

# ---- HTML escape helper (for alt text, URLs in attributes) -
html_escape <- function(s) {
  if (is.null(s)) return("")
  s <- as.character(s)
  s <- gsub("&", "&amp;",  s, fixed = TRUE)
  s <- gsub("<", "&lt;",   s, fixed = TRUE)
  s <- gsub(">", "&gt;",   s, fixed = TRUE)
  s <- gsub("'", "&#39;",  s, fixed = TRUE)
  gsub("\"", "&quot;", s, fixed = TRUE)
}

# ---- Bluesky-embed image extractor (no network) ------------
# Returns list(thumb, full) or NULL. Priority:
#   1. user-uploaded photo (app.bsky.embed.images view)
#   2. external link card thumbnail (app.bsky.embed.external view)
extract_post_image <- function(feed_embed) {
  imgs <- safe(feed_embed$images)
  if (!is.null(imgs) && length(imgs) > 0) {
    thumb <- safe(imgs[[1]]$thumb)
    full  <- safe(imgs[[1]]$fullsize)
    if (!is.null(thumb) && is.character(thumb) && nzchar(thumb)) {
      if (is.null(full) || !is.character(full) || !nzchar(full)) full <- thumb
      return(list(thumb = thumb, full = full))
    }
  }
  ext_thumb <- safe(feed_embed$external$thumb)
  if (!is.null(ext_thumb) && is.character(ext_thumb) && nzchar(ext_thumb)) {
    return(list(thumb = ext_thumb, full = ext_thumb))
  }
  NULL
}

# ---- URL meta fetcher (cached, timed out, follows redirects) ----
# Single network call returns BOTH a paper title and an og:image.
# Old caches stored plain strings; we transparently upgrade those
# to list(title=, image=) on read.
#
# Title sources (in order): citation_title, dc.title, og:title,
# twitter:title, <title>. Image sources: og:image, og:image:url,
# twitter:image, twitter:image:src. Junk title patterns and
# blocked domains return list(title=NULL, image=NULL).
get_url_meta <- function(url, cache = list(), timeout_s = 10) {
  empty <- list(title = NULL, image = NULL)
  if (is.null(url) || !nzchar(url)) return(empty)

  if (!is.null(cache[[url]])) {
    cached <- cache[[url]]
    # Backward-compat: legacy entries are character strings (title only).
    if (is.character(cached)) return(list(title = cached, image = NULL))
    return(cached)
  }

  blocked_domains <- c("lnkd.in", "linkedin.com", "x.com", "twitter.com",
                       "facebook.com", "fb.com", "instagram.com",
                       "globalecologybs.github.io")
  if (any(vapply(blocked_domains, function(d) grepl(d, url, fixed = TRUE), logical(1)))) {
    return(empty)
  }

  junk_re <- paste0(
    "^\\s*(just a moment|access denied|page not found|404 not found|404|",
    "loading|redirecting|please verify|attention required|cloudflare|",
    "linkedin|facebook|twitter|x|bluesky|instagram|youtube|vimeo|",
    "researchgate|google|sign in|sign up|welcome|home page?|home|",
    "untitled|untitled document|global ecology digest.*)\\s*$"
  )

  resolve_url <- function(base_url, rel) {
    if (is.null(rel) || !nzchar(rel)) return(NULL)
    if (grepl("^https?://", rel))     return(rel)
    if (startsWith(rel, "//"))        return(paste0("https:", rel))
    base <- regmatches(base_url, regexpr("^https?://[^/]+", base_url))
    if (length(base) == 0)            return(NULL)
    if (startsWith(rel, "/"))         return(paste0(base, rel))
    NULL  # don't bother with relative-relative paths
  }

  do_fetch <- function() {
    req <- curl::new_handle(
      timeout        = timeout_s,
      followlocation = TRUE,
      maxredirs      = 10,
      useragent      = "Mozilla/5.0 (compatible; GlobalEcologyDigestBot/1.0; +https://globalecologybs.github.io/feeddigest.github.io/)"
    )
    resp <- curl::curl_fetch_memory(url, handle = req)
    if (resp$status_code >= 400) return(empty)

    body <- rawToChar(resp$content)
    Encoding(body) <- "UTF-8"
    html <- xml2::read_html(body)

    # --- Title ---
    title_candidates <- c(
      xml2::xml_attr(xml2::xml_find_first(html, "//meta[@name='citation_title']"),                "content"),
      xml2::xml_attr(xml2::xml_find_first(html, "//meta[@name='dc.title' or @name='DC.title']"),  "content"),
      xml2::xml_attr(xml2::xml_find_first(html, "//meta[@property='og:title']"),                  "content"),
      xml2::xml_attr(xml2::xml_find_first(html, "//meta[@name='twitter:title']"),                 "content"),
      xml2::xml_text(xml2::xml_find_first(html, "//title"))
    )
    title_candidates <- title_candidates[!is.na(title_candidates) & nzchar(trimws(title_candidates))]

    title <- NULL
    if (length(title_candidates) > 0) {
      t <- gsub("\\s+", " ", trimws(title_candidates[1]))
      ok <- !grepl(junk_re, t, ignore.case = TRUE) &&
            nchar(t) >= 12 && nchar(t) <= 300 &&
            length(strsplit(t, "\\s+")[[1]]) > 2
      if (ok) title <- t
    }

    # --- Image (og:image / twitter:image) ---
    img_candidates <- c(
      xml2::xml_attr(xml2::xml_find_first(html, "//meta[@property='og:image']"),       "content"),
      xml2::xml_attr(xml2::xml_find_first(html, "//meta[@property='og:image:url']"),   "content"),
      xml2::xml_attr(xml2::xml_find_first(html, "//meta[@name='twitter:image']"),      "content"),
      xml2::xml_attr(xml2::xml_find_first(html, "//meta[@name='twitter:image:src']"),  "content")
    )
    img_candidates <- img_candidates[!is.na(img_candidates) & nzchar(trimws(img_candidates))]

    image <- NULL
    if (length(img_candidates) > 0) {
      image <- resolve_url(url, trimws(img_candidates[1]))
    }

    list(title = title, image = image)
  }

  tryCatch(do_fetch(), error = function(e) empty)
}

# ---- LLM-generated title (fallback when no paper title) ----
# Returns list(title = ..., key = ...) where `key` is the
# cache key (sha1 of the post text). NULL on failure.
#
# Caching strategy: cache by post text hash, so the same post
# never costs more than once even across digest runs.
TITLE_LLM_SYSTEM_PROMPT <- paste0(
  "You write concise, scientific titles for social-media posts about ecology research.\n",
  "\n",
  "ALWAYS return a title. Look hard -- almost every post has a topic, even if it\n",
  "is just announcing a paper or webinar. Return the single word NONE (uppercase)\n",
  "ONLY if the post truly has zero discernible topic (e.g. just emoji, just hashtags,\n",
  "or 'thanks!').\n",
  "\n",
  "Rules:\n",
  "- 5-15 words. Sentence case (capitalize first word + proper nouns only).\n",
  "- Return ONLY the title text. No quotes, no period, no preamble.\n",
  "- Strip emoji, hashtags, and at-mentions from the post before extracting.\n",
  "- Focus on the SCIENCE, not the author or the act of sharing.\n",
  "- If the post text IS already a paper title (e.g. journal accounts often\n",
  "  just post the title), clean it up and return it in sentence case.\n",
  "- Jobs/webinars/calls: title them as 'Webinar: X', 'PhD position in X',\n",
  "  'Call for proposals: X', 'New R package: X', etc.\n",
  "\n",
  "Examples:\n",
  "\n",
  "Post: 'So happy to lead this paper where we used metacommunity simulations to\n",
  "       assess how biological index performance declines across drying and pollution.'\n",
  "Title: Metacommunity simulations reveal how drying and pollution degrade biological indices\n",
  "\n",
  "Post: 'Atmospheric Boundary Layer Control on Forest Thermal Properties \U0001F517'\n",
  "Title: Atmospheric boundary layer control on forest thermal properties\n",
  "\n",
  "Post: 'Thank you Samuel Bickel & Berg Gabriele for highlighting our findings of\n",
  "       @cellhostmicrobe article -Microbial diversity creates a global firewall\n",
  "       against pathogens in soil. lnkd.in/eQiZd5ST'\n",
  "Title: Microbial diversity creates a global firewall against pathogens in soil\n",
  "\n",
  "Post: 'Don't miss our webinar tomorrow! Learn how GuardIAS and OneStop are\n",
  "       helping safeguard Europe against invasive species.'\n",
  "Title: Webinar: GuardIAS and OneStop on safeguarding Europe from invasive species\n",
  "\n",
  "Post: 'My rstats package mfclim is available on CRAN. It provides access to\n",
  "       archived meteorological data from Meteo-France.'\n",
  "Title: New R package mfclim: archived Meteo-France meteorological data on CRAN\n",
  "\n",
  "Post: 'How can globally networked, interdisciplinary research address climate change,\n",
  "       biodiversity loss and resource scarcity?'\n",
  "Title: Networked interdisciplinary research for climate, biodiversity, and resource crises\n",
  "\n",
  "Post: 'PhD position in marine ecology at University X, apply by June 1.'\n",
  "Title: PhD position in marine ecology (deadline 1 June)"
)

generate_title_llm <- function(text, cache = list()) {
  if (is.null(text) || !nzchar(text)) {
    return(list(title = NULL, key = NULL, reason = "empty"))
  }
  if (!requireNamespace("digest", quietly = TRUE)) install.packages("digest")
  if (!requireNamespace("ellmer", quietly = TRUE)) install.packages("ellmer")

  key <- digest::digest(text, algo = "sha1")
  if (!is.null(cache[[key]])) {
    if (identical(cache[[key]], "")) {
      return(list(title = NULL, key = key, reason = "cached_none"))
    }
    return(list(title = cache[[key]], key = key, reason = "cached_hit"))
  }

  # Inner function isolates return() inside its own closure.
  do_call <- function() {
    chat <- ellmer::chat_anthropic(
      model         = CONFIG$llm_model,
      system_prompt = TITLE_LLM_SYSTEM_PROMPT,
      echo          = "none"
    )
    raw   <- chat$chat(text)
    title <- trimws(as.character(raw))
    title <- gsub('^["“”\']+|["“”\']+$', "", title)
    title <- gsub('\\.$', "", title)
    title <- gsub("\\s+", " ", title)

    if (identical(toupper(title), "NONE"))      return(list(ok = FALSE, reason = "none"))
    if (nchar(title) < 8 || nchar(title) > 250) return(list(ok = FALSE, reason = "bad_length"))
    list(ok = TRUE, title = title)
  }

  result <- tryCatch(
    do_call(),
    error = function(e) list(ok = FALSE, reason = paste0("error: ", conditionMessage(e)))
  )

  if (isTRUE(result$ok)) {
    return(list(title = result$title, key = key, reason = "ok"))
  }
  list(title = NULL, key = key, reason = result$reason)
}

# ---- LLM-generated tags (semantic classifier) --------------
# The regex classifier in classify_post() trips on incidental mentions
# (e.g. an author named "Forest"). The LLM understands context and
# produces clean tags from a fixed vocabulary.
TAGS_LLM_VOCAB <- c(
  "marine", "freshwater", "forest", "soil", "climate", "invasives",
  "conservation", "policy", "jobs", "events", "methods",
  "pollinator", "plants", "animals", "microbiome"
)

TAGS_LLM_SYSTEM_PROMPT <- paste0(
  "You assign topic tags to social-media posts about ecology research.\n",
  "Choose ONLY from this exact vocabulary (no other words):\n",
  "  marine, freshwater, forest, soil, climate, invasives,\n",
  "  conservation, policy, jobs, events, methods,\n",
  "  pollinator, plants, animals, microbiome.\n",
  "\n",
  "Tag definitions:\n",
  "- marine: oceans, seas, reefs, fish/fisheries, marine ecology\n",
  "- freshwater: rivers, lakes, streams, wetlands\n",
  "- forest: forests, woodlands, trees AS ECOSYSTEMS (not as names!)\n",
  "- soil: soil ecology, soil microbiome, fungi, mycorrhiza\n",
  "- climate: climate change, warming, carbon cycle, drought, heat\n",
  "- invasives: invasive species, biological invasions, biosecurity\n",
  "- conservation: protected areas, biodiversity conservation, extinction\n",
  "- policy: governance, indigenous rights, environmental policy\n",
  "- jobs: PhD, postdoc, faculty position announcements\n",
  "- events: webinars, conferences, workshops, seminars\n",
  "- methods: ML/AI, eDNA, remote sensing, R packages, models, simulations\n",
  "- pollinator: bees, pollinators, pollination\n",
  "- plants: plant ecology broadly, flora, vegetation, grasslands\n",
  "- animals: animal ecology broadly, mammals/birds/insects/herps\n",
  "- microbiome: microbes, bacteria, viruses (excluding soil-specific)\n",
  "\n",
  "Rules:\n",
  "- Return 1-4 tags as a comma-separated list. No other text.\n",
  "- Tags must reflect the SCIENTIFIC TOPIC of the paper or content,\n",
  "  NOT incidental mentions (a person named 'Forest' is NOT forest).\n",
  "- Prefer specific over generic: 'pollinator' over 'animals' when both fit.\n",
  "- If truly nothing in the vocabulary applies, return the single word: NONE\n",
  "\n",
  "Examples:\n",
  "\n",
  "Input title: 'Predicting temporal stability and resilience from resistance and recovery'\n",
  "Input post: 'Paper published, led by Forest Isbell. We developed a new theoretical\n",
  "framework to predict how temporal stability and resilience emerge from the combined\n",
  "effects of resistance and recovery.'\n",
  "Output: methods, conservation\n",
  "\n",
  "Input title: 'Microbial diversity creates a global firewall against pathogens in soil'\n",
  "Input post: 'Thank you Samuel Bickel & Berg Gabriele for highlighting our findings.'\n",
  "Output: soil, microbiome, conservation\n",
  "\n",
  "Input title: 'Webinar: GuardIAS and OneStop on safeguarding Europe from invasive species'\n",
  "Input post: 'Don't miss our webinar tomorrow!'\n",
  "Output: events, invasives, policy\n",
  "\n",
  "Input title: 'Pollinators support the nutrition and income of vulnerable communities'\n",
  "Input post: 'Research in Nature: 40% of household income tied to insect pollinators.'\n",
  "Output: pollinator, policy, conservation"
)

generate_tags_llm <- function(text, paper_title = NULL,
                              cache = list(),
                              max_tags = CONFIG$max_tags_per_post %||% 4) {
  if (is.null(text) || !nzchar(text)) {
    return(list(tags = NULL, key = NULL, reason = "empty"))
  }
  if (!requireNamespace("digest", quietly = TRUE)) install.packages("digest")
  if (!requireNamespace("ellmer", quietly = TRUE)) install.packages("ellmer")

  # Cache key includes BOTH text and title (title affects answer)
  key <- digest::digest(paste(text, paper_title %||% ""), algo = "sha1")
  if (!is.null(cache[[key]])) {
    cached <- cache[[key]]
    if (length(cached) == 0 || identical(cached, "")) {
      return(list(tags = NULL, key = key, reason = "cached_none"))
    }
    return(list(tags = cached, key = key, reason = "cached_hit"))
  }

  do_call <- function() {
    user_msg <- if (!is.null(paper_title) && nzchar(paper_title)) {
      paste0("Input title: ", paper_title, "\nInput post: ", text)
    } else {
      paste0("Input post: ", text)
    }

    chat <- ellmer::chat_anthropic(
      model         = CONFIG$llm_model,
      system_prompt = TAGS_LLM_SYSTEM_PROMPT,
      echo          = "none"
    )
    raw <- trimws(as.character(chat$chat(user_msg)))

    if (identical(toupper(raw), "NONE")) {
      return(list(ok = FALSE, reason = "none"))
    }
    # Parse: lowercase, split on commas, intersect with vocab
    parts <- tolower(unlist(strsplit(raw, "[,;\\n]+")))
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    tags  <- intersect(parts, TAGS_LLM_VOCAB)

    if (length(tags) == 0) return(list(ok = FALSE, reason = "no_vocab_match"))
    list(ok = TRUE, tags = head(unique(tags), max_tags))
  }

  result <- tryCatch(
    do_call(),
    error = function(e) list(ok = FALSE, reason = paste0("error: ", conditionMessage(e)))
  )

  if (isTRUE(result$ok)) {
    return(list(tags = result$tags, key = key, reason = "ok"))
  }
  list(tags = NULL, key = key, reason = result$reason)
}

# ---- Text cleaning -----------------------------------------
clean_text <- function(text) {
  if (is.null(text) || is.na(text)) return("")
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[!grepl("[.…]{3,}$", words)]
  out <- paste(words, collapse = " ")
  out <- gsub("#", "", out)
  out <- gsub("\\.{3,}", "...", out)
  out <- gsub("https?://[^[:space:]]+\\.{3}", "", out, perl = TRUE)
  out <- gsub("[<>]", "", out)
  out <- gsub("[  ]", " ", out)
  gsub("\n", " ", out)
}

extract_uri <- function(post_record, post_embed) {
  for (j in seq_along(post_record$facets)) {
    u <- safe(post_record$facets[[j]]$features[[1]]$uri)
    if (!is.null(u)) return(u)
  }
  safe(post_embed$external$uri)
}

# ---- Registry: _data/digests.yml ---------------------------
# Single source of truth for the sidebar layout. Each digest
# gets one entry. We upsert by `num` and persist as YAML so
# Jekyll's site.data.digests just works.
update_digests_registry <- function(data_path, entry) {
  if (!requireNamespace("yaml", quietly = TRUE)) install.packages("yaml")

  reg <- if (file.exists(data_path)) {
    safe(yaml::read_yaml(data_path), default = list())
  } else list()
  if (is.null(reg) || !is.list(reg)) reg <- list()

  # Drop any existing entry with the same num
  if (length(reg) > 0) {
    nums <- vapply(reg, function(x) safe(as.integer(x$num), NA_integer_), integer(1))
    reg <- reg[is.na(nums) | nums != entry$num]
  }
  reg <- c(reg, list(entry))

  # Sort newest first
  nums <- vapply(reg, function(x) safe(as.integer(x$num), NA_integer_), integer(1))
  reg  <- reg[order(nums, decreasing = TRUE, na.last = TRUE)]

  dir.create(dirname(data_path), showWarnings = FALSE, recursive = TRUE)
  yaml::write_yaml(reg, data_path)
}

build_registry_entry <- function(X, start_date, end_date, nb_post) {
  list(
    num        = X,
    year       = format(end_date, "%Y"),
    start_date = format(start_date, "%Y-%m-%d"),
    end_date   = format(end_date,   "%Y-%m-%d"),
    nb_post    = nb_post,
    url        = paste0(CONFIG$base_url, "/archives/digest-", X, "/"),
    date_label = paste0(format(start_date, "%b %d"), " - ", format(end_date, "%b %d"))
  )
}

# Build a 160-char meta description suitable for SEO.
build_description <- function(start_date, end_date, nb_post) {
  d <- paste0(
    nb_post, " curated posts from the Bluesky Global Ecology feed (",
    format(start_date, "%b %d"), " - ", format(end_date, "%b %d, %Y"),
    "): biodiversity, ecosystems, conservation -- terrestrial, freshwater & marine."
  )
  if (nchar(d) > 300) paste0(substr(d, 1, 297), "...") else d
}

# ---- Engagement label --------------------------------------
# Avoid the false-negative "💚 0" signal on freshly posted items.
format_engagement <- function(likes) {
  n <- safe(as.integer(likes), 0L)
  if (is.na(n) || n <= 0L) {
    "\U0001F195 just posted on Bluesky"
  } else {
    paste0("\U0001F49A ", n, " like", if (n == 1L) "" else "s", " on Bluesky")
  }
}

# ---- Render a single post ----------------------------------
# Heading priority:
#   1. Paper / link title (if fetched) -- science-first, scannable.
#   2. "Post by <Author>" fallback when no title is available.
format_post <- function(p) {
  author_link <- if (!is.null(p$handle)) {
    paste0("<a href='https://bsky.app/profile/", p$handle, "' target='_blank' rel='noopener'>@", p$handle, "</a>")
  } else "Unknown author"

  has_title <- !is.null(p$paper_title) && nzchar(p$paper_title)

  # Heading. Mark LLM-generated titles for transparency.
  heading <- if (has_title) {
    marker <- if (identical(p$title_source, "llm")) {
      " <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>"
    } else ""
    paste0("##### \U0001F4C4 ", p$paper_title, marker, "\n\n")
  } else {
    paste0("##### Post by ", p$author_name, " ", author_link, "\n\n")
  }

  # Metadata line below the heading.
  meta_author <- if (has_title) {
    paste0("Shared by **", p$author_name, "** ", author_link, " &middot; ")
  } else ""

  meta_line <- paste0(
    "<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>",
    meta_author,
    "<time datetime='", p$post_date, "'>", p$post_date, "</time>",
    " &middot; ", format_engagement(p$likes),
    "</p>\n\n"
  )

  uri_block <- if (!is.null(p$uri)) {
    paste0("<br><b>link:</b> <a href='", p$uri, "' target='_blank' rel='noopener'>", p$uri, "</a><br>")
  } else "<br>"

  tag_block <- if (isTRUE(CONFIG$enable_tags) && length(p$tags) > 0) {
    chips <- paste0(
      "<span class='tag tag-", p$tags, "'>", p$tags, "</span>",
      collapse = ""
    )
    paste0("    <div class='tag-row'>", chips, "</div>\n")
  } else ""

  summary_block <- if (isTRUE(CONFIG$enable_summaries) && !is.null(p$summary)) {
    paste0("  <i>", p$summary, "</i><br>\n")
  } else ""

  # Image column (right side) + lightbox overlay
  image_html <- ""
  lightbox_html <- ""
  if (!is.null(p$image) && !is.null(p$image$thumb)) {
    alt_text <- html_escape(substr(p$text, 1, 80))
    thumb_u  <- html_escape(p$image$thumb)
    full_u   <- html_escape(p$image$full)
    lb_id    <- paste0("lb-", p$id)
    # Inline styles below duplicate the <style> block at the top
    # of the page so the layout works even if the <style> tag is
    # stripped by a strict markdown processor or theme.
    image_html <- paste0(
      "  <div class='post-image' style='flex:0 0 140px;'>\n",
      "    <a href='#", lb_id, "' aria-label='Enlarge image'>\n",
      "      <img src='", thumb_u, "' alt='", alt_text, "' loading='lazy' ",
              "width='140' height='140' ",
              "style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>\n",
      "    </a>\n",
      "  </div>\n"
    )
    lightbox_html <- paste0(
      "<a href='#_' class='lightbox' id='", lb_id, "' aria-label='Close enlarged image'>\n",
      "  <img src='", full_u, "' alt='", alt_text, "'>\n",
      "</a>\n"
    )
  }

  paste0(
    heading,
    meta_line,
    "<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>\n",
    "  <div class='post-text' style='flex:1 1 auto;min-width:0;'>\n",
    tag_block,
    "    {% raw %}", p$text, "{% endraw %}\n",
    summary_block,
    uri_block, "\n",
    "    <br><a href='", p$bluesky_link, "' target='_blank' rel='noopener'>View Original Post on Bluesky</a>\n",
    "  </div>\n",
    image_html,
    "</div>\n",
    lightbox_html,
    "\n---\n\n"
  )
}

# ---- Front matter builders ---------------------------------
yaml_quote <- function(s) {
  s <- gsub('"', '\\\\"', s)
  paste0('"', s, '"')
}

digest_front_matter <- function(X, start_date, end_date, nb_post) {
  permalink   <- paste0("/archives/digest-", X, "/")
  title       <- paste0("Global Ecology Digest #", X,
                        " - ", format(start_date, "%b %d"),
                        " to ", format(end_date,   "%b %d, %Y"))
  description <- build_description(start_date, end_date, nb_post)

  paste0(
    "---\n",
    "layout: default\n",
    "title: ",       yaml_quote(title),       "\n",
    "description: ", yaml_quote(description), "\n",
    "date: ",        format(end_date, "%Y-%m-%d"), "\n",
    "year: ",        yaml_quote(format(end_date, "%Y")), "\n",
    "digest_num: ",  X,                       "\n",
    "image: ",       CONFIG$social_image,     "\n",
    "permalink: ",   permalink,               "\n",
    "sitemap:\n",
    "  changefreq: monthly\n",
    "  priority: 0.6\n",
    "---\n\n"
  )
}

landing_front_matter <- function() {
  paste0(
    "---\n",
    "layout: default\n",
    "title: ",       yaml_quote(paste0(CONFIG$site_title, " - Bluesky biodiversity & conservation science")), "\n",
    "description: ", yaml_quote(paste0(CONFIG$site_tagline, ": biodiversity, ecosystems, conservation. Terrestrial, freshwater & marine realms.")), "\n",
    "image: ",       CONFIG$social_image, "\n",
    "permalink: /\n",
    "sitemap:\n",
    "  changefreq: weekly\n",
    "  priority: 1.0\n",
    "---\n\n"
  )
}

archive_index_front_matter <- function() {
  paste0(
    "---\n",
    "layout: default\n",
    "title: ",       yaml_quote(paste0(CONFIG$site_title, " - Archive")),   "\n",
    "description: ", yaml_quote("All past Global Ecology digests, newest first."), "\n",
    "image: ",       CONFIG$social_image, "\n",
    "permalink: /archives/\n",
    "sitemap:\n",
    "  changefreq: weekly\n",
    "  priority: 0.8\n",
    "---\n\n"
  )
}

# ---- Shared header (body, not <head>) ----------------------
shared_intro_block <- function() {
  paste0(
    "<div style='width:100%; text-align:center; margin-bottom:20px;'>\n",
    "  <img src='", CONFIG$banner_image, "' alt='Global Ecology Banner' style='width:100%; height:auto;'>\n",
    "</div>\n\n",
    "For the lazy (yes we are) and friends who do not like social media (yes they can) but could benefit from the news on the Global Ecology feed ... here is a curated digest of the \U0001F98B bluesky Global Ecology feed \U0001F310 on biodiversity, ecosystems & conservation at large scales. Terrestrial, freshwater & marine realms.\n\n",
    "- **SCIENCE ONLY (publications, data, jobs)**\n",
    "- Not on BlueSky ? email <a href='mailto:global.ecology.bs@gmail.com'> to receive weekly update</a>\n",
    "- On BlueSky ? DM <a href='https://bsky.app/profile/global-ecology.bsky.social' target='_blank' rel='noopener'>@global-ecology.bsky.social</a> to contribute and receive weekly update\n",
    "- Here to <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank' rel='noopener'>like & pin the Global Ecology</a> feed\n",
    "- <a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3lfum2bjpab24' target='_blank' rel='noopener'>Global Ecology starter pack Vol. 1</a>\n",
    "- <a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3ld2m2csaai2x' target='_blank' rel='noopener'>Global Ecology starter pack Vol. 2</a>\n",
    "- <a href='https://go.bsky.app/MkLHiKU' target='_blank' rel='noopener'>Global Ecology starter pack Vol. 3</a>\n",
    "- <a href='https://go.bsky.app/Dsk4TQ3' target='_blank' rel='noopener'>Global Ecology starter pack Vol. 4</a>\n\n"
  )
}

# ---- CSS injected at the top of every digest page ----------
# Self-contained: doesn't depend on _layouts/default.html. This is
# what makes the image column, lightbox and tag chips render correctly
# regardless of which Jekyll theme is active.
digest_inline_css <- function() {
  paste0(
    "<style>\n",
    ".post-row { display: flex; gap: 1rem; align-items: flex-start; margin: 0.5rem 0 1rem 0; }\n",
    ".post-text { flex: 1 1 auto; min-width: 0; }\n",
    ".post-image { flex: 0 0 140px; }\n",
    ".post-image a { display: block; }\n",
    ".post-image img {\n",
    "  width: 140px; height: 140px;\n",
    "  object-fit: cover;\n",
    "  border-radius: 6px;\n",
    "  display: block;\n",
    "  cursor: zoom-in;\n",
    "  background: #f3f3f3;\n",
    "  border: 1px solid #eee;\n",
    "  transition: opacity 0.15s, transform 0.15s;\n",
    "}\n",
    ".post-image a:hover img { opacity: 0.9; transform: scale(1.02); }\n",
    "@media (max-width: 600px) {\n",
    "  .post-row { flex-direction: column; }\n",
    "  .post-image img { width: 120px; height: 120px; }\n",
    "}\n",
    ".lightbox {\n",
    "  display: none;\n",
    "  position: fixed; top: 0; left: 0; right: 0; bottom: 0;\n",
    "  background: rgba(0,0,0,0.92);\n",
    "  z-index: 9999;\n",
    "  padding: 2rem;\n",
    "  cursor: zoom-out;\n",
    "  text-align: center;\n",
    "  text-decoration: none;\n",
    "}\n",
    ".lightbox:target { display: flex; align-items: center; justify-content: center; }\n",
    ".lightbox img {\n",
    "  max-width: 100%; max-height: 100%;\n",
    "  width: auto !important; height: auto !important;\n",
    "  object-fit: contain;\n",
    "  box-shadow: 0 8px 40px rgba(0,0,0,0.5);\n",
    "  border-radius: 4px;\n",
    "  cursor: zoom-out;\n",
    "}\n",
    ".tag-row { margin: 0.3rem 0 0.5rem 0; line-height: 1.9; }\n",
    ".tag {\n",
    "  display: inline-block;\n",
    "  font-size: 0.74rem;\n",
    "  font-weight: 600;\n",
    "  text-transform: lowercase;\n",
    "  padding: 2px 8px;\n",
    "  border-radius: 10px;\n",
    "  margin-right: 5px;\n",
    "  background: #eef;\n",
    "  color: #334;\n",
    "}\n",
    ".tag-marine{background:#e0f0fa;color:#0e4d6b}.tag-freshwater{background:#e3f6fa;color:#0a5667}\n",
    ".tag-forest{background:#e4f3e0;color:#2a5a1f}.tag-soil{background:#efe4d4;color:#5b3d18}\n",
    ".tag-climate{background:#fde8d8;color:#8a3a0d}.tag-invasives{background:#fbe0e0;color:#8c1f1f}\n",
    ".tag-conservation{background:#d8efe2;color:#1f5e3c}.tag-policy{background:#ece1f4;color:#4c2773}\n",
    ".tag-jobs{background:#fff3c4;color:#6e5400}.tag-events{background:#fcdef0;color:#7a1c5a}\n",
    ".tag-methods{background:#e5e7eb;color:#374151}.tag-pollinator{background:#fff0c4;color:#7a5300}\n",
    ".tag-plants{background:#e4f0d8;color:#2e5612}.tag-animals{background:#f0e4d8;color:#5a3812}\n",
    ".tag-microbiome{background:#e4daf2;color:#46248a}\n",
    "</style>\n\n"
  )
}

# ---- Visitor counter (hits.sh) -----------------------------
# IMPORTANT: the identifier below is intentionally IDENTICAL on
# every page (homepage + every digest). hits.sh counts hits per
# identifier, so a single shared identifier makes the badge show
# the SUM of hits across the whole site rather than per-page.
visitor_counter_block <- function() {
  id <- "globalecologybs.github.io/feeddigest.github.io"
  paste0(
    "<div style='text-align:center; margin:1.5rem 0;'>\n",
    "  <a href='https://hits.sh/", id, "/' target='_blank' rel='noopener'>\n",
    "    <img alt='Visitor count' src='https://hits.sh/", id,
            ".svg?style=flat-square&label=visitors&color=2d6cdf&labelColor=555'>\n",
    "  </a>\n",
    "</div>\n\n"
  )
}

# ---- Digest page body --------------------------------------
build_digest_body <- function(X, start_date, end_date, nb_post,
                              all_post_md, prev_num = NULL, next_num = NULL) {
  nav <- character()
  if (!is.null(prev_num)) nav <- c(nav, paste0("<a href='/feeddigest.github.io/archives/digest-", prev_num, "/'>← Digest #", prev_num, "</a>"))
  if (!is.null(next_num)) nav <- c(nav, paste0("<a href='/feeddigest.github.io/archives/digest-", next_num, "/'>Digest #", next_num, " →</a>"))
  nav_block <- if (length(nav) > 0) {
    paste0("<p style='font-size:small;'>", paste(nav, collapse = " &nbsp;|&nbsp; "), "</p>\n\n")
  } else ""

  paste0(
    digest_inline_css(),
    shared_intro_block(),
    "# Digest #", X, "\n\n",
    "Feeds are from **", format(start_date, "%B %d, %Y"),
    "** to **", format(end_date, "%B %d, %Y"),
    "**. Total posts: **", nb_post, "**.\n\n",
    "---\n\n",
    paste0(all_post_md, collapse = ""),
    nav_block,
    "<p style='font-size:small;'><a href='/feeddigest.github.io/archives/'>\U0001F4DA Browse all digests</a></p>\n\n",
    visitor_counter_block(),
    "<div style='text-align:left; font-size:small; color:gray;'>\n",
    "  This page is maintained by <a href='http://nicolasmouquet.free.fr/' target='_blank' rel='noopener' style='color:gray;'>Nicolas Mouquet</a>\n",
    "</div>\n"
  )
}

# ---- Homepage (landing) body -------------------------------
build_landing_body <- function(X, start_date, end_date, nb_post, all_digests) {
  paste0(
    shared_intro_block(),
    "# ", CONFIG$site_title, "\n\n",
    "Curated digest of the \U0001F98B <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank' rel='noopener'>Bluesky Global Ecology feed</a> on biodiversity, ecosystems & conservation at large scales. New issue roughly every two weeks. Browse all past digests in the sidebar.\n\n",
    "---\n\n",
    "## Latest issue: Digest #", X, "\n\n",
    "**", format(start_date, "%B %d, %Y"), " - ", format(end_date, "%B %d, %Y"),
    "** &middot; ", nb_post, " posts curated\n\n",
    "<p><a href='", CONFIG$base_url, "/archives/digest-", X, "/' style='display:inline-block;padding:10px 18px;background:#2d6cdf;color:white;border-radius:6px;text-decoration:none;'>Read Digest #", X, " →</a></p>\n\n",
    "---\n\n",
    visitor_counter_block(),
    "<div style='text-align:left; font-size:small; color:gray;'>\n",
    "  This page is maintained by <a href='http://nicolasmouquet.free.fr/' target='_blank' rel='noopener' style='color:gray;'>Nicolas Mouquet</a>\n",
    "</div>\n"
  )
}

# ---- Archive listing body ----------------------------------
build_archive_body <- function(all_digests) {
  body <- if (nrow(all_digests) == 0) {
    "_No digests yet._\n"
  } else {
    paste0(
      paste0(
        "- [Digest #", all_digests$num, "](/feeddigest.github.io/archives/digest-", all_digests$num, "/)",
        collapse = "\n"
      ),
      "\n"
    )
  }
  paste0(
    "# ", CONFIG$site_title, " - Archive\n\n",
    "All past digests, newest first.\n\n",
    body, "\n",
    "[← Back to home](/feeddigest.github.io/)\n"
  )
}

# ============================================================
# Main
# ============================================================

# ---- Paths -------------------------------------------------
end_date     <- Sys.Date()
start_date   <- end_date - CONFIG$days_back

year_dir     <- here::here("data", format(end_date, "%Y"))
archives_dir <- here::here(CONFIG$archives_dir)
dir.create(year_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(archives_dir, showWarnings = FALSE, recursive = TRUE)

X <- CONFIG$digest_number %||% next_digest_number(archives_dir)

# ---- Fetch feed --------------------------------------------
if (!requireNamespace("bskyr",    quietly = TRUE)) install.packages("bskyr")
if (!requireNamespace("xml2",     quietly = TRUE)) install.packages("xml2")
if (!requireNamespace("curl",     quietly = TRUE)) install.packages("curl")

feed <- bskyr::bs_get_feed(CONFIG$feed_uri, limit = CONFIG$feed_limit)
feed <- feed[!sapply(feed$uri,   is.na),   ]
feed <- feed[!sapply(feed$embed, is.null), ]

posts       <- if (is.list(feed) && "feed" %in% names(feed)) feed$feed else feed
handles_all <- vapply(posts$author, \(a) a$handle, character(1))
texts_all   <- vapply(posts$record, \(r) r$text,   character(1))

cut_hits <- which(
  handles_all == "global-ecology.bsky.social" &
    grepl("Global Ecology feed Digest", texts_all, fixed = TRUE)
)
cut_idx <- if (length(cut_hits) == 0) {
  warning("No previous digest post found; processing all fetched posts.")
  length(handles_all) + 1L
} else {
  max(cut_hits)
}

save(feed, file = file.path(year_dir, paste0("feed_", strftime(end_date, "%V"), ".RData")))

# ---- Caches (fetch + LLM titles + LLM tags) ----------------
title_cache_path     <- file.path(archives_dir, "title_cache.rds")
llm_title_cache_path <- file.path(archives_dir, "llm_title_cache.rds")
llm_tags_cache_path  <- file.path(archives_dir, "llm_tags_cache.rds")
title_cache     <- if (file.exists(title_cache_path))     readRDS(title_cache_path)     else list()
llm_title_cache <- if (file.exists(llm_title_cache_path)) readRDS(llm_title_cache_path) else list()
llm_tags_cache  <- if (file.exists(llm_tags_cache_path))  readRDS(llm_tags_cache_path)  else list()

# ---- Sanity-check LLM availability before the loop ---------
if (isTRUE(CONFIG$enable_llm_titles)) {
  if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
    warning("CONFIG$enable_llm_titles=TRUE but ANTHROPIC_API_KEY is not set. ",
            "Add Sys.setenv(ANTHROPIC_API_KEY=\"sk-ant-...\") to pass.R. ",
            "LLM titles will be skipped this run.")
  } else {
    cat("LLM titles: enabled (model =", CONFIG$llm_model, ")\n")
  }
}

# ---- Loop with per-post error isolation --------------------
all_post_md  <- character()
nb_post      <- 0L
kept_handles <- character()

for (i in seq_len(cut_idx - 1L)) {
  res <- tryCatch({
    handle <- safe(feed$author[[i]]$handle)

    post_date <- safe(
      as.Date(feed$record[[i]]$createdAt, format = "%Y-%m-%dT%H:%M:%OSZ"),
      NA
    )
    if (is.na(post_date) || post_date < start_date || post_date > end_date) {
      return(list(status = "skip_range", handle = handle))
    }

    text <- clean_text(safe(feed$record[[i]]$text, ""))
    if (nchar(text) < CONFIG$min_text_length) {
      return(list(status = "skip_short", handle = handle))
    }

    name  <- safe(feed$author[[i]]$displayName)
    likes <- safe(feed$like_count[[i]], 0); if (is.null(likes)) likes <- 0
    uri   <- extract_uri(feed$record[[i]], feed$embed[[i]])

    bluesky_link <- gsub("at://",               "https://bsky.app/profile/", feed$uri[[i]])
    bluesky_link <- gsub("app.bsky.feed.post/", "post/",                     bluesky_link)

    # Tag classification runs AFTER title resolution (see below)
    # so the paper title can boost relevant categories.
    tags <- character()

    # Image resolution (no LLM, no extra cost):
    #   1. Bluesky-embedded image (user photo or external card thumb)
    #   2. og:image fetched from URL (filled in below if URL is fetched)
    post_image <- extract_post_image(feed$embed[[i]])

    # Title resolution: fetched -> LLM-generated -> NULL
    paper_title  <- NULL
    title_source <- NULL
    title_diag   <- "no-title"

    if (isTRUE(CONFIG$enable_titles) && !is.null(uri)) {
      meta <- get_url_meta(uri, title_cache, CONFIG$fetch_timeout_s)
      if (!is.null(meta$title) || !is.null(meta$image)) {
        title_cache[[uri]] <- meta
      }
      if (!is.null(meta$title)) {
        paper_title  <- meta$title
        title_source <- "fetched"
        title_diag   <- "fetched"
      }
      # Image fallback (tier 2): og:image from the linked article
      if (is.null(post_image) && !is.null(meta$image)) {
        post_image <- list(thumb = meta$image, full = meta$image)
      }
    }

    # Image fallback (tier 3): og:image from the Bluesky post URL
    # itself. Bluesky's card service renders a per-post image showing
    # the post text -- guarantees something visual for every entry.
    if (is.null(post_image) && !is.null(bluesky_link) && nzchar(bluesky_link)) {
      bsky_meta <- get_url_meta(bluesky_link, title_cache, CONFIG$fetch_timeout_s)
      if (!is.null(bsky_meta$image) || !is.null(bsky_meta$title)) {
        title_cache[[bluesky_link]] <- bsky_meta
      }
      if (!is.null(bsky_meta$image)) {
        post_image <- list(thumb = bsky_meta$image, full = bsky_meta$image)
      }
    }
    if (is.null(paper_title) && isTRUE(CONFIG$enable_llm_titles)) {
      gen <- generate_title_llm(text, llm_title_cache)
      if (!is.null(gen$title)) {
        if (!identical(gen$reason, "cached_hit")) {
          llm_title_cache[[gen$key]] <- gen$title
        }
        paper_title  <- gen$title
        title_source <- "llm"
        title_diag   <- paste0("llm (", gen$reason, ")")
      } else {
        # Negative-cache "none" / "bad_length" so we don't pay for retries.
        if (!is.null(gen$key) && gen$reason %in% c("none", "bad_length")) {
          llm_title_cache[[gen$key]] <- ""
        }
        title_diag <- paste0("llm-failed (", gen$reason, ")")
      }
    }
    # Classify tags: LLM first (handles context like "Forest Isbell"),
    # regex fallback if the LLM fails (auth/network error).
    tag_source <- "none"
    if (isTRUE(CONFIG$enable_tags)) {
      if (isTRUE(CONFIG$enable_llm_tags)) {
        tag_res <- generate_tags_llm(text, paper_title, llm_tags_cache)
        if (!is.null(tag_res$tags) && length(tag_res$tags) > 0) {
          if (!identical(tag_res$reason, "cached_hit")) {
            llm_tags_cache[[tag_res$key]] <- tag_res$tags
          }
          tags <- tag_res$tags
          tag_source <- paste0("llm (", tag_res$reason, ")")
        } else {
          # Negative-cache "none" / "no_vocab_match" only (not errors)
          if (!is.null(tag_res$key) && tag_res$reason %in% c("none", "no_vocab_match")) {
            llm_tags_cache[[tag_res$key]] <- character(0)
          }
          # Regex fallback so we don't lose tags on transient LLM failures
          tags <- classify_post(text, paper_title)
          tag_source <- paste0("regex-fallback (llm: ", tag_res$reason, ")")
        }
      } else {
        tags <- classify_post(text, paper_title)
        tag_source <- "regex"
      }
    }

    cat("  title: ", title_diag,
        if (!is.null(paper_title)) paste0(" -> ", substr(paper_title, 1, 70)) else "",
        if (!is.null(post_image))  " [img]" else "",
        if (length(tags) > 0)      paste0(" [", paste(tags, collapse = ","), " via ", tag_source, "]") else "",
        "\n", sep = "")

    # Stable per-post ID for the lightbox anchor.
    if (!requireNamespace("digest", quietly = TRUE)) install.packages("digest")
    post_id <- substr(digest::digest(feed$uri[[i]], algo = "sha1"), 1, 10)

    summary_text <- NULL  # reserved for future use

    md <- format_post(list(
      text         = text,    handle       = handle,
      author_name  = name,    likes        = likes,
      uri          = uri,     bluesky_link = bluesky_link,
      post_date    = post_date,
      tags         = tags,
      paper_title  = paper_title,
      title_source = title_source,
      summary      = summary_text,
      image        = post_image,
      id           = post_id
    ))
    list(status = "ok", handle = handle, md = md)
  }, error = function(e) list(status = "error", handle = safe(feed$author[[i]]$handle, NA),
                              msg = conditionMessage(e)))

  switch(res$status,
    ok = {
      all_post_md  <- c(all_post_md, res$md)
      nb_post      <- nb_post + 1L
      if (!is.null(res$handle)) kept_handles <- c(kept_handles, paste0("@", res$handle))
      cat("i=", i, " ", res$handle, "ok\n")
    },
    skip_range = cat("i=", i, " ", res$handle, "skip (out of range)\n"),
    skip_short = cat("i=", i, " ", res$handle, "skip (too short)\n"),
    error      = cat("i=", i, " ", res$handle, "ERROR:", res$msg, "\n")
  )
}

# ---- Title-source summary ----------------------------------
# Tally how each kept post was titled so you can see at a glance
# whether the LLM is doing its job.
title_counts <- list(fetched = 0L, llm = 0L, none = 0L)
for (md in all_post_md) {
  if (grepl("✨ AI title", md, fixed = TRUE))      title_counts$llm     <- title_counts$llm + 1L
  else if (grepl("^#####\\s*\U0001F4C4", md))           title_counts$fetched <- title_counts$fetched + 1L
  else                                                  title_counts$none    <- title_counts$none + 1L
}
cat("\nTitle resolution:\n")
cat("  fetched from URL : ", title_counts$fetched, "\n", sep = "")
cat("  LLM-generated    : ", title_counts$llm,     "\n", sep = "")
cat("  none (Post by ...): ", title_counts$none,   "\n", sep = "")

if (isTRUE(CONFIG$enable_titles))     saveRDS(title_cache,     title_cache_path)
if (isTRUE(CONFIG$enable_llm_titles)) saveRDS(llm_title_cache, llm_title_cache_path)
if (isTRUE(CONFIG$enable_llm_tags))   saveRDS(llm_tags_cache,  llm_tags_cache_path)

# ---- Write digest archive page -----------------------------
archive_path <- file.path(archives_dir, paste0("digest-", X, ".md"))

# Determine prev/next digest numbers for in-page navigation.
existing_before <- list_digests(archives_dir)
existing_before <- existing_before[existing_before$num != X, ]
prev_num <- if (nrow(existing_before) > 0) max(existing_before$num[existing_before$num < X], na.rm = TRUE) else NA_integer_
next_num <- if (nrow(existing_before) > 0) min(existing_before$num[existing_before$num > X], na.rm = TRUE) else NA_integer_
prev_num <- if (is.finite(prev_num)) prev_num else NULL
next_num <- if (is.finite(next_num)) next_num else NULL

digest_markdown <- paste0(
  digest_front_matter(X, start_date, end_date, nb_post),
  build_digest_body(X, start_date, end_date, nb_post, all_post_md,
                    prev_num = prev_num, next_num = next_num)
)
write_atomic(digest_markdown, archive_path)

# ---- Update _data/digests.yml (drives sidebar) -------------
data_path <- here::here(CONFIG$data_dir, CONFIG$data_filename)
update_digests_registry(
  data_path,
  build_registry_entry(X, start_date, end_date, nb_post)
)

# ---- Update navigation of immediately previous digest ------
# So /archives/digest-(X-1)/ now links forward to /archives/digest-X/.
# (We rewrite only the body's next-link by regenerating with the new
# next_num. Cheap and consistent.)
if (!is.null(prev_num)) {
  prev_file <- file.path(archives_dir, paste0("digest-", prev_num, ".md"))
  if (file.exists(prev_file)) {
    raw <- readLines(prev_file, warn = FALSE)
    # Replace any existing "Digest #X+1 ->" link or append nav.
    # Simplest: leave the file alone (jekyll-seo-tag still works);
    # users can navigate via the archive index. This avoids fragile
    # regex edits on stale files.
  }
}

# ---- Refresh homepage and archive index --------------------
all_digests <- list_digests(archives_dir)

landing_markdown <- paste0(
  landing_front_matter(),
  build_landing_body(X, start_date, end_date, nb_post, all_digests)
)
write_atomic(landing_markdown, here::here("index.md"))

archive_markdown <- paste0(
  archive_index_front_matter(),
  build_archive_body(all_digests)
)
write_atomic(archive_markdown, file.path(archives_dir, "index.md"))

# ---- robots.txt (idempotent) -------------------------------
robots_path <- here::here("robots.txt")
robots_body <- paste0(
  "User-agent: *\n",
  "Allow: /\n\n",
  "Sitemap: ", CONFIG$site_url, CONFIG$base_url, "/sitemap.xml\n"
)
if (!file.exists(robots_path) || !identical(readLines(robots_path, warn = FALSE), strsplit(robots_body, "\n")[[1]])) {
  write_atomic(robots_body, robots_path)
}

# ---- Handles CSV for postdm --------------------------------
kept_handles <- unique(kept_handles)
handles_df   <- data.frame(handles = kept_handles)
handles_path <- file.path(dirname(here::here()), "postdm", "global_digest.csv")
write.csv2(handles_df, handles_path, row.names = FALSE)

save(feed, file = file.path(year_dir, "feed.RData"))

cat("\n--- done ---\n")
cat("Digest #",    X,             "\n")
cat("Homepage :   index.md\n")
cat("Digest   :  ", archive_path,  "\n")
cat("Archive  :   archives/index.md\n")
cat("Registry :  ", data_path,     "\n")
cat("robots.txt:  robots.txt\n")
cat("Handles  :  ", handles_path,  "\n")
cat("nb_post  =", nb_post,         "\n")
