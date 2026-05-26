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
  site_tagline     = "Fortnightly curated digest of the Bluesky Global Ecology feed",
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
  max_tags_per_post  = 6,

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
                          max_tags = CONFIG$max_tags_per_post %||% 5) {
  combined <- paste(text, paper_title %||% "")
  t <- tolower(combined)
  # Title contributes extra weight if present.
  pt <- tolower(paper_title %||% "")
  pt_boost <- function(re) if (nzchar(pt) && grepl(re, pt)) 1L else 0L

  # Each rule -> (tag, score). Order = tiebreak priority.
  rules <- list(
    # Post types
    list("jobs",             "\\bjob\\b|\\bphd\\b|postdoc|fellowship|hiring|position open|vacancy"),
    list("events",           "webinar|seminar|conference|workshop|symposium|field course"),
    list("data",             "dataset|database|data paper|open data|data release|gbif|data descriptor"),
    list("book",             "\\bbook\\b|field guide|atlas\\b|book chapter|book signing"),
    list("opinion",          "opinion|perspective|editorial|commentary|viewpoint"),
    list("preprint",         "preprint|biorxiv|ecoevo|essoar"),
    # Ecosystems
    list("coral-reef",       "coral reef|coral bleach|reef fish|reef ecosystem|scleractinian"),
    list("open-ocean",       "pelagic|oceanic|deep.sea|high seas|open ocean"),
    list("coastal",          "mangrove|estuar|salt marsh|seagrass|intertidal|tidal|coastal wetland|rocky shore"),
    list("rivers-streams",   "\\briver|\\bstream|fluvial|hydrograph|riparian|lotic"),
    list("lakes",            "\\blake\\b|\\bpond\\b|lentic|limnol"),
    list("wetlands",         "wetland|\\bfen\\b|\\bbog\\b|peatland|\\bswamp|\\bmire\\b|marsh"),
    list("tropical-forest",  "tropical forest|rainforest|cloud forest|monsoon forest|subtropical forest"),
    list("temperate-forest", "temperate forest|deciduous forest|mixed forest|mediterranean wood"),
    list("boreal",           "boreal|taiga|coniferous forest|northern forest"),
    list("grassland-savanna","grassland|savanna|steppe|prairie|\\bmeadow\\b"),
    list("shrubland",        "shrubland|heathland|chaparral|fynbos|mediterranean shrub"),
    list("desert-dryland",   "desert|semi.arid|\\bdryland|arid ecosystem"),
    list("alpine-mountain",  "alpine|subalpine|montane|\\bmountain\\b|high.elevation|high.altitude"),
    list("tundra-arctic",    "tundra|arctic|polar|permafrost|subarctic"),
    list("soil",             "\\bsoil\\b|edaphic|soil carbon|soil nutrient"),
    list("urban",            "urban|\\bcity\\b|cities|green infrastructure|human.dominated"),
    list("marine",           "marine|ocean|\\bsea\\b|fisheries|cetacean|plankton"),
    list("freshwater",       "freshwater|aquatic"),
    # Organisms
    list("plants",           "\\bplant|\\bflora\\b|vegetation|vascular plant|angiosperm|gymnosperm"),
    list("birds",            "\\bbird\\b|avian|ornitho|raptor|passerine|waterfowl|seabird"),
    list("fish",             "\\bfish\\b|ichthyo|salmon|tuna|shark|ray\\b|teleost"),
    list("fungi",            "\\bfung|mycolog|ectomycorrh|\\blichen|macrofungi"),
    list("microbiome",       "microbiome|bacteri|archaea|\\bvirus\\b|virom|microbial"),
    list("pollinator",       "pollinator|\\bbee\\b|bumblebee|hoverfly|pollination|butterfly"),
    list("animals",          "\\banimal|mammal|amphibian|reptile|invertebrate|arthropod|insect"),
    # Science topics
    list("climate",          "climate change|global warming|drought|heatwave|\\bco2\\b|greenhouse|carbon sink"),
    list("invasives",        "invasive|alien species|non.indigenous|biofouling|biological invasion"),
    list("conservation",     "conservation|extinction|threatened|red list|protected area"),
    list("evolution",        "evolutio|phylogen|speciati|\\bcoevolut|paleontol|\\bluca\\b"),
    list("macroecology",     "macroecol|biogeograph|species distribution|range shift|diversity gradient|species.area"),
    list("networks",         "\\bnetwork|food web|interaction network|bipartite|\\bmetaweb|trophic web"),
    list("traits",           "\\btrait\\b|functional divers|functional ecol|leaf economics|life.history trait|csr strateg"),
    list("policy",           "\\bpolicy|governance|indigenous right|stewardship|protected area target"),
    list("ecosystem-services","ecosystem service|nature.s contribution|natural capital|nature.based solution"),
    list("genetics",         "population genetic|landscape genetic|conservation genomic|phylogeograph|\\bedna\\b|molecular ecol"),
    list("movement",         "dispersal|migration\\b|connectivity|movement ecol|home range|\\bcorridor"),
    list("disease",          "\\bdisease|parasite|pathogen|epidemiol|zoonos|wildlife health"),
    list("biogeochemistry",  "nutrient cycling|carbon cycle|nitrogen cycle|phosphorus|stoichiom|decomposit"),
    # Approach
    list("methods",          "\\br package|\\bpython package|cran\\b|new method|new protocol|statistical framework|metabarcoding"),
    list("modelling",        "\\bmodel\\b|simulation|theoretical framework|mathematical model|\\bsdm\\b|species distribution model"),
    list("remote-sensing",   "remote sensing|satellite|\\blidar|\\bdrone\\b|earth observation|\\bgis\\b"),
    list("citizen-science",  "citizen science|community science|inaturalist|public participation"),
    list("synthesis",        "meta.analysis|systematic review|\\breview\\b|perspective|opinion|commentary")
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
  # Ecosystems (18)
  "marine", "coral-reef", "open-ocean", "coastal",
  "freshwater", "rivers-streams", "lakes", "wetlands",
  "tropical-forest", "temperate-forest", "boreal",
  "grassland-savanna", "shrubland", "desert-dryland",
  "alpine-mountain", "tundra-arctic", "soil", "urban",
  # Organisms (7)
  "plants", "animals", "birds", "fish", "fungi", "microbiome", "pollinator",
  # Science topics (13)
  "climate", "invasives", "conservation", "evolution",
  "macroecology", "networks", "traits", "policy",
  "ecosystem-services", "genetics", "movement", "disease", "biogeochemistry",
  # Approach / discipline (5)
  "methods", "modelling", "remote-sensing", "citizen-science", "synthesis",
  # Post types (6)
  "jobs", "events", "data", "book", "opinion", "preprint"
)

TAGS_LLM_SYSTEM_PROMPT <- paste0(
  "You assign topic tags to social-media posts about ecology and biodiversity research.\n",
  "Tags are organised in six dimensions. Choose ONLY from the exact vocabulary below.\n",
  "\n",
  "--- ECOSYSTEMS (pick the most specific that applies) ---\n",
  "  marine         : general marine ecology when no finer tag fits\n",
  "  coral-reef     : coral reef ecosystems, bleaching, reef fish communities\n",
  "  open-ocean     : pelagic, oceanic, deep sea, high seas\n",
  "  coastal        : mangroves, estuaries, salt marshes, seagrass beds, intertidal, shores\n",
  "  freshwater     : general freshwater ecology when no finer tag fits\n",
  "  rivers-streams : rivers, streams, fluvial systems, hydrological connectivity\n",
  "  lakes          : lakes, ponds, lentic systems, limnology\n",
  "  wetlands       : marshes, fens, bogs, peatlands, swamps, mires\n",
  "  tropical-forest: tropical and subtropical rainforest, cloud forest, monsoon forest\n",
  "  temperate-forest: temperate deciduous and mixed forest, Mediterranean woodland\n",
  "  boreal         : boreal forest, taiga, northern coniferous forest\n",
  "  grassland-savanna: tropical savanna, temperate grassland, steppe, prairie, meadow\n",
  "  shrubland      : Mediterranean shrubland, heathland, chaparral, fynbos\n",
  "  desert-dryland : desert, semi-arid, arid, dryland ecosystems\n",
  "  alpine-mountain: alpine, subalpine, montane, high-elevation ecosystems\n",
  "  tundra-arctic  : arctic tundra, polar ecosystems, permafrost\n",
  "  soil           : soil ecology, soil carbon and nutrients, edaphic processes\n",
  "  urban          : urban ecology, cities, green infrastructure, human-dominated landscapes\n",
  "\n",
  "--- ORGANISMS (tag the focal taxon/taxa if central to the paper) ---\n",
  "  plants         : plant ecology broadly, flora, vegetation, vascular plants\n",
  "  animals        : animal ecology broadly when no finer taxon tag fits\n",
  "  birds          : avian ecology, ornithology, bird migration and diversity\n",
  "  fish           : fish ecology, ichthyology, marine and freshwater fish\n",
  "  fungi          : mycology, ectomycorrhizal fungi, macrofungi, lichens\n",
  "  microbiome     : microbes, bacteria, archaea, viruses in ecological contexts\n",
  "  pollinator     : bees, butterflies, hoverflies, pollination ecology\n",
  "\n",
  "--- SCIENCE TOPICS (cross-cutting themes and processes) ---\n",
  "  climate        : climate change, warming, carbon cycle, drought, heat extremes\n",
  "  invasives      : invasive species, biological invasions, biosecurity, alien species\n",
  "  conservation   : biodiversity conservation, extinction risk, protected areas, red list\n",
  "  evolution      : evolutionary ecology, phylogenetics, speciation, adaptation, coevolution\n",
  "  macroecology   : macroecology, biogeography, large-scale biodiversity patterns,\n",
  "                   species distributions, range shifts, diversity gradients\n",
  "  networks       : ecological networks, food webs, bipartite networks, metawebs,\n",
  "                   trophic ecology, species interaction webs\n",
  "  traits         : functional traits, trait-based ecology, functional diversity,\n",
  "                   life-history traits, CSR strategies, leaf economics\n",
  "  policy         : environmental governance, policy, indigenous rights, stewardship\n",
  "  ecosystem-services: ecosystem services, nature's contributions to people (NCP),\n",
  "                      nature-based solutions, natural capital\n",
  "  genetics       : population genetics, landscape genetics, conservation genomics,\n",
  "                   phylogeography, eDNA, molecular ecology\n",
  "  movement       : dispersal, migration, connectivity, movement ecology, home range\n",
  "  disease        : wildlife disease, parasites, pathogens, epidemiology, zoonoses\n",
  "  biogeochemistry: nutrient cycling, carbon/nitrogen/phosphorus cycles,\n",
  "                   elemental stoichiometry, decomposition\n",
  "\n",
  "--- APPROACH / DISCIPLINE (tag only when the approach is a core contribution) ---\n",
  "  methods        : new statistical, lab or field methods, R/Python packages, protocols\n",
  "  modelling      : theoretical models, mathematical frameworks, simulations, SDMs\n",
  "  remote-sensing : satellite imagery, drones, LiDAR, earth observation, GIS analysis\n",
  "  citizen-science: community/citizen science, iNaturalist, public participation data\n",
  "  synthesis      : systematic review, meta-analysis, perspective, opinion, commentary\n",
  "\n",
  "--- POST TYPES (tag the format of the post/content, not the topic) ---\n",
  "  jobs           : PhD, postdoc, faculty or research position announcements\n",
  "  events         : webinars, conferences, workshops, seminars, field courses\n",
  "  data           : dataset releases, databases, open data papers\n",
  "  book           : books, book chapters, field guides, atlases, book signings\n",
  "  opinion        : opinion pieces, perspectives, editorials, commentaries\n",
  "  preprint       : preprints, bioRxiv, EcoEvoRxiv, ESSOAr\n",
  "\n",
  "RULES:\n",
  "- Return 1-6 tags as a comma-separated list. No other text.\n",
  "- Use ALL dimensions that genuinely apply; leave dimensions empty if not relevant.\n",
  "- A broad theoretical paper with no focal ecosystem or taxon may get only 2-3 tags.\n",
  "  A detailed field study will often justify 5-6.\n",
  "- Be specific: prefer 'coral-reef' over 'marine' when the study is on reefs.\n",
  "  Use 'marine' only when no finer ecosystem tag fits.\n",
  "- 'methods', 'modelling', 'remote-sensing': only when the approach is a core\n",
  "  contribution, not just a tool used in a standard analysis.\n",
  "- 'synthesis': for reviews, meta-analyses, perspectives and opinion pieces.\n",
  "- Tags must reflect SCIENTIFIC CONTENT, not incidental mentions\n",
  "  (an author named Forest Isbell does NOT trigger 'temperate-forest').\n",
  "- If nothing applies, return: NONE\n",
  "\n",
  "EXAMPLES:\n",
  "\n",
  "Input title: 'Metawebs as an ecological modeling framework in macroecology and biogeography'\n",
  "Input post: 'Our new study synthesizes the role of metawebs as an ecological modeling\n",
  "framework in macroecology and biogeography, outlining applications and future directions.'\n",
  "Output: networks, macroecology, modelling, synthesis\n",
  "\n",
  "Input title: 'Plant strategies across European grasslands under ongoing climate change'\n",
  "Input post: 'New study maps the distribution of plant strategies across European\n",
  "grasslands and projects how patterns will be affected by climate change.'\n",
  "Output: grassland-savanna, plants, traits, climate, macroecology\n",
  "\n",
  "Input title: 'Greater tree diversity lowers soil carbon temperature sensitivity\n",
  "via microbial stabilization'\n",
  "Input post: 'Tree diversity lowers soil carbon Q10 by enhancing carbon stabilization\n",
  "and shifting microbial strategies. Diverse forests help lock away carbon.'\n",
  "Output: temperate-forest, soil, microbiome, climate, biogeochemistry, traits\n",
  "\n",
  "Input title: 'Symbiotic bacteria may support calcium carbonate precipitation\n",
  "in the Gulf toadfish'\n",
  "Input post: 'What role do fish play in the oceanic carbon cycle via CaCO3 precipitation?\n",
  "Study of the toadfish gut reveals Vibrio bacteria aiding biomineralization.'\n",
  "Output: marine, fish, microbiome, biogeochemistry, climate\n",
  "\n",
  "Input title: 'Predicting temporal stability and resilience from resistance and recovery'\n",
  "Input post: 'Paper published, led by Forest Isbell. We developed a new theoretical\n",
  "framework to predict how temporal stability and resilience emerge from the combined\n",
  "effects of resistance and recovery.'\n",
  "Output: modelling, conservation\n",
  "\n",
  "Input title: 'PhD position: vegetation mapping and herbivory in Greenland'\n",
  "Input post: 'Greenland Institute seeks a postdoc in vegetation mapping and herbivory.\n",
  "Drones, traits and Greenland.'\n",
  "Output: jobs, tundra-arctic, plants, traits\n",
  "\n",
  "Input title: 'Webinar: GuardIAS and OneStop on safeguarding Europe from invasive species'\n",
  "Input post: 'Do not miss our webinar tomorrow!'\n",
  "Output: events, invasives, policy\n",
  "\n",
  "Input title: 'A global taxon-stratified GBIF sampling-effort dataset for SDMs'\n",
  "Input post: 'New paper in Diversity and Distributions: a global, taxon-stratified,\n",
  "high-resolution sampling-effort dataset from GBIF for bias-aware ecological modelling.'\n",
  "Output: data, macroecology, modelling\n",
  "\n",
  "Input title: 'Pollinators support the nutrition and income of vulnerable communities'\n",
  "Input post: 'Research in Nature: 40% of household income tied to insect pollinators.'\n",
  "Output: pollinator, ecosystem-services, policy, conservation"
)

generate_tags_llm <- function(text, paper_title = NULL,
                              cache = list(),
                              max_tags = CONFIG$max_tags_per_post %||% 5) {
  if (is.null(text) || !nzchar(text)) {
    return(list(tags = NULL, key = NULL, reason = "empty"))
  }
  if (!requireNamespace("digest", quietly = TRUE)) install.packages("digest")
  if (!requireNamespace("ellmer", quietly = TRUE)) install.packages("ellmer")

  # Cache key includes text, title, AND vocabulary so any vocab change forces fresh LLM calls
  key <- digest::digest(paste(text, paper_title %||% "", paste(sort(TAGS_LLM_VOCAB), collapse = ",")), algo = "sha1")
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

# ---- LLM-generated digest wrap-up --------------------------
# Generates a structured three-paragraph academic summary of all posts,
# with markdown links anchored to each post (#post-N).
# A fixed fourth paragraph (thank-you) is appended by R, not the LLM.
#
# Structure:
#   Para 1 — dominant science themes (ecosystems, fields, taxa, etc.)
#   Para 2 — methodological, data and modelling contributions
#   Para 3 — jobs, events, news and other non-research posts
#   Para 4 — fixed closing sentence (appended in R, not by LLM)
#
# Before calling the LLM the function prints a numbered table and
# asks which posts to exclude from the summary text (they still
# appear in the digest itself).

WRAPUP_LLM_SYSTEM_PROMPT <- paste0(
  "You write the opening summary for a fortnightly digest of the Bluesky Global Ecology feed,\n",
  "read by an academic audience with strong expertise in ecology, biodiversity and environmental\n",
  "science. Readers are researchers, practitioners and advanced students who follow primary\n",
  "literature closely. Write accordingly: be specific, substantive and precise. Do not explain\n",
  "basic concepts. Name taxa, methods, ecosystems and geographic contexts where they add meaning.\n",
  "Highlight what is genuinely novel or significant in each contribution.\n",
  "\n",
  "Tone: authoritative and collegial, like a Research Highlights note in a leading ecology\n",
  "journal. No hype, no journalistic hooks, no filler. Every clause must carry information.\n",
  "Avoid vague qualifiers ('interesting', 'important', 'fascinating', 'novel approach').\n",
  "Prefer concrete claims: taxa, biomes, methods, findings, geographic scope.\n",
  "\n",
  "You must write EXACTLY THREE paragraphs, separated by a blank line, in this fixed order:\n",
  "\n",
  "PARAGRAPH 1 — Scientific themes.\n",
  "Start this paragraph with exactly the words 'In this digest,' followed by ONE general\n",
  "sentence that captures what is distinctive or prominent about this particular fortnight's\n",
  "content — something specific to these posts, not a generic statement about the field.\n",
  "Do NOT list posts or use hyperlinks in this opening sentence.\n",
  "Do NOT state the obvious (readers know this is a global ecology digest).\n",
  "Do NOT use hollow phrases like 'a wide range of topics', 'macroecological patterns',\n",
  "'contributions span multiple ecosystems', or similar. Instead, name what is genuinely\n",
  "prominent or striking in this issue: a recurring question, a convergence of themes,\n",
  "an unusual breadth or depth, a timely topic. One concrete, specific sentence.\n",
  "Then continue: identify genuine thematic clusters from the post content (not just tags),\n",
  "grouping posts by scientific affinity — shared ecosystem, taxon, process, or question.\n",
  "Within each cluster, synthesise the contributions in flowing prose: question, system or\n",
  "taxon, key finding or advance. Move fluidly from cluster to cluster.\n",
  "\n",
  "PARAGRAPH 2 — Methods, data and modelling.\n",
  "Cover posts whose primary contribution is analytical: new R packages, statistical frameworks,\n",
  "remote-sensing workflows, open datasets, citizen-science tools, synthesis platforms.\n",
  "Be specific about what each tool does and for whom it is useful.\n",
  "If no such posts exist, write a single sentence saying so.\n",
  "\n",
  "PARAGRAPH 3 — Community, jobs and events.\n",
  "Briefly cover positions, seminars, workshops, webinars, book events, or other\n",
  "non-primary-research content. One sentence per item is sufficient.\n",
  "If no such posts exist, write a single sentence saying so.\n",
  "\n",
  "Rules applying to all three paragraphs:\n",
  "- Every post must be referenced at least once as a markdown hyperlink.\n",
  "- The link anchor is always #post-N (e.g. #post-3, #post-12).\n",
  "- The link TEXT must be a meaningful expression drawn from the post content:\n",
  "  a taxon, process, method, finding, or concept. Examples:\n",
  "  [Posidonia oceanica mass flowering](#post-2), [hespdiv R package](#post-3),\n",
  "  [CESABINAR on tropical tree coexistence](#post-18).\n",
  "- NEVER use 'Post N', '#post-N', a bare number, or 'post' as the link text.\n",
  "- NEVER write bare anchors like (#post-N) outside of a markdown link.\n",
  "- NEVER mention journal names, publisher names, or venue names (e.g. do not write\n",
  "  'published in Nature', 'in Ecography', 'Frontiers in Marine Science', etc.).\n",
  "- NEVER use the em dash. Use commas or short sentences instead.\n",
  "- NEVER use --.\n",
  "- Never use 'this week': the digest covers two weeks.\n",
  "- No bullet points, no sub-headers, no line breaks within a paragraph.\n",
  "- {WORD_LIMIT_RULE}\n",
  "- Do NOT write a fourth paragraph. Do NOT add any closing sentence.\n",
  "- End the third paragraph with a period.\n"
)

# Fixed fourth paragraph appended by R (never written by the LLM)
WRAPUP_CLOSING <- paste0(
  "Many thanks to all who contribute to the Global Ecology feed ",
  "by sharing their science on Bluesky."
)

# ---- LLM wrap-up generator ---------------------------------
generate_wrapup_llm <- function(post_meta) {
  if (length(post_meta) == 0) return(NULL)
  if (!requireNamespace("ellmer", quietly = TRUE)) install.packages("ellmer")

  n_posts <- length(post_meta)
  cat("Generating wrap-up for", n_posts, "posts...\n")

  word_limit <- if (n_posts < 20L) 150L else if (n_posts < 35L) 200L else if (n_posts < 50L) 300L else 350L
  word_limit_rule <- paste0(
    "The three paragraphs combined must not exceed ", word_limit, " words in total."
  )
  system_prompt <- gsub("{WORD_LIMIT_RULE}", word_limit_rule, WRAPUP_LLM_SYSTEM_PROMPT, fixed = TRUE)

  # Build a structured brief for each post: title, tags, author, and a
  # short excerpt of the actual post text to give the LLM real content.
  lines <- vapply(post_meta, function(p) {
    title_str  <- if (!is.null(p$title)  && nzchar(p$title))       p$title       else "(no title)"
    author_str <- if (!is.null(p$author_name) && nzchar(p$author_name)) p$author_name else
                  if (!is.null(p$handle) && nzchar(p$handle))       paste0("@", p$handle) else ""
    tag_str    <- if (length(p$tags) > 0) paste(p$tags, collapse = ", ") else "untagged"
    # Trim post text to ~220 chars to give context without bloating the prompt
    txt <- if (!is.null(p$text) && nzchar(p$text)) {
      t <- gsub("\\s+", " ", trimws(p$text))
      if (nchar(t) > 220) paste0(substr(t, 1, 217), "...") else t
    } else ""
    paste0(
      "POST ", p$num, "\n",
      "  Title  : ", title_str, "\n",
      "  Author : ", author_str, "\n",
      "  Tags   : ", tag_str, "\n",
      if (nzchar(txt)) paste0("  Text   : ", txt, "\n") else ""
    )
  }, character(1))

  user_msg <- paste0(
    "Below are the ", n_posts, " posts in this digest. Each entry gives the paper title,\n",
    "author, thematic tags, and an excerpt of the Bluesky post text.\n\n",
    paste(lines, collapse = "\n"),
    "\nWrite the three-paragraph academic summary now."
  )

  clean_raw <- function(x) {
    x <- gsub("—", ",", x, fixed = TRUE)
    x <- gsub("–", ",", x, fixed = TRUE)
    x <- gsub("--",     ",", x, fixed = TRUE)
    trimws(x)
  }

  find_missing <- function(text, all_nums) {
    found <- suppressWarnings(
      as.integer(unique(regmatches(text, gregexpr("(?<=#post-)\\d+", text, perl = TRUE))[[1]]))
    )
    sort(setdiff(all_nums, found[!is.na(found)]))
  }

  all_nums <- vapply(post_meta, `[[`, integer(1), "num")

  tryCatch({
    chat <- ellmer::chat_anthropic(
      model         = CONFIG$llm_model,
      system_prompt = system_prompt,
      echo          = "none"
    )

    # ---- First pass -------------------------------------------
    raw <- clean_raw(as.character(chat$chat(user_msg)))
    if (nchar(raw) < 20) return(NULL)

    # ---- Verification + correction loop (max 2 attempts) ------
    for (attempt in 1:2) {
      missing <- find_missing(raw, all_nums)
      if (length(missing) == 0) break

      cat("Wrap-up missing", length(missing), "post(s):",
          paste(missing, collapse = ", "), "— asking LLM to fix...\n")

      missing_lines <- lines[vapply(post_meta,
                                    function(p) p$num %in% missing, logical(1))]
      fix_msg <- paste0(
        "Your summary is missing a link to the following post(s):\n\n",
        paste(missing_lines, collapse = "\n"),
        "\nFor each missing post, insert a markdown hyperlink whose text is a meaningful\n",
        "expression from the post content (taxon, method, finding) and whose anchor is #post-N.\n",
        "Keep the three-paragraph structure. Do not add a fourth paragraph.\n",
        "Return the complete revised summary."
      )
      raw <- clean_raw(as.character(chat$chat(fix_msg)))
    }

    # Final report
    still_missing <- find_missing(raw, all_nums)
    if (length(still_missing) > 0) {
      warning("Wrap-up still missing post(s) after correction: ",
              paste(still_missing, collapse = ", "))
    } else {
      cat("Wrap-up ok — all", n_posts, "posts referenced.\n")
    }

    paste0(raw, "\n\n", WRAPUP_CLOSING)
  }, error = function(e) {
    warning("Wrap-up generation failed: ", conditionMessage(e))
    NULL
  })
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

# Read the registry back as a list of entries, newest first.
# Carries the full metadata (dates, post counts) that the
# filename-only list_digests() cannot provide.
read_digests_registry <- function(data_path) {
  if (!requireNamespace("yaml", quietly = TRUE)) install.packages("yaml")
  if (!file.exists(data_path)) return(list())
  reg <- safe(yaml::read_yaml(data_path), default = list())
  if (is.null(reg) || !is.list(reg) || length(reg) == 0) return(list())
  nums <- vapply(reg, function(x) safe(as.integer(x$num), NA_integer_), integer(1))
  reg[order(nums, decreasing = TRUE, na.last = TRUE)]
}

# Human-readable date range for one registry entry.
format_digest_dates <- function(entry, with_year = TRUE) {
  sd <- safe(as.Date(as.character(entry$start_date)), NA)
  ed <- safe(as.Date(as.character(entry$end_date)),   NA)
  if (is.na(sd) || is.na(ed)) return(as.character(entry$date_label %||% ""))
  if (with_year) {
    paste0(format(sd, "%b %d"), " to ", format(ed, "%b %d, %Y"))
  } else {
    paste0(format(sd, "%b %d"), " to ", format(ed, "%b %d"))
  }
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

  # Per-post anchor for in-page navigation from the wrap-up paragraph.
  anchor_block <- if (!is.null(p$post_num)) {
    paste0("<div id='post-", p$post_num, "'></div>\n\n")
  } else ""

  # Heading. Mark LLM-generated titles for transparency.
  heading <- if (has_title) {
    marker <- if (identical(p$title_source, "llm")) {
      " <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>"
    } else ""
    paste0(anchor_block, "##### \U0001F4C4 ", p$paper_title, marker, "\n\n")
  } else {
    paste0(anchor_block, "##### Post by ", p$author_name, " ", author_link, "\n\n")
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
    alt_text  <- html_escape(substr(p$text, 1, 80))
    thumb_u   <- html_escape(p$image$thumb)
    full_u    <- html_escape(p$image$full)
    lb_id     <- paste0("lb-", p$id)
    is_avatar <- isTRUE(p$image$is_avatar)
    # Avatars render as a circle (clearly "the author"), real
    # illustrations as a rounded square.
    radius    <- if (is_avatar) "50%" else "6px"
    caption   <- if (is_avatar) {
      "    <div style='text-align:center;font-size:0.68rem;color:#999;margin-top:3px;'>author</div>\n"
    } else ""
    # Inline styles below duplicate the <style> block at the top
    # of the page so the layout works even if the <style> tag is
    # stripped by a strict markdown processor or theme.
    image_html <- paste0(
      "  <div class='post-image' style='flex:0 0 140px;'>\n",
      "    <a href='#", lb_id, "' aria-label='Enlarge image'>\n",
      "      <img src='", thumb_u, "' alt='", alt_text, "' loading='lazy' ",
              "width='140' height='140' ",
              "style='width:140px;height:140px;object-fit:cover;border-radius:", radius,
              ";display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>\n",
      "    </a>\n",
      caption,
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
    "    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'>",
    "<a href='", p$bluesky_link, "' target='_blank' rel='noopener'>View Original Post on Bluesky</a>",
    "<a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a>",
    "</span>\n",
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

# ---- Shared body blocks ------------------------------------
# Banner image only (sits at the very top of every page).
banner_block <- function() {
  paste0(
    "<div style='width:100%; text-align:center; margin-bottom:20px;'>\n",
    "  <img src='", CONFIG$banner_image, "' alt='Global Ecology Banner' style='width:100%; height:auto;'>\n",
    "</div>\n\n"
  )
}

# ============================================================
# "Global Ecology ecosystem" content.
# TWO independent versions so the homepage and the digest pages
# can carry DIFFERENT text. Edit whichever one you need:
#   * ecosystem_block_home()   -> homepage "Global Ecology ecosystem" section
#   * ecosystem_block_digest() -> top of every digest page
# They start identical; change them freely and independently.
# ============================================================

# --- HOMEPAGE version -- edit this for the front page -------
ecosystem_block_home <- function() {
  paste0(
    "<p style='font-size:0.95rem;color:#444;'>",
    "Science-only curated digest (publications, data, jobs) from the \U0001F98B ",
    "<a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank' rel='noopener'>Bluesky Global Ecology feed</a> \U0001F310. ",
    "Not on BlueSky? <a href='mailto:global.ecology.bs@gmail.com'>Email us</a> to receive updates. ",
    "On BlueSky? DM <a href='https://bsky.app/profile/global-ecology.bsky.social' target='_blank' rel='noopener'>@global-ecology.bsky.social</a> to contribute. ",
    "<a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank' rel='noopener'>Like &amp; pin the feed</a>. ",
    "Starter packs: ",
    "<a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3lfum2bjpab24' target='_blank' rel='noopener'>Vol. 1</a>, ",
    "<a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3ld2m2csaai2x' target='_blank' rel='noopener'>Vol. 2</a>, ",
    "<a href='https://go.bsky.app/MkLHiKU' target='_blank' rel='noopener'>Vol. 3</a>, ",
    "<a href='https://go.bsky.app/Dsk4TQ3' target='_blank' rel='noopener'>Vol. 4</a>.",
    "</p>\n\n"
  )
}

# --- DIGEST-PAGE version -- edit this for the digest pages --
ecosystem_block_digest <- function() {
  paste0(
    "<p style='font-size:0.95rem;color:#444;'>",
    "Science-only curated digest (publications, data, jobs) from the \U0001F98B ",
    "<a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank' rel='noopener'>Bluesky Global Ecology feed</a> \U0001F310. ",
    "Not on BlueSky? <a href='mailto:global.ecology.bs@gmail.com'>Email us</a> to receive updates. ",
    "On BlueSky? DM <a href='https://bsky.app/profile/global-ecology.bsky.social' target='_blank' rel='noopener'>@global-ecology.bsky.social</a> to contribute. ",
    "<a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank' rel='noopener'>Like &amp; pin the feed</a>. ",
    "Starter packs: ",
    "<a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3lfum2bjpab24' target='_blank' rel='noopener'>Vol. 1</a>, ",
    "<a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3ld2m2csaai2x' target='_blank' rel='noopener'>Vol. 2</a>, ",
    "<a href='https://go.bsky.app/MkLHiKU' target='_blank' rel='noopener'>Vol. 3</a>, ",
    "<a href='https://go.bsky.app/Dsk4TQ3' target='_blank' rel='noopener'>Vol. 4</a>.",
    "</p>\n\n"
  )
}


# ---- CSS injected at the top of every digest page ----------
# Self-contained: doesn't depend on _layouts/default.html. This is
# what makes the image column, lightbox and tag chips render correctly
# regardless of which Jekyll theme is active.
digest_inline_css <- function() {
  paste0(
    "<style>\n",
    "html, body { overflow-x: hidden; max-width: 100%; }\n",
    ".post-row { display: flex; gap: 1rem; align-items: flex-start; margin: 0.5rem 0 1rem 0; max-width: 100%; }\n",
    ".post-text { flex: 1 1 auto; min-width: 0; word-break: break-word; overflow-wrap: break-word; }\n",
    ".post-image { flex: 0 0 140px; max-width: 140px; }\n",
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
    "@media (max-width: 700px) {\n",
    "  .post-row { flex-direction: column; }\n",
    "  .post-image { max-width: 100%; }\n",
    "  .post-image img { width: 120px; height: 120px; max-width: 100%; }\n",
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
    "/* -- Ecosystems: marine blues -- */\n",
    ".tag-marine{background:#ddeef8;color:#0e4d6b}.tag-coral-reef{background:#fde8d8;color:#7a2a0a}\n",
    ".tag-open-ocean{background:#d0e4f5;color:#0a3358}.tag-coastal{background:#d8f0ea;color:#0a4a38}\n",
    "/* -- Ecosystems: freshwater teals -- */\n",
    ".tag-freshwater{background:#e3f6fa;color:#0a5667}.tag-rivers-streams{background:#d8eeec;color:#0a4450}\n",
    ".tag-lakes{background:#dce8f5;color:#1a3a5a}.tag-wetlands{background:#d8eedd;color:#1a4a2a}\n",
    "/* -- Ecosystems: terrestrial forests -- */\n",
    ".tag-tropical-forest{background:#d4edcc;color:#1a4a12}.tag-temperate-forest{background:#dceedd;color:#2a4a1a}\n",
    ".tag-boreal{background:#dde8df;color:#2a4a2a}\n",
    "/* -- Ecosystems: other terrestrial -- */\n",
    ".tag-grassland-savanna{background:#f0e8d0;color:#5a3a10}.tag-shrubland{background:#e8e0d0;color:#4a3820}\n",
    ".tag-desert-dryland{background:#f5ead8;color:#6a4820}.tag-alpine-mountain{background:#e8e8f2;color:#2a2a5a}\n",
    ".tag-tundra-arctic{background:#deeef5;color:#1a3a4a}.tag-soil{background:#efe4d4;color:#5b3d18}\n",
    ".tag-urban{background:#e5e5e8;color:#3a3a4a}\n",
    "/* -- Organisms -- */\n",
    ".tag-plants{background:#e4f0d8;color:#2e5612}.tag-animals{background:#f0e4d8;color:#5a3812}\n",
    ".tag-birds{background:#f5ead8;color:#5a3200}.tag-fish{background:#d8e8f5;color:#1a3a58}\n",
    ".tag-fungi{background:#ead8f0;color:#4a1a5a}.tag-microbiome{background:#e4daf2;color:#46248a}\n",
    ".tag-pollinator{background:#fff0c4;color:#7a5300}\n",
    "/* -- Science topics -- */\n",
    ".tag-climate{background:#fde8d8;color:#8a3a0d}.tag-invasives{background:#fbe0e0;color:#8c1f1f}\n",
    ".tag-conservation{background:#d8efe2;color:#1f5e3c}.tag-evolution{background:#f5ecd5;color:#5c3a00}\n",
    ".tag-macroecology{background:#f0e8d8;color:#5c3a10}.tag-networks{background:#dce8f5;color:#1a3a5e}\n",
    ".tag-traits{background:#e6efd8;color:#2e4a12}.tag-policy{background:#ece1f4;color:#4c2773}\n",
    ".tag-ecosystem-services{background:#d8f0e8;color:#1a5a3a}.tag-genetics{background:#f0d8f5;color:#5a1a6a}\n",
    ".tag-movement{background:#d8e8f5;color:#1a3a5a}.tag-disease{background:#f5d8d8;color:#6a1a1a}\n",
    ".tag-biogeochemistry{background:#f0e8d0;color:#4a3a10}\n",
    "/* -- Approach / discipline -- */\n",
    ".tag-methods{background:#e5e7eb;color:#374151}.tag-modelling{background:#e8e8f2;color:#2a2a5a}\n",
    ".tag-remote-sensing{background:#d8eef5;color:#1a3a4a}.tag-citizen-science{background:#f5ecd8;color:#4a3a10}\n",
    ".tag-synthesis{background:#ece8f0;color:#3a2a5a}\n",
    "/* -- Post types -- */\n",
    ".tag-jobs{background:#fff3c4;color:#6e5400}.tag-events{background:#fcdef0;color:#7a1c5a}\n",
    ".tag-data{background:#d8f0ef;color:#1a4a4a}.tag-book{background:#f0ead8;color:#4a3a10}\n",
    ".tag-opinion{background:#f5e8d8;color:#5a3a10}.tag-preprint{background:#e8f0f5;color:#1a3a5a}\n",
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

# ---- Shared top navigation ---------------------------------
# Sits on every page directly under the banner -- gives the site
# a consistent header and one-click access to Home and Archive.
top_nav_block <- function() {
  paste0(
    "<p style='text-align:center;font-size:0.95rem;margin:0 0 1.3rem;",
    "padding-bottom:0.7rem;border-bottom:1px solid #eee;'>\n",
    "  <a href='", CONFIG$base_url, "/' style='text-decoration:none;margin:0 0.6rem;'>Home</a>\n",
    "  &middot;\n",
    "  <a href='", CONFIG$base_url, "/archives/' style='text-decoration:none;margin:0 0.6rem;'>Archive</a>\n",
    "</p>\n\n"
  )
}

# ---- Shared page footer ------------------------------------
# Identical bottom block on every page: a separator, optional
# extra navigation (e.g. prev/next digest links), Home/Archive
# links, the visitor counter, and the maintainer credit.
page_footer_block <- function(home = TRUE, archive = TRUE, extra_nav = "") {
  links <- character()
  if (home) {
    links <- c(links, paste0("<a href='", CONFIG$base_url, "/'>\U0001F3E0 Back to home</a>"))
  }
  if (archive) {
    links <- c(links, paste0("<a href='", CONFIG$base_url, "/archives/'>\U0001F4DA All digests</a>"))
  }
  nav_html <- if (length(links) > 0) {
    paste0("<p style='font-size:0.95rem;'>", paste(links, collapse = " &nbsp;&middot;&nbsp; "), "</p>\n\n")
  } else ""
  paste0(
    "---\n\n",
    extra_nav,
    nav_html,
    visitor_counter_block(),
    "<div style='text-align:center; font-size:small; color:gray;'>\n",
    "  This page is maintained by <a href='http://nicolasmouquet.free.fr/' target='_blank' rel='noopener' style='color:gray;'>Nicolas Mouquet</a>\n",
    "</div>\n"
  )
}

# ---- Digest page body --------------------------------------
build_digest_body <- function(X, start_date, end_date, nb_post,
                              all_post_md, prev_num = NULL, next_num = NULL,
                              wrapup = NULL) {
  # Prev/next digest links, passed to the footer.
  nav <- character()
  if (!is.null(prev_num)) nav <- c(nav, paste0("<a href='", CONFIG$base_url, "/archives/digest-", prev_num, "/'>&larr; Digest #", prev_num, "</a>"))
  if (!is.null(next_num)) nav <- c(nav, paste0("<a href='", CONFIG$base_url, "/archives/digest-", next_num, "/'>Digest #", next_num, " &rarr;</a>"))
  extra_nav <- if (length(nav) > 0) {
    paste0("<p style='font-size:0.95rem;'>", paste(nav, collapse = " &nbsp;|&nbsp; "), "</p>\n\n")
  } else ""

  # Wrap-up paragraph (inserted between the header line and the first post).
  wrapup_block <- if (!is.null(wrapup) && nzchar(wrapup)) {
    paste0("\n\n", wrapup, "\n\n")
  } else "\n\n"

  paste0(
    digest_inline_css(),
    banner_block(),
    top_nav_block(),
    ecosystem_block_digest(),
    "# Digest #", X, "\n\n",
    "Feeds are from **", format(start_date, "%B %d, %Y"),
    "** to **", format(end_date, "%B %d, %Y"),
    "**. Total posts: **", nb_post, "**.",
    wrapup_block,
    "---\n\n",
    paste0(all_post_md, collapse = ""),
    page_footer_block(home = TRUE, archive = TRUE, extra_nav = extra_nav)
  )
}

# ---- Homepage (landing) body -------------------------------
build_landing_body <- function(X, start_date, end_date, nb_post, registry) {
  # ONE "Digests" section: a featured card for the latest issue,
  # then -- only if older issues exist -- a compact list of them,
  # then the link to the full archive. No duplication.

  # Older issues = registry minus the current digest (#X).
  older <- Filter(
    function(e) !identical(safe(as.integer(e$num), NA_integer_), as.integer(X)),
    registry
  )
  older <- head(older, 6)

  older_block <- if (length(older) == 0) "" else {
    lines <- vapply(older, function(e) {
      paste0("- [Digest #", e$num, "](", e$url, ") ",
             format_digest_dates(e, with_year = TRUE))
    }, character(1))
    paste0(
      "<p style='font-weight:600;margin:1.4rem 0 0.4rem;'>Earlier issues</p>\n\n",
      paste(lines, collapse = "\n"), "\n\n"
    )
  }

  # Featured card for the latest digest.
  featured_card <- paste0(
    "<div style='border:1px solid #e5e5e5;border-radius:10px;",
    "padding:1.1rem 1.3rem;margin:0.6rem 0 1.1rem;background:#fafbfc;'>\n",
    "  <div style='font-size:1.15rem;font-weight:700;'>Digest #", X, "</div>\n",
    "  <div style='color:#666;font-size:0.92rem;margin:0.25rem 0 0.9rem;'>",
    format(start_date, "%B %d, %Y"), " &ndash; ", format(end_date, "%B %d, %Y"),
    " &middot; ", nb_post, " posts curated</div>\n",
    "  <a href='", CONFIG$base_url, "/archives/digest-", X, "/' ",
    "style='display:inline-block;padding:10px 18px;background:#2d6cdf;",
    "color:white;border-radius:6px;text-decoration:none;font-weight:600;'>",
    "Read Digest #", X, " →</a>\n",
    "</div>\n\n"
  )

  archive_link <- paste0(
    "<p><a href='", CONFIG$base_url, "/archives/' ",
    "style='display:inline-block;padding:8px 16px;border:1px solid #2d6cdf;",
    "color:#2d6cdf;border-radius:6px;text-decoration:none;'>",
    "Browse the full archive →</a></p>\n\n"
  )

  paste0(
    digest_inline_css(),
    banner_block(),
    top_nav_block(),
    "# ", CONFIG$site_title, "\n\n",
    "Curated digest of the \U0001F98B <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank' rel='noopener'>Bluesky Global Ecology feed</a> on biodiversity, ecosystems & conservation at large scales. New issue roughly every two weeks.\n\n",
    "---\n\n",
    "## Digests\n\n",
    featured_card,
    older_block,
    archive_link,
    "---\n\n",
    "## Global Ecology ecosystem\n\n",
    ecosystem_block_home(),
    page_footer_block(home = FALSE, archive = TRUE)
  )
}

# ---- Archive listing body ----------------------------------
# Digests grouped by year (newest year first), each line showing
# the issue number, its date range and post count.
build_archive_body <- function(registry) {
  if (length(registry) == 0) {
    return(paste0(
      digest_inline_css(),
      banner_block(),
      top_nav_block(),
      "# Archive\n\n",
      "_No digests yet._\n\n",
      page_footer_block(home = TRUE, archive = FALSE)
    ))
  }

  years      <- vapply(registry, function(x) as.character(x$year %||% "?"), character(1))
  uniq_years <- sort(unique(years), decreasing = TRUE)

  sections <- vapply(uniq_years, function(yr) {
    entries <- registry[years == yr]
    lines <- vapply(entries, function(e) {
      dates <- format_digest_dates(e, with_year = FALSE)
      np    <- as.character(e$nb_post %||% "")
      paste0("- [**Digest #", e$num, "**](", e$url, ") ", dates,
             if (nzchar(np)) paste0(" &middot; ", np, " posts") else "")
    }, character(1))
    paste0("## ", yr, "\n\n", paste(lines, collapse = "\n"), "\n")
  }, character(1))

  paste0(
    digest_inline_css(),
    banner_block(),
    top_nav_block(),
    "# Archive\n\n",
    "All past digests, organized by year (newest first).\n\n",
    paste(sections, collapse = "\n"),
    page_footer_block(home = TRUE, archive = FALSE)
  )
}

# ============================================================
# Main
# ============================================================

# ---- Paths -------------------------------------------------
end_date     <- Sys.Date()
start_date   <- end_date - CONFIG$days_back

archives_dir <- here::here(CONFIG$archives_dir)
feeds_dir    <- file.path(archives_dir, "feeds", format(end_date, "%Y"))
dir.create(archives_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(feeds_dir,    showWarnings = FALSE, recursive = TRUE)

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

# Raw feed snapshot saved later, after all processing

# ---- Caches (fetch + LLM titles + LLM tags) ----------------
title_cache_path     <- file.path(archives_dir, "title_cache.rds")
llm_title_cache_path <- file.path(archives_dir, "llm_title_cache.rds")
llm_tags_cache_path  <- file.path(archives_dir, "llm_tags_cache.rds")
title_cache     <- if (file.exists(title_cache_path))     readRDS(title_cache_path)     else list()
llm_title_cache <- if (file.exists(llm_title_cache_path)) readRDS(llm_title_cache_path) else list()
llm_tags_cache  <- if (file.exists(llm_tags_cache_path))  readRDS(llm_tags_cache_path)  else list()

# ---- Sanity-check LLM availability before the loop ---------
if (isTRUE(CONFIG$enable_llm_titles)) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(api_key)) {
    stop("ANTHROPIC_API_KEY is not set. ",
         "Add Sys.setenv(ANTHROPIC_API_KEY = \"sk-ant-...\") to pass.R and re-run.")
  }

  # Probe the API with a minimal call to catch billing errors before the loop.
  cat("Checking Anthropic API access...\n")
  probe_resp <- tryCatch({
    h <- curl::new_handle()
    curl::handle_setheaders(h,
      "anthropic-version" = "2023-06-01",
      "x-api-key"         = api_key,
      "content-type"      = "application/json"
    )
    curl::handle_setopt(h, post = TRUE, postfields = jsonlite::toJSON(list(
      model      = CONFIG$llm_model,
      max_tokens = 1L,
      messages   = list(list(role = "user", content = "hi"))
    ), auto_unbox = TRUE))
    r <- curl::curl_fetch_memory("https://api.anthropic.com/v1/messages", handle = h)
    jsonlite::fromJSON(rawToChar(r$content))
  }, error = function(e) list(error = list(message = conditionMessage(e))))

  if (!is.null(probe_resp$error)) {
    msg <- probe_resp$error$message %||% "unknown error"
    if (grepl("credit|billing|balance|payment|quota", msg, ignore.case = TRUE)) {
      stop("Anthropic API: no credits remaining.\n",
           "  Please top up your account at https://platform.claude.com/settings/billing\n",
           "  then re-run the script.\n",
           "  (API message: ", msg, ")")
    } else {
      stop("Anthropic API returned an error: ", msg,
           "\n  Check your API key and account at https://platform.claude.com")
    }
  }
  cat("Anthropic API: OK (model =", CONFIG$llm_model, ")\n")
}

# ---- Loop with per-post error isolation --------------------
all_post_md  <- character()
nb_post      <- 0L
kept_handles <- character()
post_meta    <- list()   # collects (num, title, tags) for wrap-up generation

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

    # Image fallback (tier 4): the poster's Bluesky profile picture.
    # Last resort -- ensures every post has a visual. Flagged as an
    # avatar so format_post() renders it as a circle, not a figure.
    if (is.null(post_image)) {
      avatar <- safe(feed$author[[i]]$avatar)
      if (!is.null(avatar) && is.character(avatar) && nzchar(avatar)) {
        post_image <- list(thumb = avatar, full = avatar, is_avatar = TRUE)
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
        if (!is.null(post_image)) {
          if (isTRUE(post_image$is_avatar)) " [img:avatar]" else " [img]"
        } else "",
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
      id           = post_id,
      post_num     = nb_post + 1L
    ))
    list(status = "ok", handle = handle, md = md,
         paper_title = paper_title, tags = tags,
         text = text, author_name = name)
  }, error = function(e) list(status = "error", handle = safe(feed$author[[i]]$handle, NA),
                              msg = conditionMessage(e)))

  switch(res$status,
    ok = {
      all_post_md  <- c(all_post_md, res$md)
      nb_post      <- nb_post + 1L
      post_meta    <- c(post_meta, list(list(
        num         = nb_post,
        title       = res$paper_title %||% "",
        tags        = res$tags %||% character(),
        handle      = res$handle %||% "",
        author_name = res$author_name %||% "",
        text        = res$text %||% ""
      )))
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

# ---- Generate digest wrap-up paragraph ---------------------
wrapup <- NULL
if (isTRUE(CONFIG$enable_llm_titles) &&
    nzchar(Sys.getenv("ANTHROPIC_API_KEY")) &&
    length(post_meta) > 0) {
  wrapup <- generate_wrapup_llm(post_meta)
  if (!is.null(wrapup)) {
    cat("Wrap-up ok (", nchar(wrapup), " chars)\n", sep = "")
  } else {
    cat("Wrap-up skipped or failed\n")
  }
}

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
                    prev_num = prev_num, next_num = next_num,
                    wrapup = wrapup)
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
# Read the registry we just updated -- it carries full metadata
# (dates, post counts, year) that list_digests() does not.
digests_registry <- read_digests_registry(data_path)

landing_markdown <- paste0(
  landing_front_matter(),
  build_landing_body(X, start_date, end_date, nb_post, digests_registry)
)
write_atomic(landing_markdown, here::here("index.md"))

archive_markdown <- paste0(
  archive_index_front_matter(),
  build_archive_body(digests_registry)
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

# Save raw feed snapshot (backup only, never read back by the pipeline)
save(feed, file = file.path(feeds_dir, paste0("feed_", strftime(end_date, "%V"), ".RData")))

cat("\n--- done ---\n")
cat("Digest #",    X,             "\n")
cat("Homepage :   index.md\n")
cat("Digest   :  ", archive_path,  "\n")
cat("Archive  :   archives/index.md\n")
cat("Registry :  ", data_path,     "\n")
cat("robots.txt:  robots.txt\n")
cat("Handles  :  ", handles_path,  "\n")
cat("nb_post  =", nb_post,         "\n")
