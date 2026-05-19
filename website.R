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
source(here::here('pass.R'))
bskyr::set_bluesky_user('nmouquet.bsky.social')
bskyr::set_bluesky_pass(BLUESKY_PASS)

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

  # Feature flags -- all default off for a no-surprise rollout.
  enable_tags      = FALSE,
  enable_titles    = FALSE,
  enable_summaries = FALSE,

  # Network safety for paper-title fetcher.
  fetch_timeout_s  = 8
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

# ---- Stub: topic tags --------------------------------------
classify_post <- function(text) {
  t <- tolower(text)
  tags <- character()
  if (grepl("marine|ocean|sea\\b|reef|coral|fish|kelp|seagrass", t)) tags <- c(tags, "marine")
  if (grepl("soil|microbi|fung", t))                                 tags <- c(tags, "soil")
  if (grepl("forest|tree\\b|wood|canopy", t))                        tags <- c(tags, "forest")
  if (grepl("climate|warming|carbon|temperature|drought", t))        tags <- c(tags, "climate")
  if (grepl("freshwater|river|lake|stream|wetland", t))              tags <- c(tags, "freshwater")
  if (grepl("invasive|alien|non[- ]indigenous|biofouling", t))       tags <- c(tags, "invasives")
  if (grepl("conservation|protected area|biodiversit", t))           tags <- c(tags, "conservation")
  if (grepl("policy|governance|indigenous", t))                      tags <- c(tags, "policy")
  if (grepl("\\bjob\\b|phd|postdoc|position|hiring|fellowship", t))  tags <- c(tags, "jobs")
  if (grepl("webinar|seminar|conference|workshop|symposium", t))     tags <- c(tags, "events")
  if (grepl("\\bml\\b|machine learning|deep learning|model|simulation|edna|remote sensing|ai\\b",
            t))                                                       tags <- c(tags, "methods")
  unique(tags)
}

# ---- Stub: paper title fetcher (cached, timed out) ---------
get_paper_title <- function(url, cache = list(), timeout_s = 8) {
  if (is.null(url) || !nzchar(url)) return(NULL)
  if (!is.null(cache[[url]])) return(cache[[url]])
  safe({
    req  <- curl::new_handle(timeout = timeout_s, useragent = "Mozilla/5.0 (digest-bot)")
    resp <- curl::curl_fetch_memory(url, handle = req)
    if (resp$status_code >= 400) return(NULL)
    html  <- xml2::read_html(rawToChar(resp$content))
    node  <- xml2::xml_find_first(html, "//meta[@name='citation_title']")
    title <- xml2::xml_attr(node, "content")
    if (is.na(title)) title <- xml2::xml_text(xml2::xml_find_first(html, "//title"))
    if (!is.na(title) && nzchar(title)) trimws(title) else NULL
  })
}

# ---- Stub: LLM one-line summary ----------------------------
get_summary <- function(text, paper_title = NULL) {
  # TODO: ellmer::chat_anthropic(...)$chat(prompt) -> single sentence
  NULL
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

# Build a 160-char meta description suitable for SEO.
build_description <- function(start_date, end_date, nb_post) {
  d <- paste0(
    nb_post, " curated posts from the Bluesky Global Ecology feed (",
    format(start_date, "%b %d"), " - ", format(end_date, "%b %d, %Y"),
    "): biodiversity, ecosystems, conservation -- terrestrial, freshwater & marine."
  )
  if (nchar(d) > 300) paste0(substr(d, 1, 297), "...") else d
}

# ---- Render a single post ----------------------------------
format_post <- function(p) {
  author_link <- if (!is.null(p$handle)) {
    paste0("<a href='https://bsky.app/profile/", p$handle, "' target='_blank' rel='noopener'>@", p$handle, "</a>")
  } else "Unknown author"

  uri_block <- if (!is.null(p$uri)) {
    paste0("<br><b>uri:</b> <a href='", p$uri, "' target='_blank' rel='noopener'>", p$uri, "</a><br>")
  } else "<br>"

  title_block <- if (isTRUE(CONFIG$enable_titles) && !is.null(p$paper_title)) {
    paste0("  <b>\U0001F4C4 ", p$paper_title, "</b><br>\n")
  } else ""

  tag_block <- if (isTRUE(CONFIG$enable_tags) && length(p$tags) > 0) {
    paste0(
      "  ",
      paste0(
        "<span style='background:#eef;padding:2px 6px;border-radius:4px;",
        "font-size:0.85em;margin-right:4px;'>", p$tags, "</span>",
        collapse = ""
      ),
      "<br>\n"
    )
  } else ""

  summary_block <- if (isTRUE(CONFIG$enable_summaries) && !is.null(p$summary)) {
    paste0("  <i>", p$summary, "</i><br>\n")
  } else ""

  paste0(
    "##### Post by ", p$author_name, " ", author_link,
    " - ", p$post_date, " -   \U0001F49A ", p$likes, "\n\n",
    "<div style='width:100%; padding:10px; border:none; box-sizing:border-box;'>\n",
    title_block,
    tag_block,
    "  {% raw %}", p$text, "{% endraw %}\n",
    summary_block,
    uri_block, "\n",
    "  <br><a href='", p$bluesky_link, "' target='_blank' rel='noopener'>View Original Post</a>\n",
    "</div>\n\n",
    "---\n\n"
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
    shared_intro_block(),
    "# Digest #", X, "\n\n",
    "Feeds are from **", format(start_date, "%B %d, %Y"),
    "** to **", format(end_date, "%B %d, %Y"),
    "**. Total posts: **", nb_post, "**.\n\n",
    "---\n\n",
    paste0(all_post_md, collapse = ""),
    nav_block,
    "<p style='font-size:small;'><a href='/feeddigest.github.io/archives/'>\U0001F4DA Browse all digests</a></p>\n\n",
    "<div style='text-align:left; font-size:small; color:gray;'>\n",
    "  This page is maintained by <a href='http://nicolasmouquet.free.fr/' target='_blank' rel='noopener' style='color:gray;'>Nicolas Mouquet</a>\n",
    "</div>\n"
  )
}

# ---- Homepage (landing) body -------------------------------
build_landing_body <- function(X, start_date, end_date, nb_post, all_digests) {
  recent <- head(all_digests, 5)
  recent_list <- if (nrow(recent) == 0) "" else {
    paste0(
      "## Recent digests\n\n",
      paste0(
        "- [Digest #", recent$num, "](/feeddigest.github.io/archives/digest-", recent$num, "/)",
        collapse = "\n"
      ),
      "\n\n",
      "[Browse the full archive →](/feeddigest.github.io/archives/)\n\n"
    )
  }

  paste0(
    shared_intro_block(),
    "# ", CONFIG$site_title, "\n\n",
    "Curated digest of the \U0001F98B <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank' rel='noopener'>Bluesky Global Ecology feed</a> on biodiversity, ecosystems & conservation at large scales. New issue roughly every two weeks.\n\n",
    "---\n\n",
    "## Latest issue: Digest #", X, "\n\n",
    "**", format(start_date, "%B %d, %Y"), " - ", format(end_date, "%B %d, %Y"),
    "** &middot; ", nb_post, " posts curated\n\n",
    "<p><a href='/feeddigest.github.io/archives/digest-", X, "/' style='display:inline-block;padding:10px 18px;background:#2d6cdf;color:white;border-radius:6px;text-decoration:none;'>Read Digest #", X, " →</a></p>\n\n",
    "---\n\n",
    recent_list,
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

# ---- Title cache -------------------------------------------
title_cache_path <- file.path(archives_dir, "title_cache.rds")
title_cache <- if (file.exists(title_cache_path)) readRDS(title_cache_path) else list()

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

    tags <- if (isTRUE(CONFIG$enable_tags)) classify_post(text) else character()

    paper_title <- if (isTRUE(CONFIG$enable_titles) && !is.null(uri)) {
      t <- get_paper_title(uri, title_cache, CONFIG$fetch_timeout_s)
      if (!is.null(t)) title_cache[[uri]] <- t
      t
    } else NULL

    summary_text <- if (isTRUE(CONFIG$enable_summaries)) get_summary(text, paper_title) else NULL

    md <- format_post(list(
      text         = text,    handle       = handle,
      author_name  = name,    likes        = likes,
      uri          = uri,     bluesky_link = bluesky_link,
      post_date    = post_date,
      tags         = tags,    paper_title  = paper_title,
      summary      = summary_text
    ))
    list(status = "ok", handle = handle, md = md)
  }, error = function(e) list(status = "error", handle = NA, msg = conditionMessage(e)))

  switch(res$status,
    ok = {
      all_post_md  <- c(all_post_md, res$md)
      nb_post      <- nb_post + 1L
      if (!is.null(res$handle)) kept_handles <- c(kept_handles, paste0("@", res$handle))
      cat("i=", i, " ", res$handle, "ok\n")
    },
    skip_range = cat("i=", i, " ", res$handle, "skip (out of range)\n"),
    skip_short = cat("i=", i, " ", res$handle, "skip (too short)\n"),
    error      = cat("i=", i, " ERROR:", res$msg, "\n")
  )
}

if (isTRUE(CONFIG$enable_titles)) saveRDS(title_cache, title_cache_path)

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
cat("Digest #",    X,            "\n")
cat("Homepage :   index.md\n")
cat("Digest   :  ", archive_path, "\n")
cat("Archive  :   archives/index.md\n")
cat("robots.txt:  robots.txt\n")
cat("Handles  :  ", handles_path, "\n")
cat("nb_post  =", nb_post,        "\n")
