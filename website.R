# ============================================================
# Global Ecology Bluesky Digest Generator
# ------------------------------------------------------------
# Refactored: helpers, bug fixes, archive support, and stubs
# for topic tags / paper titles / LLM summaries.
#
# Output is byte-identical to the previous script while all
# feature flags below are FALSE. Flip them on one at a time.
# ============================================================

# ---- Auth --------------------------------------------------
source(here::here('pass.R'))
bskyr::set_bluesky_user('nmouquet.bsky.social')
bskyr::set_bluesky_pass(BLUESKY_PASS)

# ---- Config ------------------------------------------------
CONFIG <- list(
  # Set to NULL to auto-detect the next digest number from
  # the contents of `archives/`. Set an integer to force.
  digest_number    = NULL,

  days_back        = 14,
  min_text_length  = 50,
  feed_limit       = 150,
  feed_uri         = 'at://did:plc:ppsghcl5bbpgjcljnhra353s/app.bsky.feed.generator/global.ecology',
  archives_dir     = "archives",

  # Feature flags -- leave FALSE for output parity with the
  # current site. Turn on one at a time after testing.
  enable_tags      = FALSE,
  enable_titles    = FALSE,
  enable_summaries = FALSE
)

# ---- Tiny helpers ------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

safe <- function(expr, default = NULL) {
  tryCatch(expr, error = function(e) default)
}

# Auto-pick next digest number from existing archive files.
# Files are named `digest-<N>.md` inside CONFIG$archives_dir.
next_digest_number <- function(archives_dir) {
  if (!dir.exists(archives_dir)) return(1L)
  files <- list.files(archives_dir, pattern = "^digest-\\d+\\.md$")
  if (length(files) == 0) return(1L)
  nums <- suppressWarnings(as.integer(gsub("^digest-(\\d+)\\.md$", "\\1", files)))
  max(nums, na.rm = TRUE) + 1L
}

# ---- Stub: topic tags --------------------------------------
# Lightweight keyword classifier. Replace later with an
# LLM-based classifier (e.g. ellmer) for better recall.
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

# ---- Stub: paper title fetcher -----------------------------
# Reads <meta name="citation_title"> first, falls back to
# <title>. Caches by URL so repeated runs don't re-fetch.
get_paper_title <- function(url, cache = list()) {
  if (is.null(url) || !nzchar(url)) return(NULL)
  if (!is.null(cache[[url]])) return(cache[[url]])
  safe({
    html  <- xml2::read_html(url)
    title <- xml2::xml_attr(
      xml2::xml_find_first(html, "//meta[@name='citation_title']"),
      "content"
    )
    if (is.na(title)) title <- xml2::xml_text(xml2::xml_find_first(html, "//title"))
    if (!is.na(title) && nzchar(title)) trimws(title) else NULL
  })
}

# ---- Stub: LLM one-line summary ----------------------------
# Replace body with an ellmer / httr2 call to your provider.
# Should return a single sentence (or NULL on failure).
get_summary <- function(text, paper_title = NULL) {
  # TODO: ellmer::chat_anthropic() or similar.
  # prompt <- paste("Summarize in one plain-English sentence:",
  #                 paper_title %||% "", text)
  NULL
}

# ---- Text cleaning -----------------------------------------
clean_text <- function(text) {
  if (is.null(text) || is.na(text)) return("")
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[!grepl("[.…]{3,}$", words)]      # drop truncated tails
  out <- paste(words, collapse = " ")
  out <- gsub("#", "", out)
  out <- gsub("\\.{3,}", "...", out)
  # Use [:space:] (POSIX) since \\s requires perl = TRUE.
  out <- gsub("https?://[^[:space:]]+\\.{3}", "", out, perl = TRUE)
  out <- gsub("[<>]", "", out)
  out <- gsub("[  ]", " ", out)
  gsub("\n", " ", out)
}

# ---- URI extraction ----------------------------------------
extract_uri <- function(post_record, post_embed) {
  facets <- post_record$facets
  for (j in seq_along(facets)) {
    u <- safe(facets[[j]]$features[[1]]$uri)
    if (!is.null(u)) return(u)
  }
  safe(post_embed$external$uri)
}

# ---- Render a single post ----------------------------------
format_post <- function(p) {
  author_link <- if (!is.null(p$handle)) {
    paste0("<a href='https://bsky.app/profile/", p$handle, "' target='_blank'>@", p$handle, "</a>")
  } else "Unknown author"

  uri_block <- if (!is.null(p$uri)) {
    paste0("<br><b>uri:</b> <a href='", p$uri, "' target='_blank'>", p$uri, "</a><br>")
  } else "<br>"

  # Optional enrichments -- empty strings when flags are off
  # so the output is byte-identical to the previous script.
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
    "  <br><a href='", p$bluesky_link, "' target='_blank'>View Original Post</a>\n",
    "</div>\n\n",
    "---\n\n"
  )
}

# ---- Header / footer ---------------------------------------
build_header <- function(X, start_date, end_date, nb_post) {
  paste0(
    "<head>\n",
    "  <title>Global Ecology </title>\n",
    "  <link rel='icon' href='/feeddigest.github.io/favicon.png' type='image/png'>\n",
    "  <meta property='og:title' content='bluesky Global Ecology Feed Digest #", X, "'>\n",
    "  <meta property='og:description' content='Curated digest of the bluesky Global Ecology feed on biodiversity, ecosystems & conservation at large scales. Terrestrial, freshwater & marine realms.'>\n",
    "  <meta property='og:image' content='https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology.jpg'>\n",
    "  <meta property='og:url' content='https://globalecologybs.github.io/feeddigest.github.io/'>\n",
    "  <meta property='og:type' content='website'>\n",
    "  <meta name='keywords' content='Global Ecology, Biogeography, Macroecology, Biodiversity, Ecosystems, Conservation, Marine Ecology, Terrestrial Ecology, Environmental Science, Climate Change, Sustainability, Bluesky Feed'>\n",
    "  <meta name='author' content='Global Ecology Team'>\n",
    "</head>\n\n",
    "<div style='width:100%; text-align:center; margin-bottom:20px;'>\n",
    "  <img src='https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology_banner.png' alt='Global Ecology Banner' style='width:100%; height:auto;'>\n",
    "</div>\n\n",
    "# <img src='https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology.jpg' alt='Global Ecology' style='height: 1em; vertical-align: middle;'> ",
    "<a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank'> bluesky Global Ecology Feed</a> Digest #", X, "\n\n",
    "Feeds are from **", format(start_date, "%B %d, %Y"), "** to **", format(end_date, "%B %d, %Y"),
    "**. Total posts: **", nb_post, "**.\n\n",
    "For the lazy (yes we are) and friends who do not like social media (yes they can) but could benefit from the news on the Global Ecology feed ... here is a curated digest of the \U0001F98B bluesky Global Ecology feed \U0001F310 on biodiversity, ecosystems & conservation at large scales. Terrestrial, freshwater & marine realms.\n\n",
    "- **SCIENCE ONLY (publications, data, jobs)**\n",
    "- Not on BlueSky ? email <a href='mailto:global.ecology.bs@gmail.com' target='_blank'> to receive weekly update</a>\n\n",
    "- On BlueSky ? DM <a href='https://bsky.app/profile/global-ecology.bsky.social' target='_blank'>@global-ecology.bsky.social</a> to contribute and receive weekly update\n\n",
    "- Here to <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank'>like & pin the Global Ecology</a> feed\n\n",
    "- <a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3lfum2bjpab24' target='_blank'>Global Ecology starter pack Vol. 1</a>\n",
    "- <a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3ld2m2csaai2x' target='_blank'>Global Ecology starter pack Vol. 2</a>\n",
    "- <a href='https://go.bsky.app/MkLHiKU' target='_blank'>Global Ecology starter pack Vol. 3</a>\n",
    "- <a href='https://go.bsky.app/Dsk4TQ3' target='_blank'>Global Ecology starter pack Vol. 4</a>\n\n",
    "---\n\n"
  )
}

build_footer <- function(archive_link = TRUE) {
  archive <- if (isTRUE(archive_link)) {
    paste0(
      "<div style='text-align:left; margin-bottom:10px;'>\n",
      "  <a href='archives/'>\U0001F4DA Browse previous digests</a>\n",
      "</div>\n\n"
    )
  } else ""

  paste0(
    archive,
    "![Visitors](https://hits.sh/globalecologybs.github.io/feeddigest.github.io.svg)\n\n",
    "<div style='text-align:left; font-size:small; color:gray;'>\n",
    "  This page is maintained by <a href='http://nicolasmouquet.free.fr/' target='_blank' style='color:gray;'>Nicolas Mouquet</a>\n",
    "</div>\n"
  )
}

# Build the archives/index.md listing all past digests.
build_archive_index <- function(archives_dir) {
  files <- list.files(archives_dir, pattern = "^digest-\\d+\\.md$")
  if (length(files) == 0) return(invisible(NULL))
  nums  <- suppressWarnings(as.integer(gsub("^digest-(\\d+)\\.md$", "\\1", files)))
  ord   <- order(nums, decreasing = TRUE)
  files <- files[ord]; nums <- nums[ord]

  body <- paste0(
    "# Global Ecology Digest — Archive\n\n",
    "All past digests, newest first.\n\n",
    paste0("- [Digest #", nums, "](", files, ")", collapse = "\n"),
    "\n"
  )
  writeLines(body, file.path(archives_dir, "index.md"))
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

# ---- Digest number -----------------------------------------
X <- CONFIG$digest_number %||% next_digest_number(archives_dir)

# ---- Fetch feed --------------------------------------------
if (!requireNamespace("bskyr", quietly = TRUE)) install.packages("bskyr")

feed <- bskyr::bs_get_feed(CONFIG$feed_uri, limit = CONFIG$feed_limit)
feed <- feed[!sapply(feed$uri,   is.na),   ]
feed <- feed[!sapply(feed$embed, is.null), ]

# ---- Locate previous-digest cutoff -------------------------
posts       <- if (is.list(feed) && "feed" %in% names(feed)) feed$feed else feed
handles_all <- vapply(posts$author, \(a) a$handle, character(1))
texts_all   <- vapply(posts$record, \(r) r$text,   character(1))

cut_hits <- which(
  handles_all == "global-ecology.bsky.social" &
    grepl("Global Ecology feed Digest", texts_all, fixed = TRUE)
)
if (length(cut_hits) == 0) {
  warning("No previous digest post found; processing all fetched posts.")
  cut_idx <- length(handles_all) + 1L
} else {
  # The most recent prior digest is the cutoff. (If you want
  # to skip your own digest post too, use min()+1 here.)
  cut_idx <- max(cut_hits)
}

# ---- Snapshot raw feed -------------------------------------
save(feed, file = file.path(year_dir, paste0("feed_", strftime(end_date, "%V"), ".RData")))

# ---- Title cache -------------------------------------------
title_cache_path <- file.path(archives_dir, "title_cache.rds")
title_cache <- if (file.exists(title_cache_path)) readRDS(title_cache_path) else list()

# ---- Loop --------------------------------------------------
all_post_md   <- character()
nb_post       <- 0L
kept_handles  <- character()

for (i in seq_len(cut_idx - 1L)) {
  handle <- safe(feed$author[[i]]$handle)

  # Date check first -- cheap, drops out-of-window posts early.
  post_date <- safe(
    as.Date(feed$record[[i]]$createdAt, format = "%Y-%m-%dT%H:%M:%OSZ"),
    NA
  )
  if (is.na(post_date) || post_date < start_date || post_date > end_date) {
    cat("i=", i, " ", handle, "REMOVED (out of range)\n")
    next
  }

  text <- clean_text(safe(feed$record[[i]]$text, ""))
  if (nchar(text) < CONFIG$min_text_length) {
    cat("i=", i, " ", handle, "REMOVED (too short)\n")
    next
  }

  name  <- safe(feed$author[[i]]$displayName)
  likes <- safe(feed$like_count[[i]], 0)
  if (is.null(likes)) likes <- 0
  uri   <- extract_uri(feed$record[[i]], feed$embed[[i]])

  bluesky_link <- gsub("at://",                 "https://bsky.app/profile/", feed$uri[[i]])
  bluesky_link <- gsub("app.bsky.feed.post/",   "post/",                     bluesky_link)

  # Optional enrichments (all gated by CONFIG flags).
  tags <- if (isTRUE(CONFIG$enable_tags)) classify_post(text) else character()

  paper_title <- if (isTRUE(CONFIG$enable_titles) && !is.null(uri)) {
    t <- get_paper_title(uri, title_cache)
    if (!is.null(t)) title_cache[[uri]] <- t
    t
  } else NULL

  summary_text <- if (isTRUE(CONFIG$enable_summaries)) get_summary(text, paper_title) else NULL

  post_md <- format_post(list(
    text         = text,
    handle       = handle,
    author_name  = name,
    likes        = likes,
    uri          = uri,
    bluesky_link = bluesky_link,
    post_date    = post_date,
    tags         = tags,
    paper_title  = paper_title,
    summary      = summary_text
  ))

  all_post_md  <- c(all_post_md, post_md)
  nb_post      <- nb_post + 1L
  if (!is.null(handle)) kept_handles <- c(kept_handles, paste0("@", handle))

  cat("i=", i, " ", handle, "ok\n")
}

# Persist title cache only if we actually used it.
if (isTRUE(CONFIG$enable_titles)) saveRDS(title_cache, title_cache_path)

# ---- Assemble & write --------------------------------------
markdown_text <- paste0(
  build_header(X, start_date, end_date, nb_post),
  paste0(all_post_md, collapse = ""),
  build_footer(archive_link = TRUE)
)

# Live homepage
writeLines(markdown_text, "index.md")

# Archive copy
archive_path <- file.path(archives_dir, paste0("digest-", X, ".md"))
writeLines(markdown_text, archive_path)

# Rebuild archive index
build_archive_index(archives_dir)

# ---- Handles CSV for postdm --------------------------------
kept_handles <- unique(kept_handles)
handles_df   <- data.frame(handles = kept_handles)
handles_path <- file.path(dirname(here::here()), "postdm", "global_digest.csv")
write.csv2(handles_df, handles_path, row.names = FALSE)

# Final snapshot
save(feed, file = file.path(year_dir, "feed.RData"))

cat("\n--- done ---\n")
cat("Digest #",     X,            "\n")
cat("Live page :   index.md\n")
cat("Archive   :  ", archive_path, "\n")
cat("Handles   :  ", handles_path, "\n")
cat("nb_post=",     nb_post,      "\n")
