source(here::here('pass.R'))
bskyr::set_bluesky_user('nmouquet.bsky.social')
bskyr::set_bluesky_pass(BLUESKY_PASS)


# Install necessary package if not already installed
if (!require("bskyr")) {
  install.packages("bskyr")
}

# Fetch feed
feed <- bskyr::bs_get_feed('at://did:plc:ppsghcl5bbpgjcljnhra353s/app.bsky.feed.generator/global.ecology',
                           limit=100)

# Get the current date and 7 days ago
end_date <- Sys.Date()
start_date <- Sys.Date() - 6

# Add metadata for the title and initialize content without a duplicate Markdown title
# Initialize Markdown content with metadata and header
# Initialize Markdown content with metadata and header
# markdown_text <- paste0(
#   "---\n",
#   "title: \"<img src='https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology.jpg' alt='Global Ecology' style='height: 1em; vertical-align: middle;'> [Global Ecology Feed](https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology) Digest!\"\n",
#   "---\n\n",
#   "Bluesky Feed from **", format(start_date, "%B %d, %Y"), "** to **", format(end_date, "%B %d, %Y"), "**.\n\n",
#   "<br><br>\n\n" # Add two HTML line breaks explicitly for rendering
# )

# markdown_text <- paste0(
#   "<head>\n",
#   "  <title>Global Ecology</title>\n", # Specify the desired bookmark title
#   "  <link rel='icon' href='/favicon.ico' type='image/x-icon'>\n", # Favicon link
#   "</head>\n\n",
#   "# <img src='https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology.jpg' alt='Global Ecology' style='height: 1em; vertical-align: middle;'> <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank'>Global Ecology Feed</a> Digest!\n\n",
#   "Feeds are from **", format(start_date, "%B %d, %Y"), "** to **", format(end_date, "%B %d, %Y"), "**.\n\n",
#   "A feed on research in biodiversity, ecosystems & conservation at large scale. Terrestrial & marine realms. Sounds like Global Ecology!\n\n",
#   "- **SCIENCE ONLY**\n",
#   "- DM <a href='https://bsky.app/profile/global-ecology.bsky.social' target='_blank'>@global-ecology.bsky.social</a> to contribute\n\n"
# )

markdown_text <- paste0(
  "<head>\n",
  "  <title>Global Ecology</title>\n",
  "  <link rel='icon' href='/feeddigest.github.io/favicon.png' type='image/png'>\n",
  "  <!-- Open Graph Metadata -->\n",
  "  <meta property='og:title' content='Global Ecology Feed Digest'>\n",
  "  <meta property='og:description' content='For the lazy (yes we are) and friends who do not like social media (yes they can) but could benefit from the news on the Global Ecology feed ... here is a curated digest of the 🦋 bluesky Global Ecology feed 🌐 on biodiversity, ecosystems & conservation at large scales. Terrestrial & marine realms.'>\n",
  "  <meta property='og:image' content='https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology.jpg'>\n",
  "  <meta property='og:url' content='https://globalecologybs.github.io/feeddigest.github.io/'>\n",
  "  <meta property='og:type' content='website'>\n",
  "</head>\n\n",
  "# <img src='https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology.jpg' alt='Global Ecology' style='height: 1em; vertical-align: middle;'> <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank'>Global Ecology Feed</a> Digest!\n\n",
  "Feeds are from **", format(start_date, "%B %d, %Y"), "** to **", format(end_date, "%B %d, %Y"), "**.\n\n",
  "For the lazy (yes we are) and friends who do not like social media (yes they can) but could benefit from the news on the Global Ecology feed ... here is a curated digest of the 🦋 bluesky Global Ecology feed 🌐 on biodiversity, ecosystems & conservation at large scales. Terrestrial & marine realms.\n\n",
  "- **SCIENCE ONLY**\n",
  "- DM <a href='https://bsky.app/profile/global-ecology.bsky.social' target='_blank'>@global-ecology.bsky.social</a> to contribute\n\n"
)

# Loop through the feed and format posts
for (i in 2:dim(feed)[1]) {
  #i=1
  text <- feed$record[[i]]$text
  
  # Split the text into words and remove any word ending with "…" or "...."
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[!grepl("[.…]{3,}$", words)] # Remove words ending with "…" or "...."
  text <- paste(words, collapse = " ")
  text <- gsub("#", "", text)
  
  # Skip posts with less than 40 characters
  if (nchar(text) < 50) {
    next
  }
  
  # Extract author and post details
  author_handle <- tryCatch(feed$author[[i]]$handle, error = function(e) NULL)
  author_name <- tryCatch(feed$author[[i]]$displayName, error = function(e) NULL)
  
  uri <- NULL
  for (j in 1:length(feed$record[[i]]$facets)) {
    if (is.null(uri)) {
      uri <- tryCatch(feed$record[[i]]$facets[[j]]$features[[1]]$uri, error = function(e) NULL)
    }
  }
  if (is.null(uri)) {
    uri <- tryCatch(feed$embed[[i]]$external$uri, error = function(e) NULL)
  }
  
  post_date <- tryCatch(
    as.Date(feed$record[[i]]$createdAt, format = "%Y-%m-%dT%H:%M:%OSZ"),
    error = function(e) NA
  )
  
  # Skip posts outside the date range
  if (is.na(post_date) || post_date < start_date || post_date > end_date) {
    next
  }
  
  # Create hyperlinks
  author_link <- if (!is.null(author_handle)) {
    paste0("<a href='https://bsky.app/profile/", author_handle, "' target='_blank'>@", author_handle, "</a>")
  } else {
    "Unknown author"
  }
  bluesky_link <- gsub("at://", "https://bsky.app/profile/", feed$uri[[i]])
  bluesky_link <- gsub("app.bsky.feed.post/", "post/", bluesky_link)
  
  uri_hyperlink <- if (!is.null(uri)) {
    paste0("<a href='", uri, "' target='_blank'>", uri, "</a>")
  } else {
    "No URI available"
  }
  
  # Ensure the URI is displayed
  markdown_text <- paste0(
    markdown_text,
    "##### Post by ", author_name, " ", author_link, " on ", post_date, "\n\n", # Add header
    "<div style='width:100%; padding:10px; border:none; box-sizing:border-box;'>\n", # Container for text
    "  ", gsub("\n", " ", text), "\n", # Post content
    if (!is.null(uri)) paste0("<br><b>uri:</b> ", uri_hyperlink, "<br>"), "\n", # Add URI as hyperlink if available
    "  <br><a href='", bluesky_link, "' target='_blank'>View Original Post</a>\n", # Link to the original post
    "</div>\n\n",
    "---\n\n" # Separator
  )
}

# 
# for (i in 2:dim(feed)[1]) {
#   #i=2
#   text <- feed$record[[i]]$text
#   
#   # Split the text into words and remove any word ending with "…" or "...."
#   words <- unlist(strsplit(text, "\\s+")) # Split by whitespace
#   words <- words[!grepl("[.…]{3,}$", words)] # Remove words ending with "…" or "...."
#   text <- paste(words, collapse = " ") # Reassemble the text
#   text <- gsub("#", "", text)
#   
#   # Skip the post if the text contains less than 40 characters
#   if (nchar(text) < 50) {
#     next
#   }
#   
#   # Attempt to fetch the author handle, URI, and thumbnail image, safely handling errors
#   author_handle <- tryCatch(
#     feed$author[[i]]$handle,
#     error = function(e) NULL
#   )
#   
#   author_name <- tryCatch(
#     feed$author[[i]]$displayName,
#     error = function(e) NULL
#   )
#   
#   uri <- NULL
#   for (j in 1:length(feed$record[[i]]$facets)){
#     if (is.null(uri)){
#       uri <- tryCatch(
#         feed$record[[i]]$facets[[j]]$features[[1]]$uri,
#         error = function(e) NULL
#       )
#     }
#   }
#   
#   if (is.null(uri)){
#     uri <- tryCatch(
#       feed$embed[[i]]$external$uri,
#       error = function(e) NULL
#     )
#   }
#   
#   
#   # Convert to a Bluesky link
#   bluesky_link <- gsub("at://", "https://bsky.app/profile/", feed$uri[[i]])
#   bluesky_link <- gsub("app.bsky.feed.post/", "post/", bluesky_link)
#   
#   thumb_url <- tryCatch(
#     feed$embed[[i]]$images[[1]]$thumb,
#     error = function(e) NULL
#   )
#   if (is.null(thumb_url)) {
#     thumb_url <- tryCatch(
#       feed$embed[[i]]$external$thumb,
#       error = function(e) NULL
#     )
#   }
#   
#   # Extract the post date and format it
#   post_date <- tryCatch(
#     as.Date(feed$record[[i]]$createdAt, format = "%Y-%m-%dT%H:%M:%OSZ"), # Parse ISO 8601 format
#     error = function(e) NA
#   )
#   
#   # Skip posts outside the date range
#   if (is.na(post_date) || post_date < start_date || post_date > end_date) {
#     next
#   }
#   
#   # Create hyperlink for the author handle if it exists
#   author_link <- if (!is.null(author_handle)) {
#     paste0("<a href=\"https://bsky.app/profile/", author_handle, "\" target=\"_blank\">@", author_handle, "</a>")
#   } else {
#     "Unknown author"
#   }
#   
#   # Convert the `uri` into a hyperlink if it exists
#   uri_hyperlink <- if (!is.null(uri)) {
#     paste0("<a href=\"", uri, "\" target=\"_blank\">", uri, "</a>")
#   } else {
#     "No URI available"
#   }
#   
#   # Add the record content as a Markdown table with fixed-width cells and resized images
#   # markdown_text <- paste0(
#   #   markdown_text,
#   #   "##### Post by ", author_name, " ", author_link, " on ", post_date, "\n\n", # Add record header with author and date
#   #   "<table style=\"width:100%; table-layout:fixed; border:none;\">\n", # Enforce table width, no borders
#   #   "<tr>\n",
#   #   "  <td style=\"vertical-align:top; width:75%; padding:10px; box-sizing:border-box; overflow-wrap:break-word; word-wrap:break-word; hyphens:auto;\">\n", # Ensure 75% width with box-sizing and text wrapping
#   #   "    ", gsub("\n", " ", text), "\n", # Post content
#   #   "    ", if (!is.null(uri)) paste0("<br><b>uri:</b> ", uri_hyperlink, "<br>"), "\n", # Add URI as hyperlink
#   #   "    <br><a href=\"", bluesky_link, "\" target=\"_blank\">View Original Post</a>\n", # Add hyperlink to the original post on a new line
#   #   "  </td>\n",
#   #   "  <td style=\"vertical-align:top; width:25%; padding:10px; box-sizing:border-box; text-align:right;\">\n", # Ensure 25% width, align image right
#   #   "    ", if (!is.null(thumb_url)) paste0("<img src=\"", thumb_url, "\" style=\"max-height:150px; width:100%; object-fit:contain;\">") else "", "\n", # Add image with max height and consistent scaling
#   #   "  </td>\n",
#   #   "</tr>\n",
#   #   "</table>\n\n",
#   # )
#   
#   markdown_text <- paste0(
#     markdown_text,
#     "##### Post by ", author_name, " ", author_link, " on ", post_date, "\n\n", # Add record header with author and date
#     "<table style=\"width:100%; table-layout:fixed; border-collapse:collapse;\">\n", # Enforce table width and collapse borders
#     "<tr>\n",
#     "  <td style=\"width:75%; padding:10px; vertical-align:top; border:none;\">\n", # 75% width for text, no border
#     "    ", gsub("\n", " ", text), "\n", # Post content
#     "    ", if (!is.null(uri)) paste0("<br><b>uri:</b> ", uri_hyperlink, "<br>"), "\n", # Add URI as hyperlink
#     "    <br><a href=\"", bluesky_link, "\" target=\"_blank\">View Original Post</a>\n", # Add hyperlink to the original post
#     "  </td>\n",
#     "  <td style=\"width:25%; padding:10px; text-align:right; vertical-align:top; border:none;\">\n", # 25% width for image, aligned right, no border
#     "    ", if (!is.null(thumb_url)) paste0("<img src=\"", thumb_url, "\" style=\"max-height:150px; width:auto;\">") else "", "\n", # Add image
#     "  </td>\n",
#     "</tr>\n",
#     "</table>\n\n"
#   )
# }

# Add the visitor counter at the end of the site
markdown_text <- paste0(
  markdown_text,
  "![Visitors](https://hits.sh/globalecologybs.github.io/feeddigest.github.io.svg)\n"
)

# Specify the file path to save as index.md
file_path <- "index.md"

# Save the Markdown content to a file
writeLines(markdown_text, file_path)

cat("Markdown file saved as:", file_path, "\n")