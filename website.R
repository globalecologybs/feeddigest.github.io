source(here::here('pass.R'))
bskyr::set_bluesky_user('nmouquet.bsky.social')
bskyr::set_bluesky_pass(BLUESKY_PASS)

X <- 21

# Get the current date and 7 days ago
end_date <- Sys.Date()
start_date <- Sys.Date() - 4

# Install necessary package if not already installed
if (!require("bskyr")) {
  install.packages("bskyr")
}

# Fetch feed
feed <- bskyr::bs_get_feed('at://did:plc:ppsghcl5bbpgjcljnhra353s/app.bsky.feed.generator/global.ecology',
                           limit=150)
#feed <- feed[!sapply(feed$embed, is.null),]
feed <- feed[1:6,]

nb_post=0
for (i in 1:nrow(feed)){
  post_date <- tryCatch(
    as.Date(feed$record[[i]]$createdAt, format = "%Y-%m-%dT%H:%M:%OSZ"),
    error = function(e) NA
  )
  if (is.na(post_date) || post_date > start_date & post_date < end_date) {
    nb_post=nb_post+1
  }
}
 
#i=1
# Initialize Markdown content with metadata and header

markdown_text <- paste0(
  "<head>\n",
  "  <title>Global Ecology</title>\n",
  "  <link rel='icon' href='/feeddigest.github.io/favicon.png' type='image/png'>\n",
  "  <!-- Open Graph Metadata -->\n",
  "  <meta property='og:title' content='bluesky Global Ecology Feed Digest #", X, "'>\n", # Add X dynamically
  "  <meta property='og:description' content='Curated digest of the bluesky Global Ecology feed on biodiversity, ecosystems & conservation at large scales. Terrestrial, freswater & marine realms.'>\n",
  "  <meta property='og:image' content='https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology.jpg'>\n",
  "  <meta property='og:url' content='https://globalecologybs.github.io/feeddigest.github.io/'>\n",
  "  <meta property='og:type' content='website'>\n",
  "  <!-- SEO Keywords -->\n",
  "  <meta name='keywords' content='Global Ecology, Biogeography, Macroecology, Biodiversity, Ecosystems, Conservation, Marine Ecology, Terrestrial Ecology, Environmental Science, Climate Change, Sustainability, Bluesky Feed'>\n",
  "  <meta name='author' content='Global Ecology Team'>\n",
  "</head>\n\n",
  # Add banner image at the top
  "<div style='width:100%; text-align:center; margin-bottom:20px;'>\n",
  "  <img src='https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology_banner.png' alt='Global Ecology Banner' style='width:100%; height:auto;'>\n",
  "</div>\n\n",
  "# <img src='https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology.jpg' alt='Global Ecology' style='height: 1em; vertical-align: middle;'> <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank'> bluesky Global Ecology Feed</a> Digest #", X, "\n\n",
  "Feeds are from **", format(start_date, "%B %d, %Y"), "** to **", format(end_date, "%B %d, %Y"), "**. Total posts: **", nb_post, "**.\n\n",
  "For the lazy (yes we are) and friends who do not like social media (yes they can) but could benefit from the news on the Global Ecology feed ... here is a curated digest of the 🦋 bluesky Global Ecology feed 🌐 on biodiversity, ecosystems & conservation at large scales. Terrestrial, freswater & marine realms.\n\n",
  "- **SCIENCE ONLY**\n",
  "- DM <a href='https://bsky.app/profile/global-ecology.bsky.social' target='_blank'>@global-ecology.bsky.social</a> to contribute\n\n",
  "- Here to <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank'>like & pin the Global Ecology</a> feed\n\n",
  "- <a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3lfum2bjpab24' target='_blank'>Global Ecology starter pack Vol. 1</a>\n",
  "- <a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3ld2m2csaai2x' target='_blank'>Global Ecology starter pack Vol. 2</a>\n",
  "- <a href='https://go.bsky.app/MkLHiKU' target='_blank'>Global Ecology starter pack Vol. 3</a>\n\n",
  
  "---\n\n" # Separator
)



# Loop through the feed and format posts
for (i in 1:dim(feed)[1]) {
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
  likes <- tryCatch(feed$like_count[[i]], error = function(e) NULL)
  if (is.null(likes)) likes=0
  
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
    "##### Post by ", author_name, " ", author_link, " - ", post_date," -   💚 ",likes, "\n\n", # Add header
    "<div style='width:100%; padding:10px; border:none; box-sizing:border-box;'>\n", # Container for text
    "  ", gsub("\n", " ", text), "\n", # Post content
    if (!is.null(uri)) {
      paste0("<br><b>uri:</b> ", uri_hyperlink, "<br>")
    } else {
      "<br>"
    }, "\n", # Add URI as hyperlink if available
    "  <br><a href='", bluesky_link, "' target='_blank'>View Original Post</a>\n", # Link to the original post
    "</div>\n\n",
    "---\n\n" # Separator
  )
}

# Add the visitor counter at the end of the site
markdown_text <- paste0(
  markdown_text,
  "![Visitors](https://hits.sh/globalecologybs.github.io/feeddigest.github.io.svg)\n\n",
  "<div style='text-align:left; font-size:small; color:gray;'>\n",
  "  This page is maintained by <a href='http://nicolasmouquet.free.fr/' target='_blank' style='color:gray;'>Nicolas Mouquet</a>\n",
  "</div>\n"
)

# Specify the file path to save as index.md
file_path <- "index.md"

# Save the Markdown content to a file
writeLines(markdown_text, file_path)

# Save the list of handle concerned 
handles <- do.call(rbind,lapply (1:nrow(feed), function(i){
  post_date <- tryCatch(
    as.Date(feed$record[[i]]$createdAt, format = "%Y-%m-%dT%H:%M:%OSZ"),
    error = function(e) NA
  )
  if (is.na(post_date) || post_date > start_date & post_date < end_date) {
    paste0('@',feed$author[[i]]$handle)
  }
}))
colnames(handles) <- "handles"

handles <- handles[!duplicated(handles)]
handles_path <- file.path(dirname(here::here()), "postdm", "global_digest.csv")
write.csv2(handles,handles_path,row.names = F)

#done

cat("Markdown file saved as:", file_path, "\n")
cat("Handles file saved as", handles_path,"\n")
cat("nb_post=",nb_post)