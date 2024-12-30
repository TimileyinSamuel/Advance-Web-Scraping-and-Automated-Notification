################################################  START ####################################################
##### Package installation and loading #####
# install.packages("blastula")
# install.packages("glue")
###################################### SETTING UP EMAIL NOTIFICATION ###############################################
library(blastula)
library(glue)
library(keyring)

##### My gmail details #####
create_smtp_creds_key(
  id = "gmail_credential",
  user = "senderemailaddress@gmail.com",
  provider = "gmail",
  host = "smtp.gmail.com",
  port = 587,
  use_ssl = TRUE,
  overwrite = TRUE
)

##### Email notification for new articles on Website
send_article_notification <- function(article_title, article_link) {
  email <- compose_email(
    body = md(glue(
      "A new article has been published on the website!\n\n**Title:** {article_title}\n**Link:** {article_link}"
    ))
  )
  
### Make subject as a single string
subject_text <- as.character(glue("New Article Published: {article_title}"))
  
### Send the email notification to receiveremailaddress@gmail.com
smtp_send(
    email = email,
    from = "senderemailaddress@gmail.com",
    to = "receiveremailaddress@gmail.com",
    subject = subject_text,
    credentials = creds_key(id = "gmail_credential"),
    verbose = TRUE
  )
}

################################### WEBSITE SCRAPPING ######################################
### Loading web scraping library
library(rvest)

### Defining the URL of Website
archive_url <- "https://www.website.com"

### Path to the file where previously fetched articles will be saved
prev_articles_file <- "previous_articles.txt"

### Function to check for new articles
check_for_new_articles <- function() {
### Reading the HTML of the archive page
  page <- read_html(archive_url)
  
### Extracting all article titles and links
  article_titles <- page %>% html_elements('[data-testid="preview-title"]') %>% html_text(trim = TRUE)
  article_links <- page %>% html_elements('[data-testid="preview-title"]') %>% html_attr("href")
  
### Construct full URLs if links are relative
  article_links <- ifelse(grepl("^https?://", article_links), article_links,
                          paste0("https://www.website.com", article_links))
  
### Load previously saved articles
  if (file.exists(prev_articles_file)) {
    prev_articles <- readLines(prev_articles_file)
  } else {
    prev_articles <- character()  # Starts with an empty list if no file exists
  }
  
### Identify new articles by checking if they are in the previous list
  new_articles <- article_links[!article_links %in% prev_articles]
  new_titles <- article_titles[!article_links %in% prev_articles]
  
### Send notifications for new articles
  if (length(new_articles) > 0) {
    for (i in seq_along(new_articles)) {
      send_article_notification(new_titles[i], new_articles[i])
    }
### Update the file with the current list of article links
    writeLines(article_links, prev_articles_file)
  } else {
    message("No new articles found.")
  }
}

### Run the function to check for new articles
check_for_new_articles()
