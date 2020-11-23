# Project 1 for C2 - Webscraping

#install.packages("rvest")
#install.packages("selectr")
#install.packages("prettydoc")

library(rvest)
library(selectr)
library(prettydoc)
library(dplyr)
library(data.table)

scrape_page <- function(url){
  p<-read_html(url)
  write_html(p,'p.html')
  
  # Get container divs
  containers <-
    p %>% html_nodes('.c-compact-river__entry')
  
  # Iterate through the containers and extract html tags
  list_of_df <-
    lapply(containers, function(container){
      
      title <- 
        container %>% 
        html_nodes('.c-entry-box--compact__title') %>% 
        html_nodes('a') %>% 
        html_text()
      
      article_link <-
        container %>% 
        html_nodes('.c-entry-box--compact__title') %>% 
        html_nodes('a') %>% 
        html_attr("href")
      
      bylines <- 
        container %>% 
        html_nodes('.c-byline__item')
      
      author <- 
        container %>% 
        html_nodes('.c-byline__author-name') %>% 
        html_text()
      if (length(author) ==0) {
        author <- ''
      }
      
      author_link <-
        bylines[1] %>% 
        html_nodes('a') %>% 
        html_attr("href")
      if (length(author_link) ==0) {
        author_link <- ''
      }
      
      dt <- 
        bylines[2] %>% 
        html_nodes('time') %>% 
        html_attr('datetime')
      if (length(dt) ==0) {
        dt <- ''
      }
      
      comments <-
        container %>% 
        html_nodes('.c-entry-stat__comment-data') %>%
        nth(1) %>%
        html_text() %>% 
        trimws()
      if (length(comments) != 0) {
        comments <- as.numeric(comments)
        comments_link <- 
          container %>%
          html_nodes('.c-entry-stat--words') %>% 
          html_nodes('a') %>% 
          html_attr("href")
        # OR
        # comments_link <- paste0(article_link,'#comments')
      } else {
        comments <- ''
        comments_link <- ''
      }
      
      # it will be a one row data.frame
      return(data.frame('date'=dt, 'title'=title, 'article_link'= article_link,'author'=author, 'author_link'=author_link, 'comments'=comments, 'comments_link'=comments_link))
    })
  df_page = rbindlist(list_of_df)
  return(df_page)
}

get_num_pages<-function(url){
  #get the maximum number of pages to iterate through
  doc <- read_html(url)
  pages <-
    doc %>% 
    html_nodes('.c-pagination__text') 
  
  if (length(pages) != 0) { #if there is pagination
    pages <- pages %>% 
      html_text() %>% 
      strsplit("of") %>% 
      unlist() %>% 
      nth(2) %>% 
      as.numeric()   
  } else { #if there is no pagination
    pages <- 1
  }
  return(pages)
}

scrape_data_verge <- function(keywords,till_page=0){
  # saving the domain name to variable
  d='https://www.theverge.com'
  
  #creating the query string from keyword(s)
  #keywords=c()
  kwq=paste0('q=',keywords[1])
  if (length(keywords)>1){
    for (kw in keywords[2:length(keywords)]) {
      kwq=paste0(kwq,'+',as.character(kw))
    }
  }
 
  # create url from domain and query string to get number of pages
  url=paste0(d,'/search?',kwq)
  pages <- get_num_pages(url)
  
  # scrape all pages if not specified till which page should scrape
  if (till_page > 0) {
    pages <- till_page
  }
  
  # Create urls for iterative scraping
  urls=c()
  for (page in 1:pages){
    url=paste0(d,'/search?page=',page,'&',kwq)
    urls <- urls %>% append(url)
  }
  print(urls)
  
  # iterate through pages and get article data in df format
  df_list_urls <- lapply(urls, scrape_page)
  
  return(rbindlist(df_list_urls)) # unite dfs

}

# Test function
data<-scrape_page('https://www.theverge.com/search?page=1&q=zoom+linux')

# Verify final function result
all_pages<-scrape_data_verge("ubuntu")
pages_till_2<-scrape_data_verge("ubuntu",2)

# Export to csv
write.csv(pages_till_2,"verge_ubuntu_p2.csv",row.names = FALSE)
saveRDS(pages_till_2, file = "verge_ubuntu_p2.rds")
