## create a small database of at least one category
## database content: model names and price for at least on category

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

url_mtb <- "https://www.rosebikes.de/fahrr%C3%A4der/mtb"

xopen(url_mtb)

html_mtb <- read_html(url_mtb)

bike_names_tbl <- html_mtb %>%
  html_nodes(css = ".catalog-category-bikes__title-text") %>%
  html_text() %>%
  stringr::str_replace_all("\n", "") %>%
  as_tibble() %>% 
  rename(name=value)

bike_price_tbl <- html_mtb %>%
  html_nodes(css = ".catalog-category-bikes__price-title") %>%
  html_text() %>%
  stringr::str_replace_all("\n", "") %>%
  stringr::str_replace_all("\200", "") %>%
  as_tibble() %>% 
  rename(name=value)

full_tibble <- bind_cols(bike_names_tbl, bike_price_tbl)  
 



