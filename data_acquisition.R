library(RSQLite)
library(dplyr)


#connect with sqlite to database
con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "DS_101/00_data/02_chinook/Chinook_Sqlite.sqlite")

#see Table of database
dbListTables(con)

#see album table
dplyr::tbl(con, "Album")

#load album table into ram
album_tbl <- tbl(con, "Album") %>% collect()

#disconnect from database

dbDisconnect(con)
con

###### now Web #######
library(httr)
con_web <- GET("https://swapi.dev/api/people/?page=2")
con_web
#load webpage to manipulate with glue
library(glue)
resp <- GET("https://swapi.dev/api/people/1/")

# create a function to call page from web
sw_api <- function(path){
  url <- modify_url(url= "https://Swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp)
}

resp <-sw_api("/people/1")
resp
## format response body
temp <- rawToChar(resp$content)
##convert to list data from the above
library(jsonlite)
next_temp <- fromJSON(temp)
toJSON(next_temp)

#list objekts
data_list <- list(strings= c("string1", "string2"), 
                  numbers = c(1,2,3), 
                  TRUE, 
                  100.23, 
                  tibble(
                    A = c(1,2), 
                    B = c("x", "y")
                  )
)

resp %>% 
  .$content %>%
  rawToChar() %>%
  fromJSON()
#accesing the content
content(resp, as = "text")
content(resp, as = "parsed")
content(resp)

##get wirecard stock value
resp <- GET('https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE')
resp
content(resp, as = "text")

#api acces with token
token    <- "my_individual_token"
response <- GET(glue("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey={token}"))
response


alphavantage_api_url <- "https://www.alphavantage.co/query"
ticker               <- "WDI.DE"
# You can pass all query parameters as a list to the query argument of GET()
GET(alphavantage_api_url, query = list('function' = "GLOBAL_QUOTE",
                                       symbol     = ticker,
                                       apikey     = Sys.getenv('TOKEN'))
    
#passwort speichern
keyring::key_set("token")
GET(alphavantage_api_url, query = list('function' = "GLOBAL_QUOTE",
                                       symbol     = ticker,
                                       apikey     = key_get("token"))
)
########web scrapping
########load an element from th wikipage
# get the URL for the wikipedia page with all S&P 500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the S&P 500 table using rvest
library(rvest)
sp_500 <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # Get the nodes with the id
  html_nodes(css = "#constituents") %>%
  # html_nodes(xpath = "//*[@id='constituents']"") %>% 
  # Extract the table and turn the list into a tibble
  html_table() %>% 
  .[[1]] %>% 
  as_tibble()

#########extract 250 top movies from idbm
url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- url %>% 
  read_html()
rank <-  html %>% 
  html_nodes(css = ".titleColumn") %>% 
  html_text() %>% 
  # Extrag all digits between " " and ".\n" The "\" have to be escaped
  # You can use Look ahead "<=" and Look behind "?=" for this
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  # Make all values numeric
  as.numeric()
title <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_text()

year <- html %>%
  html_node(".titleColumn .secondaryInfo > a") %>%
  html_text() %>%
  # Remove all brackets --> "(" OR ")"
  stringr::str_replace_all(pattern= "\\(|\\)", replacement = "") %>%
  as.numeric()

people <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_attr("title")

rating <- html %>% 
  html_nodes(css = ".imdbRating > strong") %>% 
  html_text() %>% 
  as.numeric()

num_ratings <- html %>% 
  html_nodes(css = ".imdbRating > strong") %>% 
  html_attr('title') %>% 
  # Extract the numbers and remove the comma to make it numeric values
  stringr::str_extract("(?<=based on ).*(?=\ user ratings)" ) %>% 
  stringr::str_replace_all(pattern = ",", replacement = "") %>% 
  as.numeric()

imdb_tbl <- tibble(rank, title, year, people, rating, num_ratings)

#### Sprache einstellen
resp <- GET(url = "https://www.imdb.com/chart/top/?ref_=nv_mv_250",  
            add_headers('Accept-Language' = "en-US, en;q=0.5"))
html <- content(resp)

## aus json ausgelesen
bike_data_lst <- fromJSON("bike_data.json")
bike_data_lst [["productDetail"]] [["variationAttributes"]] [["values"]] [[1]] [["displayValue"]]
