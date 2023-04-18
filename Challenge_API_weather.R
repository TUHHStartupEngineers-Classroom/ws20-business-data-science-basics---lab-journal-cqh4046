library(httr)
library(glue)
library(jsonlite)
library(tidyverse)
library(dplyr)

city_names <- c("London", "Hamburg", "Bremen", "Paris", "Moskau")
final_tibble = NULL

for(name in city_names){
  temp <- weather_api_foo(name)
  if(is.null(final_tibble)){
    final_tibble <- temp
  }
  
  if(is.null(final_tibble) == FALSE){
    final_tibble <- bind_rows(final_tibble, temp)
  }
}


  
weather_api_foo <- function(city){
  API_key <- "4ecc371b838af43c32214d58d6cbc46b"
  resp <- GET(glue("https://api.openweathermap.org/data/2.5/weather?q={city}&appid={API_key}"))
  
  response_body<-rawToChar(resp$content)
  weather_list <- fromJSON(response_body)
  
  cords<-as_tibble(weather_list[["coord"]])
  weather<-as_tibble(weather_list[["weather"]])%>% rename(sky=main)
  wind<-as_tibble(weather_list[["wind"]])
  city<-as_tibble(weather_list[["name"]]) %>% rename(city=value)
  temp<-as_tibble(weather_list[["main"]][["temp"]]) %>% rename(temp_Kelvin=value)
  
  a <- c(cords,weather,wind,city,temp)
  
  full_tibble <- as_tibble(a) %>% 
    select("city","lon", "lat", "sky", "description","temp_Kelvin")
  
  return(full_tibble)
}
