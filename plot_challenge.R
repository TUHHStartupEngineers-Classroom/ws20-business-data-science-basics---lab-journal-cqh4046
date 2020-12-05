library(lubridate)
library(tidyverse)

library(data.table)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)

class(covid_data_dt)

covid_tbl <- covid_data_dt %>%
  select(dateRep, cases, month,countriesAndTerritories, "Cumulative_number_for_14_days_of_COVID-19_cases_per_100000") %>%
  filter(countriesAndTerritories == 'Germany' | 
           countriesAndTerritories == 'United_Kingdom' | 
           countriesAndTerritories == 'Spain' | 
           countriesAndTerritories == 'France' | 
           countriesAndTerritories == 'United_States_of_America')%>%
  group_by(countriesAndTerritories) %>%
    mutate(date       = lubridate::dmy(dateRep)) %>%
    arrange(date) %>%
    mutate(cumsum = cumsum(cases))

max_value <-covid_tbl %>%
  select(cumsum) %>%
  arrange(desc(cumsum))
value <- max_value[1, "cumsum"]




 covid_tbl %>%
   
   #canvas
   
   ggplot(aes(date, cumsum, fill = countriesAndTerritories, color=countriesAndTerritories))+
   geom_line(size=1)+
   theme_light()+
   scale_y_continuous(labels = scales::dollar_format(scale= 1/ 1e6,
                                                     prefix = "",
                                                     suffix =  "M Cases"))+
   labs(
     title = "Cumulative Covid-19 cases",
     subtitle = "sorted by C[o]untrys",
     x = "Date (DDMMYYYY)",
     y = "Cumulative Cases",
     caption = str_glue("#Murika FIRST\nsuper sad story to be true")
   )+
   theme(
     legend.position = "bottom",
     plot.title = element_text(face = "bold"),
     plot.caption = element_text(face = "bold.italic"),
     axis.text.x = element_text(angle = 30, hjust = 1)
   )+
   scale_color_brewer(palette="Dark2")
 
 ############Challenge 2
 library(maps)
 library(ggplot2)
 
 world <- map_data("world") 
 
 covid_death_tbl <- covid_data_dt %>%
   select(countriesAndTerritories, geoId, popData2019, deaths) %>% 
   mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
   mutate(countriesAndTerritories = case_when(
     
     countriesAndTerritories == "United Kingdom" ~ "UK",
     countriesAndTerritories == "United States of America" ~ "USA",
     countriesAndTerritories == "Czechia" ~ "Czech Republic",
     TRUE ~ countriesAndTerritories
     
   ))  %>%
   group_by(countriesAndTerritories) %>%
   summarise(total_death = sum (deaths), popData2019)%>%
   unique()%>%
   mutate(mortality_rate = total_death/popData2019) %>%
   merge(y=world, by.x = "countriesAndTerritories", by.y = "region")

 
 covid_death_tbl %>% ggplot(world, aes(x=long, y=lat))

 
 world_map <- map_data("world")
 
 covid_death_tbl %>%
   ggplot()+
   geom_map(aes(x=long, 
                y=lat, 
                map_id = countriesAndTerritories, 
                fill = mortality_rate), 
            map=world)
 