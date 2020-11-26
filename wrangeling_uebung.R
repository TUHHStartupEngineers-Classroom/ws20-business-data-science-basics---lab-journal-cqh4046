bike_orderlines_tbl <- read_rds("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")


temp <- bike_orderlines_tbl %>%
  mutate(freight_costs = 2 * weight)

bike_orderlines_tbl %>%
  summarise(
    revenue = sum(total_price)
  )

bike_orderlines_tbl %>%
  group_by(category_1) %>%
  summarise(revenue = sum(total_price))


################################### DATA.TABLE ###############################

library(data.table)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)

class(covid_data_dt)

test_dt <- data.table(ID = c("b","b","b","a","a","c"),
                      a  = 1:6,
                      b  = 7:12,
                      c  = 13:18)


## FROM[WHERE, SELECT/ORDER BY/UPDATE, GROUP BY]

#covid_data_dt[i, j, by]

# Example (filter by year, sum cases, group by continent)
covid_data_dt[year == 2020, sum(cases), by = continentExp]

covid_data_dt[countriesAndTerritories == "Germany" & 
                lubridate::month(dateRep, label = T, abbr = F) == "June"]

covid_data_dt[1:2]

covid_data_dt[order(year, month, day, countriesAndTerritories)]

covid_data_dt[,geoId]
covid_data_dt[,c("geoId", "countriesAndTerritories")]
temp<-covid_data_dt[,list(geoId)]
covid_data_dt[,.(geoId)]

t <- covid_data_dt[,sum(deaths > 1000)]

t <- covid_data_dt[deaths > 1000, sum(geoID)]
t_1 <- covid_data_dt[countriesAndTerritories == "Germany" & month == 11, 
                     .(m_cases= mean(cases),
                     m_death = mean(deaths)
                     )
                     ]

covid_data_dt[country == "Germany" & month == 11, 
              .(m_cases = mean(cases), 
                m_death = mean(deaths)
              )
]

covid_data_dt[countriesAndTerritories == "United_States_of_America" & deaths < 1000, 
              .N
]

covid_data_dt[deaths > 1000, .N , by = countriesAndTerritories]

covid_data_dt[,.I[deaths > 1000]]
#################### exercise ###################

aq_dt <- data.table(airquality)

temp<-aq_dt[!is.na(Ozone), .(Solar.R, Wind, Temp)]

cars_dt <- data.table(mtcars)

cars_dt[, mileage_type := ifelse(mpg > 20, 'high', 'low')]
library(magrittr) # to use the pipe

mtcars_dt[, .(.N, mileage = mean(mpg) %>% round(2)), by=gear]
cars_dt[, .(.N, mileage = mean(mpg) %>% round(2)), by=gear]

combined_data[,current_loan_delinquency_status] %>% unique()
