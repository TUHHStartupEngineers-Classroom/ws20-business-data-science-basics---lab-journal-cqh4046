library("tidyverse")


diamonds2 <- readRDS("diamonds2.rds")

diamonds2 %>% head(n = 5)

#shit ist nicht direkt verständlich, niemand wusste das die Zahl unter Jahr irgendwie
#einen scheiß Preis darstellen sollte
#nimmt die Preise unter den Jahreszahlen und schiebt sie untereinander geordnet wech
# auf [cut, year, prise]
diamonds2 %>% pivot_longer(cols = c("2008","2009"), 
                           names_to = 'year',
                           values_to='price') %>%
  head(n=10)


?pivot_wider


diamonds3 <- readRDS("diamonds3.rds")

diamonds3 %>% head(n = 5)

#verbindet Daten von dimensionen mit den Messwerten, wird genutzt
#weil Daten zu einer Messung über mehrere Zeilen verschmiert wurde
#verändert Datenformat auf [cut, price, clarity, x,y,z] und dann alles unter weg
diamonds3 %>% pivot_wider(names_from = "dimension",
                          values_from= "measurement") %>%
  head(n=5)


#übung zum trennen von Daten
diamonds4 <- readRDS("diamonds4.rds")

diamonds4

x <- diamonds4 %>% separate(col = "dim",
                       into= c("x","y","z"),
                       sep= "/", convert = T) %>%
  head(n=5)
diamonds4
?unite()

#unite shit under first eypression from other expression with spacing given as sep=''
diamonds5 <- readRDS("diamonds5.rds")
diamonds5

diamonds5 %>% 
  unite(clarity, clarity_prefix, clarity_suffix, sep='')%>% 
  head(n=5)

## Filtern und sclicen

library(ggplot2) # To load the diamonds dataset
library(dplyr)
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  head(5)

## # A tibble: 5 x 10
##   carat cut     color clarity depth table price     x     y     z
##                
## 1 0.23  Ideal   E     SI2      61.5    55   326  3.95  3.98  2.43
## 2 0.290 Premium I     VS2      62.4    58   334  4.2   4.23  2.63
## 3 0.23  Ideal   J     VS1      62.8    56   340  3.93  3.9   2.46
## 4 0.31  Ideal   J     SI2      62.2    54   344  4.35  4.37  2.71
## 5 0.32  Premium E     I1       60.9    58   345  4.38  4.42  2.68

diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  slice(3:4)

diamonds
#umsortieren von Listen, je nach angabe hintereinander werg
#erste Ordnung, zweite Ordnung, dritte Ordnung -->>> von klein nach Groß
diamonds %>% 
  arrange(cut, carat, desc(price))

#auswahl von color clarity und von x bis z
diamonds %>% 
  #Auswahlmöglichkeitebn stats_with(), ends_with(), contains(), everything()
  select(color, clarity, x:z) %>% 
  head(n = 5)

#alles außer x bis z wählen
diamonds %>% 
  select(-(x:z)) %>% 
  head(n = 5)

#renaming
diamonds %>% 
  rename(var_x = x) %>% 
  head(n = 5)

#neue Variable aus anderen erstellen

diamonds %>% 
  mutate(p = x + z, q = p + y) %>% 
  select(-(depth:price)) %>% 
  head(n = 5)
#fügt variablen zusammen

diamonds %>% 
  transmute(carat, cut, sum = x + y + z) %>% 
  head(n = 5)

#bind_cols() and bind_rows(): binds two tibbles column-wise or row-wise.

#group_by() and summarize() reduces multiple values down to a single summary
diamonds %>% 
  group_by(cut) %>% 
  summarize(max_price  = max(price),
            mean_price = mean(price),
            min_price  = min(price))

#rohe Datenübersicht
glimpse(diamonds)

#bib zum arbeiten mit Datumsangaben
library(lubridate)
ymd(20101215)
## "2010-12-15"
mdy("4/1/17")
## "2017-04-01"