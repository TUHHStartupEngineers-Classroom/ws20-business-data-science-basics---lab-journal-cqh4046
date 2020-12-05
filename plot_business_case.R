library(lubridate)
library(tidyverse)

bike_orderlines_tbl <- read_rds("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

n <- 10
 top_customers_tbl <- bike_orderlines_tbl %>%
   
   #select relevant columns
   select(bikeshop, total_price) %>%
   
   #collapse the least frequentvalues into "other"
   mutate(bikeshop =as_factor(bikeshop) %>% fct_lump(n = n, w= total_price)) %>%
   
   #group and summarize
   group_by(bikeshop) %>%
   summarize(revenue = sum(total_price)) %>%
   ungroup() %>%
   
   #reorder the column customer_city by revenue
   mutate(bikeshop = bikeshop %>% fct_reorder(revenue)) %>%
   #place "Other" at the beginning
   mutate(bikeshop= bikeshop %>% fct_relevel("Other", after = 0)) %>%
   #sort by this column
   arrange(desc(bikeshop)) %>%
   
   #add Revenue Text
   
  mutate(revenue_text = scales::dollar(revenue,
                                      scale = 1e-6,
                                      prefix = "",
                                      suffix = "M Euro")) %>%
   
  #add Cumulative Percent
   mutate(cum_pct = cumsum(revenue)/sum(revenue)) %>%
   mutate(cum_pct_text = scales::percent(cum_pct))%>%
   
   #Add Rank
   mutate(rank = row_number()) %>%
   mutate(rank = case_when(
     rank == max(rank) ~ NA_integer_,
     TRUE ~ rank
   )) %>%
   
   #Add Label text
   mutate(label_text = str_glue("Rank: {rank}\nRev: {revenue_text}\nCumPct: {cum_pct_text}"))
 
 
 ##Data visualisation
 
 top_customers_tbl %>%
   
   #canvas
   ggplot(aes(revenue, bikeshop)) +
   
   #geometries
   geom_segment(aes(xend = 0, yend = bikeshop), 
                color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11],
                size  = 1) +
   
   geom_point(aes(size = revenue),
              color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) +
   
   geom_label(aes(label = label_text), 
              hjust = "inward",
              size  = 3,
              color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11])+
   # Formatting
   scale_x_continuous(labels = scales::dollar_format(scale= 1e-6,
                                                     prefix = "",
                                                     suffix = "M Euro"))+
   labs(
     title = str_glue("Top {n} Customers"),
     subtitle = str_glue(
       "Start: {year(min(bike_orderlines_tbl$order_date))}
               End:  {year(max(bike_orderlines_tbl$order_date))}"),
     x = "Revenue (M €)",
     y = "Customer",
     caption = str_glue("Top 6 customers contribute 52% of purchasing power.")
   ) +
   
   theme_minimal() +
   theme(
     legend.position = "none",
     plot.title = element_text(face = "bold"),
     plot.caption = element_text(face = "bold.italic")
   )
