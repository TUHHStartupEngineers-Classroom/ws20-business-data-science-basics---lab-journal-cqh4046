library(tidyverse)
library(lubridate)
library(scales)

bike_orderlines_tbl <- read_rds("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
# 1.0 Anatomy of a ggplot ----

# 1.1 How ggplot works ----

# Step 1: Format data ----
sales_by_year_tbl <- bike_orderlines %>%
  
  #selecting columns
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  
  #grouping by year
  group_by(year) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  
  #format Text
  mutate(sales_text = dollar(sales, accuracy = NULL, scale=1, 
                             prefix = "$", 
                             suffix = "", 
                             big.mark =  ",", 
                             decimal.mark =  ".", 
                             trim = TRUE, 
                             largest_with_cents = 1e+05, 
                             negative_parens = FALSE))
#show table
sales_by_year_tbl

# Step 2: Plot ----
sales_by_year_tbl %>%
  
  #canvas
  ggplot(aes(x=year, y = sales, color = sales)) +
  
  #geometries
  geom_line(size=1) +
  geom_point(aes(size=2), color ="red")+ #unterschiedlich große Punkte
  geom_smooth(method = "lm", se =FALSE)
  
#Data Manipulation
order_value_tbl <- bike_orderlines_tbl %>%
  
  select(order_id, order_line, total_price, quantity)%>%
  
  group_by(order_id) %>%
  summarize(
    total_quantity = sum(quantity),
    total_price = sum (total_price)
    )    %>% 
      ungroup()
#Scatter Plot

order_value_tbl %>%
  ggplot(aes(x=total_quantity, y = total_price))+
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE)

#line plot

revenue_by_month_tbl <- bike_orderlines %>%
  
  select(order_date, total_price) %>%
  mutate(year_month = floor_date(order_date, "months") %>% ymd()) %>%
  
  group_by(year_month) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

revenue_by_month_tbl %>%
  ggplot(aes(year_month, revenue)) +
  geom_line(size=0.5, linetype = 1) +
  geom_smooth(method = "loess", span = 0.2)
# bar plot

revenue_by_category_2_tbl <- bike_orderlines_tbl %>%
  
  select(category_2, total_price) %>%
  
  group_by(category_2) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

revenue_by_category_2_tbl %>%
  
  mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%
  
  ggplot(aes(category_2, revenue)) +
  
  geom_col(fill = "#2c3e50") + 
  coord_flip()

#Histogramm plot

bike_orderlines_tbl %>%
  
  distinct(model, price) %>%
  
  ggplot(aes(price)) +
  
  geom_histogram(bins = 25, fill = "blue", color = "white")

bike_orderlines_tbl %>%
  
  distinct(price, model, frame_material) %>%
  
  ggplot(aes(price, fill = frame_material)) +
  geom_histogram() +
  
  facet_wrap(~ frame_material, ncol = 1)

# density

bike_orderlines_tbl %>%
  distinct(price, model, frame_material) %>%
  ggplot(aes(price, fill = frame_material)) +
  
  geom_density(alpha = 0.5) +
  
  theme(legend.position = "bottom")

#Box plot

unit_price_by_cat_2_tbl <- bike_orderlines_tbl %>%
  select(category_2, model, price) %>%
  distinct() %>%
  
  mutate(category_2 = as_factor(category_2) %>% fct_reorder(price))

# Box Plot
unit_price_by_cat_2_tbl %>%

  
  ggplot(aes(category_2, price)) +
  
  geom_boxplot() +
  coord_flip()

# violin Plot & Jitter Plot

unit_price_by_cat_2_tbl %>%
  
  ggplot(aes(category_2, price))+
  
  geom_jitter(width = 0.15, color = "#2c3e50") +
  geom_violin(alpha = 0.5) + 
  coord_flip()

# Text & Labels

revenue_by_year_tbl <- bike_orderlines %>%
  
  select(order_date, total_price) %>%
  
  mutate(year = year(order_date)) %>%
  
  group_by(year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# adding text to bar chart
# Filtering labels to highlight a point

revenue_by_year_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill= "#2c3e50") +
  geom_smooth(method = "lm", se = FALSE)+
  
  geom_text(aes(label = scales::dollar(revenue,
    scale = 1e-6,
    prefix = "",
    suffix = "M")),
    vjust = 1.5, color = "white") +
  
  geom_label(label = "mahor Demand This Year",
             vjust = -0.5,
             size = 5,
             fill = "#1f78b4",
             color = "white",
             fontface = "italic",
             data = revenue_by_year_tbl %>%
               filter(year %in% c(2019)))+
  expand_limits(y= 2e7)

# Formatting

sales_by_year_tbl %>%
  
#Canvas
  ggplot(aes(x=year, y =sales, color=sales))+
  geom_line(size=1)+
  geom_point(size=5)+
  geom_smooth(method = "lm", se = FALSE)

sales_by_year_tbl %>%
  
  #canvas
  ggplot(aes(x=year, y =sales, color = sales)) +
  
  #geometries
  geom_line(size=1)+
  geom_point(size=5)+
  geom_smooth(method = "lm", se = FALSE, color = "#d62dc6") +
  
  #Formating
  expand_limits(y=0)+
  #you can type black an red for scales
  scale_color_continuous(low = "#95E1EA", high = "#2097A3",
                         labels = scales::dollar_format(scale= 1/1e6,
                                                        prefix = "",
                                                        suffix =  "M Euro")) +
  scale_y_continuous(labels = scales::dollar_format(scale= 1/ 1e6,
                                                    prefix = "",
                                                    suffix =  "M Euro"))+
#change axis labels and title with labs()
  labs(
    title = "Revenue",
    subtitle =  "Sales are trending up and to the right!!",
    x= "",
    y= "sales (Million)",
    color = "Rev (M Euro)",
    caption = "What's happening?\nSales numbers showing year-over-year growth"
  )

#themes
library(ggthemes)
sales_by_month_2015 <- bike_orderlines_tbl %>%
  #select columns to focus on and adding month column
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  mutate(month = month(order_date)) %>%
  
  filter(year=="2015") %>%
  
  #Grouping by month, and summarize sales
  group_by(month) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup()%>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark    = ",",
                                     prefix          = "",  
                                     suffix          = " €"))

##Plotting
#canvas
sales_by_month_2015 %>%
  ggplot(aes(x=month, y = sales, color = sales)) +
           geom_line(size = 1)+
           geom_point(size = 5)+
           geom_smooth(method= "lm", se = FALSE)+
           
# formatting
  expand_limits(y=0)+
  scale_color_continuous(low = "red", high = "black",
                         labels = scales::dollar_format(scale = 1/1e6, 
                                                        prefix = "", 
                                                        suffix = "M €")) +
  scale_x_continuous(breaks = sales_by_month_2015$month,
                     labels = month(sales_by_month_2015$month, label = T)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M"))+
  labs(title =  "Monthly sales (2015)",
       subtitle = "April is the strongeth month!",
       x = "",
       y= "Sales (Million)",
       caption = "Whats up bro, at the End of the year")+
  theme_economist()+
  theme(legend.position =  "right",
        legend.direction = "vertical",
        axis.text.x = element_text(angle=45))

sales_by_year_category_1_tbl <- bike_orderlines_tbl %>%
  select(order_date, category_1, total_price) %>%
  
  mutate(order_date = ymd(order_date)) %>%
  mutate(year = year(order_date)) %>%
  
  group_by(category_1, year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup() %>%
  
  # Convert character vectors to factors
  # Arrange by year and revenue
  mutate(category_1 = fct_reorder2(category_1, year, revenue))


#colors for rgb
colors()

#Example
sales_by_year_category_1_tbl %>%
  ggplot(aes(year, revenue)) +
  geom_col(fill = "slateblue")

# to rgb
col2rgb("slateblue")
col2rgb("#2C3E50")

# To HEX (this function should be provided to a geom)
rgb(44, 62, 80, maxColorValue = 255)

### Brewer. Comes with basic R.
#Primarly for discrete data.

# We can use those palletes by just calling their names (e.g. "Blues")
# Display the colors
RColorBrewer::display.brewer.all() 
# Get information
RColorBrewer::brewer.pal.info
# Get the HEX codes
RColorBrewer::brewer.pal(n = 8, name = "Blues")[1]

# Example
sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = RColorBrewer::brewer.pal(n = 8, name = "Blues")[8])


### Viridis
viridisLite::viridis(n = 20)
# The last two characters indicate the transparency (e.g. FF makes it 100% transparent)

# Example
sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = viridisLite::viridis(n = 20)[10])

############### Aesthetic Mapping
sales_by_year_category_1_tbl %>%
  
  #Put the aes color mapping here, to apply it to geom_line and geom_point
  ggplot(aes(year,revenue, color = category_1)) +
  #or put the colour over there
  geom_line(size=1)+
  geom_point(color = "dodgerblue", size =5)

#plot with coloured bars
sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  geom_col(aes(fill = category_1)) 
# You could use color = ... to color the outlines

##size with point
sales_by_year_category_1_tbl %>%
ggplot(aes(year, revenue, size = revenue)) +
  #the local size overrides the global size
  geom_line(aes(color=category_1), size =1)+
  geom_point()

#####################Faceting
sales_by_year_category_1_tbl %>%
  ggplot(aes(year,revenue, color = category_1)) +
  geom_line(color="black") +
  geom_smooth(method = "lm", se = FALSE) +
  
  #Break out stacked plot
  facet_wrap(~ category_1, ncol = 3, scales = "free_y") +
  expand_limits(y = 0)

###Position Adjustments(Stack & Dodge)

sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year,revenue, fill= category_1)) +
  
  geom_col(position = position_dodge(width = 0.9), color = "black")

##### Stack Area %>%

sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year,revenue, fill= category_1)) +
  geom_area(color = "black")


######Scales (Colors, Fills, Axis)

g_facet_continuous  <- sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, color, color= revenue)) +
  geom_line(size =1)+
  geom_point(size=3)+
  
  facet_wrap(~ category_1, scales= "free_y")+
  expand_limits(y=0) +
  theme_minimal()

g_facet_continuous 

### discrete Farben

g_facet_discrete <- sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, color = category_1)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  
  facet_wrap(~ category_1, scales = "free_y") +
  expand_limits(y = 0) +
  
  theme_minimal()

g_facet_discrete


#### stacked area plot
g_area_discrete <- sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, fill = category_1)) +
  geom_area(color = "black") +
  
  theme_minimal()

g_area_discrete

#### color by revenue continious scale

g_facet_continuous +
  # scale_color_continuous(
  #     low   = "black",
  #     high  = "cornflowerblue"
  # )
  # This is basically like adding a theme
  scale_color_viridis_c(option = "E", direction = -1)

#color by category 1 discrete scales

RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info
RColorBrewer::display.brewer.pal(n=8, name = "Blues")

g_facet_discrete +
  scale_color_brewer(palette = "Set3")+
  theme_dark()

g_facet_discrete +
  scale_color_viridis_d(option = "D") +
  theme_dark()

#### Fill by Category 1
g_area_discrete +
  scale_fill_brewer(palette = "Set3")

g_area_discrete +
  scale_fill_viridis_d()


############Axis Scale

g_facet_continuous + 
  scale_x_continuous(breaks = seq(2015,2019, by = 2)) +
  scale_y_continuous(labels = scales::dollar_format(scale= 1e-6,
                                                    prefix = "",
                                                    suffix = "M"))

##########Labels

g_facet_continuous +
  scale_x_continuous(breaks = seq(2011,2015, by =2)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6,
                                                    suffix = "M"))+
  geom_smooth(method= "lm", se = FALSE) +
  scale_color_viridis_c() +
  theme_dark() +
  
  labs(
    title= "Bike Sales",
    subtitle = "Sales are trending up",
    caption = "5-year sales trends\nblablabla",
    x= "Year",
    y= "Revenue (M Euro)",
    color = "Revenue"
  )
