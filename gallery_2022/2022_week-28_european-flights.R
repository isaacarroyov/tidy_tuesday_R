# Week 28 - European Flights
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggtext)
library(MetBrewer)
library(sysfonts)
library(showtext)

font_add_google("Philosopher", "titleFont")
font_add_google("Mulish", "bodyFont")
showtext_auto()

# Load data
data <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv")
data <- data %>% as_tibble()
data <- data %>%
  mutate(FLT_DATE = ymd(stringr::str_replace_all(FLT_DATE, "T00:00:00Z", "")))

# Select variables
data_01 <- data %>%
  select(STATE_NAME, FLT_DATE, FLT_TOT_1) %>%
  rename(country = STATE_NAME, 
         full_date = FLT_DATE,
         total_number_movements = FLT_TOT_1)

# Step 1 - Filtrar date_year == 2019
# Step 2 - Calcular la cuenta de movimientos en cada pais 
# Step 3 - Encontrar los 6 paises con mayor numero de movimientos en 2019

popular_countries <- data_01 %>%
  filter(year(full_date) == 2019) %>%
  group_by(country) %>%
  summarise(number_movements = sum(total_number_movements),
            .groups = "drop") %>%
  arrange(-number_movements) %>%
  head(6) %>%
  pull(country)

# Step 4 - Filtrar el conjunto de datos con los nombres de paises
# Step 5 - agrupar por paises y por mes 
# Step 6 - Filter por fecha >= Enero 2018
data_02 <- data_01 %>%
  filter(country %in% popular_countries,) %>%
  group_by(country, date_year_month = floor_date(full_date, "month")) %>%
  summarise(sum_movements_month = sum(total_number_movements), .groups = "drop") %>%
  filter(date_year_month >= as.Date("2019-01-01"))

# Step 7 - encontrar movimientos promedios del 2019
avg_movements_2019 <- data_02 %>%
  filter(year(date_year_month)==2019) %>%
  group_by(country) %>%
  summarise(avg_movements_year_2019 = mean(sum_movements_month),
            .groups = "drop") %>% as.data.frame()
  

# Step 8 - Crear variable higher_than_avg_movement_2019
df <- data_02 %>%
  mutate(higher_than_avg_movement_2019 = case_when(year(date_year_month) == 2019 ~ "Nothing",
                                                   country == avg_movements_2019[1,1] & sum_movements_month > avg_movements_2019[1,2] ~ "Higher",
                                                   country == avg_movements_2019[2,1] & sum_movements_month > avg_movements_2019[2,2] ~ "Higher",
                                                   country == avg_movements_2019[3,1] & sum_movements_month > avg_movements_2019[3,2] ~ "Higher",
                                                   country == avg_movements_2019[4,1] & sum_movements_month > avg_movements_2019[4,2] ~ "Higher",
                                                   country == avg_movements_2019[5,1] & sum_movements_month > avg_movements_2019[5,2] ~ "Higher",
                                                   country == avg_movements_2019[6,1] & sum_movements_month > avg_movements_2019[6,2] ~ "Higher",
                                                   TRUE ~ "Lower"))

# Data Visualization
df %>%
  ggplot(aes(x=date_year_month, y=1,
             size = sum_movements_month,
             label = sum_movements_month,
             color = higher_than_avg_movement_2019)) +
  geom_point(alpha = 0.75) +
  geom_vline(aes(xintercept = as.Date("2020-03-01")), size = 1) +
  scale_size_area(max_size = 13, labels = scales::label_comma()) + 
  scale_colour_manual(breaks = c("Nothing","Higher","Lower") ,values = c("#924F5A","#DD5540","#BABD89")) +
  guides(color="none", size = "none") +
  facet_wrap(~country, nrow = 6) +
  coord_cartesian(clip = "off") +
  labs(title = "European flights and the events caused by COVID-19",
       subtitle = "<span style='font-size:60px'><b>How do you read the data visualization?</b></span><br>• Each circle represents a month of the year (i.e. June 2019, January 2020 or May 2022), and they are ordered chronologically, starting January 2019 and ending May 2022.<br>• The <b>size</b> of the circle represents the <b>country's total number of flights (arrivals + departures) of the month</b>.<br>• The colour represents whether the number of flights is <span style='color:#DD5540'><b>higher</b></span> than the <b>country's 2019 average</b> or <span style='color:#808349'><b>lower</b></span>. The <b>2019 flights</b> are <span style='color:#924F5A'><b>coloured in the same way</b></span>.<br>• The <b>black line is in March 2020</b>, when most nations started a lockdown.<br><br><span style='font-size:60px'><b>Comments concerning what's displayed</b></span>.<br>The visualization shows the evolution of the number of flights (arrivals + departures) over time. It's clear when the first fall of flights is and how these countries have been struggling recently. Half of them (**France**, **Germany** and the **UK**) **have not yet surpassed their respective country's 2019 average number of flights**. On the other side, the other half have **reached a higher number than their 2019 average**: **Türkiye in July and August 2021, Spain and Italy in May 2021**.",
       caption = "#TidyTuesday Week 28: European Flights<br>Data: Eurocontrol<br>Visualization by Isaac Arroyo (@unisaacarroyov)"
       ) +
  theme_minimal() + 
  theme(
    # Background plot
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    plot.background = element_rect(fill = "white", colour = "transparent"),
    # Facets
    strip.text = element_text(colour = "black", face = "bold", hjust = 0, family = "titleFont", size = 45),
    # Title
    plot.title = element_textbox_simple(colour = "black", face = "bold", family = "titleFont", lineheight = 0.1, size = 90, margin = margin(5,15,0,5)),
    plot.title.position = "plot",
    # Subtitle
    plot.subtitle = element_textbox_simple(colour = "black", family = "bodyFont", lineheight = 0.33, size = 40, margin = margin(5,15,20,5)),
    # Caption
    plot.caption = element_textbox_simple(colour = "black", face = "bold", family = "bodyFont", lineheight = 0.33, size = 30, halign = 0.5, margin = margin(20,0,5,0)),
    plot.caption.position = "plot",
    # Axes
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "bodyFont", face = "italic", colour = "grey20", size = 35, margin = margin(5,0,0,0)),
    # Grid
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(colour = 'grey20', size = 0.35, linetype = "dashed")
  )

# Save plot
ggsave("./gallery_2022/2022_week-28_european-flights.png",
       width = 11, height = 8.5, units = "in", dpi = 300)