# Week 28 - European Flights
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggtext)
library(MetBrewer)

# Load data
data <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv")
data <- data %>% as_tibble()
data <- data %>%
  mutate(FLT_DATE = ymd(stringr::str_replace_all(FLT_DATE, "T00:00:00Z", "")))

# Select variables
data_01 <- data %>%
  select(STATE_NAME,
         FLT_DATE, FLT_TOT_1) %>%
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
  mutate(higher_than_avg_movement_2019 = case_when(country == avg_movements_2019[1,1] & sum_movements_month > avg_movements_2019[1,2] ~ "Higher",
                                                   country == avg_movements_2019[2,1] & sum_movements_month > avg_movements_2019[2,2] ~ "Higher",
                                                   country == avg_movements_2019[3,1] & sum_movements_month > avg_movements_2019[3,2] ~ "Higher",
                                                   country == avg_movements_2019[4,1] & sum_movements_month > avg_movements_2019[4,2] ~ "Higher",
                                                   country == avg_movements_2019[5,1] & sum_movements_month > avg_movements_2019[5,2] ~ "Higher",
                                                   country == avg_movements_2019[6,1] & sum_movements_month > avg_movements_2019[6,2] ~ "Higher",
                                                   TRUE ~ "Lower"))

# Data Visualization
df %>%
  ggplot(aes(x=date_year_month, y=1, size = sum_movements_month, color = higher_than_avg_movement_2019)) +
  geom_vline(aes(xintercept = as.Date("2020-03-01")), size = 0.5) +
  geom_point(alpha = 0.75) +
  scale_size_area(max_size = 20, labels = scales::label_comma()) + 
  #scale_colour_manual(values = met.brewer("Cassatt1",2)) +
  guides(color="none",
         size = guide_legend(title = "_**Number of movements**<br>(arrivals + departures)_", title.position = "top",
                             label.position = "bottom",
                             override.aes = list(alpha=1)),
         ) +
  facet_wrap(~country, nrow = 6) +
  coord_cartesian(clip = "off") +
  labs(title = "Airlines are still recovering from the events caused by COVID-19",
       subtitle = "Description",
       caption = "#TidyTuesday Week 28: European Flights<br>Data: Eurocontrol<br>Visualization by Isaac Arroyo (@unisaacarroyov)"
       ) +
  theme_minimal() + 
  theme(
    # Background plot
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    plot.background = element_rect(fill = "white", colour = "transparent"),
    # Facets
    strip.text = element_text(face = "bold", hjust = 0),
    # Legend
    legend.position = "top",
    legend.title = element_markdown(),
    legend.key.width = unit(7,"lines"),
    # Title
    plot.title = element_textbox_simple(face = "bold"),
    plot.title.position = "plot",
    # Subtitle
    plot.subtitle = element_textbox_simple(),
    # Caption
    plot.caption = element_textbox_simple(face = "bold"),
    plot.caption.position = "plot",
    # Axes
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    # Grid
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(colour = 'grey60', size = 0.25, linetype = "dashed")
  )

# Save plot
ggsave("./gallery_2022/2022_week-28_european-flights.png",
       width = 11, height = 8.5, units = "in", dpi = 300)
