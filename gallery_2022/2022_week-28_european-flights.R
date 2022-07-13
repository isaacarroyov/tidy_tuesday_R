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
df %>%
  ggplot(aes(x=date_year_month, y=country, size = sum_movements_month, color = higher_than_avg_movement_2019)) +
  geom_point(alpha = 0.5) +
  scale_size_area(max_size = 8)
