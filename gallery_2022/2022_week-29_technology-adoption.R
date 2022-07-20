# Week 29 
library(dplyr)
# library(forcats)
# library(lubridate)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)

# font_add_google("", "titleFont")
# font_add_google("", "bodyFont")
# showtext_auto()

# LOAD DATA
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')
data <- data %>%
  mutate(sector = factor(category),
         type_sector = factor(group),
         variable_name = factor(variable),
         variable_label = factor(label),
         country_code = factor(iso3c)) %>%
  rename(date_year = year) %>%
  select(country_code, variable_name, variable_label,
         sector, type_sector, date_year, value)

# Filter date and variables of interest
# TODO: Read the article






