# Week 28 - European Flights
library(dplyr)
library(forcats)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggtext)
library(MetBrewer)

# Load data
data <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv")
data <- data%>%as_tibble()

# Data Exploration
data <- data %>%
  mutate(YEAR = as.factor(YEAR),
         MONTH_MON = factor(MONTH_MON, levels = unique(data$MONTH_MON)),
         APT_ICAO = as.factor(APT_ICAO),
         APT_NAME = as.factor(APT_NAME),
         Pivot.Label = as.factor(Pivot.Label),
         STATE_NAME = as.factor(STATE_NAME))

