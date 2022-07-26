# Week 30: BYOD (Bring Your Own Data) -> Redesigning Data Visualizations: Undergrad Thesis
library(dplyr)
library(sf)
sf_use_s2(FALSE)
library(geojsonsf)
library(ggplot2)

# LOAD DATA -> Points and yucatan region
firms_data <- geojson_sf("./data/2022/datos_firms_2001_2020.geojson")
region_yuc <- read_sf("./data/2022/31ent.shp")

# DENSITY POINTS



# DATA VISUALIZATION