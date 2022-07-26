# Week 30: BYOD (Bring Your Own Data) -> Redesigning Data Visualizations: Undergrad Thesis
library(dplyr)
library(sf)
sf_use_s2(FALSE)
library(geojsonsf)
library(ggplot2)
library(ggtext)
library(MetBrewer)

# LOAD DATA
firms_data <- geojson_sf("./data/2022/datos_firms_2001_2020.geojson")

firms_data %>%
  st_coordinates() %>%
  as_tibble() %>%
  ggplot(aes(x=X,y=Y)) +
  stat_density2d_filled(contour = T, n = 100) +
  scale_fill_manual(values = c("transparent",met.brewer("Morgenstern",8))) +
  theme_minimal() + 
  theme(
    legend.position = "none"
  )
  

