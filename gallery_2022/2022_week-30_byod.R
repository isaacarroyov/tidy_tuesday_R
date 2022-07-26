# Week 30: BYOD (Bring Your Own Data) -> Redesigning Data Visualizations: Undergrad Thesis
library(dplyr)
library(sf)
sf_use_s2(FALSE)
library(geojsonsf)
library(spatstat)
library(ggplot2)
library(NatParksPalettes) # devtools::install_github("kevinsblake/NatParksPalettes")
library(MetBrewer)

# LOAD DATA -> Points and yucatan region
firms_data <- geojson_sf("./data/2022/datos_firms_2001_2020.geojson") %>% select(geometry)
region_yuc <- read_sf("./data/2022/31ent.shp")

# DENSITY POINTS (FORMA 1)
# 1- Cambiar a proyeccion Mercantor (crs = 3857) ambos mapas
firms_data <- firms_data %>%
  st_transform(crs = 3857)
region_yuc <- region_yuc %>%
  st_transform(crs = 3857)

# 2 - Transformar firms_data a ppp usando spatstat
firms_data_ppp <- as.ppp(firms_data)

# 3 - Asignarle un Window. El window sera el estado de Yucatan
Window(firms_data_ppp) <- as.owin(region_yuc)

# 4 - Suavizar puntos para crear el density plot.
# 5 - De los puntos suavizados crear un objeto stars (raster)
# 6 - Y de objecto stars a sf
# 7 - Dar la proyeccion mercantor
firms_data_density <- firms_data_ppp %>%
  density.ppp() %>%
  stars::st_as_stars() %>%
  st_as_sf() %>%
  st_set_crs(3857)

# DATA VISUALIZATION (Forma 1)

# Data processing (Forma 2)

