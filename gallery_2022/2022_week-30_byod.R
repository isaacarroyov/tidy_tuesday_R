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

# DATA-VIS SETTINGS
theme_set(theme_minimal())
theme_update(
  legend.position = "none",
  panel.grid = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
)


# DATA VISUALIZATION (Forma 1)
ggplot() +
  geom_sf(data = firms_data_density, aes(fill=v), size = 0) +
  scale_fill_met_c("Derain", direction = -1) +
  geom_sf(data = st_boundary(region_yuc), size = 0.3, color = 'black') +
  coord_sf(ylim = c(22.3e5, 24.6e5), xlim = c(-100.5e5, -97.5e5))


# Data processing (Forma 2)
# 1 - Obtener los puntos de las coordenadas
firms_data_pos <- firms_data %>%
  st_coordinates() %>% 
  as_tibble() %>%
  rename(x_pos = X, y_pos = Y)

# DATA VISUALIZATION
firms_data_pos %>%
  ggplot(aes(x=x_pos, y=y_pos)) +
  stat_density_2d_filled(n=75) +
  scale_fill_manual(values = c("transparent", met.brewer("Derain", n=13, direction = -1)))


firms_data_pos %>%
  ggplot(aes(x=x_pos,y=y_pos)) + 
  stat_density_2d(geom = "raster",
                  aes(fill = after_stat(density)),
                  contour = F) +
  scale_fill_met_c("Derain", direction = -1)









