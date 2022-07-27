# Week 30: BYOD (Bring Your Own Data) -> Redesigning Data Visualizations: Undergrad Thesis
library(dplyr)
library(sf)
sf_use_s2(FALSE)
library(geojsonsf)
# library(spatstat)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)
library(rayshader)

# LOAD DATA -> Points and yucatan region
firms_data <- geojson_sf("./data/2022/datos_firms_2001_2020.geojson") %>%
  select(geometry, acq_date)
region_yuc <- read_sf("./data/2022/31ent.shp")

# ------------------------------------------------------------------------------
# # DATA PROCESSING 01 (from https://stackoverflow.com/questions/68643517/smoothed-density-maps-for-points-in-using-sf-within-a-given-boundary-in-r)
# # 1- Cambiar a proyeccion Mercantor (crs = 3857) ambos mapas
# firms_data <- firms_data %>%
#   st_transform(crs = 3857)
# region_yuc <- region_yuc %>%
#   st_transform(crs = 3857)
# # 2 - Transformar firms_data a ppp usando spatstat
# firms_data_ppp <- as.ppp(firms_data)
# # 3 - Asignarle un Window. El window sera el estado de Yucatan
# Window(firms_data_ppp) <- as.owin(region_yuc)
# # 4 - Suavizar puntos para crear el density plot.
# # 5 - De los puntos suavizados crear un objeto stars (raster)
# # 6 - Y de objecto stars a sf
# # 7 - Dar la proyeccion mercantor
# firms_data_density <- firms_data_ppp %>%
#   density.ppp() %>%
#   stars::st_as_stars() %>%
#   st_as_sf() %>%
#   st_set_crs(3857)

# ------------------------------------------------------------------------------
# DATA PROCESSING 02
# 1 - Obtener los puntos de las coordenadas
firms_data_pos <- firms_data %>%
  as_tibble() %>%
  select(acq_date) %>%
  mutate(acq_date = stringr::str_replace(acq_date, "T00:00:00","")) %>%
  mutate(acq_date = lubridate::year(acq_date)) %>%
  bind_cols(firms_data %>%
              st_coordinates() %>%
              as_tibble() %>%
              rename(x_pos = X, y_pos = Y)
              )
  

# ------------------------------------------------------------------------------
# DATA VISUALIZATION -> DENSTY PLOT

# DATA-VIS SETTINGS
theme_set(theme_minimal())
theme_update(
  legend.position = "none",
  panel.grid = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
)

# ··················································································
# DATA VISUALIZATION (Forma 1)
# p1 <- ggplot() +
#   geom_sf(data = firms_data_density, aes(fill=v), size = 0) +
#   MetBrewer::scale_fill_met_c("Derain", direction = -1) +
#   geom_sf(data = st_boundary(region_yuc), size = 0.3, color = 'black') +
#   coord_sf(ylim = c(22.3e5, 24.6e5), xlim = c(-100.5e5, -97.5e5))

# ··················································································
# DATA VISUALIZATION (Forma 2)
firms_data_pos %>%
  ggplot(aes(x=x_pos, y=y_pos)) +
  stat_density_2d_filled(n=200, bins = 50, contour = T, size = 0) +
  #scale_fill_manual(values = c("white", NatParksPalettes::natparks.pals("Arches2", 49, direction = -1)))
  scale_fill_manual(values = c("#FEF8F0", MetBrewer::met.brewer("OKeeffe2", n=49, direction = 1))) +
  labs(title = "Hotspot accumulation in Yucatan from 2001 to 2020",
       subtitle = "",
       caption = "Visualization by Isaac Arroyo<br>Data: NASA's Fire Information for Resource Management System (FIRMS)") +
  theme(
    # Background
    plot.background = element_rect(fill = '#FEF8F0', color = "#FEF8F0"),
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    # Title
    plot.title.position = "plot",
    # Subtitle
    plot.subtitle = element_markdown(),
    # Caption
    plot.caption.position = "plot",
    plot.caption = element_markdown(),
  )


# ··················································································
# TODO -> Enhance other day 
# firms_data_pos %>%
#   ggplot(aes(x=x_pos,y=y_pos)) + 
#   stat_density_2d(geom = "raster",
#                   aes(fill = after_stat(density)),
#                   contour = F) +
#   scale_fill_met_c("Derain", direction = -1)






