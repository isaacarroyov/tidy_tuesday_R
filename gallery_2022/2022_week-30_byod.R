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
library(cowplot)

font_add_google("Quattrocento", "titleFont")
font_add_google("Quattrocento Sans", "bodyFont")
showtext_auto()

# LOAD DATA -> Points and yucatan region
firms_data <- geojson_sf("./data/2022/datos_firms_2001_2020.geojson")
region_yuc <- read_sf("./data/2022/31ent.shp")

# ------------------------------------------------------------------------------
# DATA PROCESSING 02
# 1 - Obtener los puntos de las coordenadas
firms_data_pos <- firms_data %>%
  as_tibble() %>%
  select(acq_date, confidence) %>%
  mutate(acq_date = stringr::str_replace(acq_date, "T00:00:00","")) %>%
  mutate(acq_date = lubridate::year(acq_date)) %>%
  bind_cols(firms_data %>%
              st_coordinates() %>%
              as_tibble() %>%
              rename(x_pos = X, y_pos = Y)
              ) %>%
  filter(confidence > 84)
  

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

# Overall view
overall_view <- firms_data_pos %>%
  ggplot(aes(x=x_pos, y=y_pos)) +
  stat_density_2d_filled(n=200, bins = 50, contour = T, size = 0) +
  scale_fill_manual(values = c("transparent", MetBrewer::met.brewer("OKeeffe2", n=49, direction = 1))) +
  labs(title = "Over all view:\nfrom 2001 to 2020") +
  theme(
    # Background
    plot.background = element_rect(fill = 'transparent', color = "transparent"),
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", family = "titleFont", size = 40, lineheight = 0.3, hjust = 0.5)
    )


p2 <- firms_data_pos %>%
  ggplot(aes(x=x_pos, y=y_pos)) +
  stat_density_2d_filled(n=200, bins = 50, contour = T, size = 0) +
  scale_fill_manual(values = c("#FEF8F0", MetBrewer::met.brewer("OKeeffe2", n=49, direction = 1))) +
  facet_wrap(~acq_date) +
  labs(title = "20 years of historical hotspots in Yucatan, Mexico",
       subtitle = "Thanks to the data provided by NASA's Fire Information for Resource Management System (FIRMS), we can see what had been the potential wildfires (confidence higher than 84%) density within Yucatan's territory between 2001 and 2020.<br><br>The overall view of these potential wildfires suggests a higher density in the southwest region (<em>right image</em>); however, looking at individual years (<em>grid from below</em>), the visualization showcases that in 2003, there was a much higher concentration of wildfires in the northern part of the state.",
       caption = "#TidyTuesday Week 30: BYOD (Bring Your Own Data)<br>Visualization by Isaac Arroyo (@unisaacarroyov on Twitter)<br>Data: NASA's Fire Information for Resource Management System (FIRMS)") +
  theme(
    # Background
    plot.background = element_rect(fill = '#FEF8F0', color = "#FEF8F0"),
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    # Title
    plot.title.position = "plot",
    plot.title = element_text(family = "titleFont", face = "bold", size = 80, margin = margin(10,0,0,0), hjust = 0.5),
    # Subtitle
    plot.subtitle = element_textbox_simple(family = "bodyFont", lineheight = 0.4, size = 41.5, width = unit(5, "in"), halign = 0, hjust = 0, margin = margin(t = 0.7,b = 0.25,r = 0, l = 0.25, unit = "cm")),
    # Strips
    strip.text = element_text(family = "titleFont", face = "bold", size = 40),
    # Caption
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(family = "bodyFont", lineheight = 0.4, face = "bold", size = 30, margin = margin(t = 0.5, b = 0.25, l = 0, r = 0, unit = "cm"))
    )


final <- ggdraw() +
  draw_plot(p2, 0,0,1,1) + 
  draw_plot(overall_view, 0.65, .71, 0.35, 0.23)
  
ggsave(filename = "./gallery_2022/2022_week-30_byod.png", plot = final, width = 8.5, height = 11, units = "in" )

# Visualization no elegida
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


# ··················································································
# DATA VISUALIZATION (Forma 1)
# p1 <- ggplot() +
#   geom_sf(data = firms_data_density, aes(fill=v), size = 0) +
#   MetBrewer::scale_fill_met_c("Derain", direction = -1) +
#   geom_sf(data = st_boundary(region_yuc), size = 0.3, color = 'black') +
#   coord_sf(ylim = c(22.3e5, 24.6e5), xlim = c(-100.5e5, -97.5e5))
