# TidyTuesday Week 38: Wastewater plants
library(tidyverse)
library(ggtext)
# devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(sf)
library(geojsonsf)

# LOAD DATA
HydroWASTE_v10 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv')
sf_mex_states <- geojson_sf("https://raw.githubusercontent.com/isaacarroyov/data_visualization_practice/master/data/R_vector_conafor_wildfires_2017_states_clean_en_2022-03-03.geojson") %>%
  select(State, geometry) %>%
  rename(state_mex = State)

# DESARROLLO DE IDEA
# Para esta visualizacion, lo importante va a ser mostrar la cantidad de WWTP en Mexico
# y señalar las que no cumplen con el DF mínimo. 

# Separar dos conjuntos de datos
# WWTP
sf_wwtp_mex <- HydroWASTE_v10 %>%
  filter(COUNTRY == "Mexico") %>%
  select(LAT_WWTP, LON_WWTP, WWTP_NAME, DF) %>%
  rename(dilusion_factor = DF) %>%
  st_as_sf(coords = c("LON_WWTP","LAT_WWTP"),
           crs = st_crs(4326))


# ------ DATA WRANGLING ------
# ------ Unir puntos a los respectivos estados ------
df_wwtp_mex <- sf_wwtp_mex %>%
  st_join(sf_mex_states) %>%
  as_tibble() %>%
  select(WWTP_NAME, state_mex, dilusion_factor) %>%
  filter(!is.na(state_mex)) %>%
  mutate(state_mex = case_when(state_mex == "México" ~ "Estado de México",
                               state_mex == "Ciudad de México" ~ "CDMX",
                               T ~ state_mex),
         state_mex = factor(state_mex))

# ------ Categorizar de acuerdo a su cumplen el minimo dilusion_factor ------
df_wwtp_mex_cat <- df_wwtp_mex %>%
  mutate(pais = 'México', # Agregar primer nodo del sankey
         cumple_min_dilusion_factor = if_else(condition = dilusion_factor > 10,
                                              true = 1,
                                              false = 2,
                                              missing = 3),
         cumple_min_dilusion_factor = ordered(cumple_min_dilusion_factor, 
                                              levels = c(1,2,3),
                                              labels = c("% de plantas que si cumplen el mínimo de acuerdo con la EMA",
                                                         "% de plantas que no cumplen el mínimo de acuerdo con la EMA",
                                                         "% de plantas de las que no se tiene información")))


# Encontrar proporciones
df_wwtp_mex_cat_perc <- df_wwtp_mex_cat %>%
  group_by(state_mex, cumple_min_dilusion_factor) %>%
  summarise(freq = n()) %>% ungroup(cumple_min_dilusion_factor) %>%
  mutate(perc_del_total = (freq/sum(freq)) * 100) %>% ungroup() 


# ------ DATA VISUALIZATION ------
df_wwtp_mex_cat_perc_dataviz <- df_wwtp_mex_cat_perc %>%
  mutate(state_mex = fct_reorder(state_mex, # Ordenar eje y por numero de wwtp
                                 freq, sum)) %>% 
  # Mostrar mas informacion -> cuantas wwtp del total no cumplen/no hay info
  group_by(state_mex) %>%
  mutate(del_total = sum(freq)) %>% ungroup() %>%
  mutate(more_info = if_else(cumple_min_dilusion_factor != "% de plantas que si cumplen el mínimo de acuerdo con la EMA",
                             true = glue::glue("{round(perc_del_total,2)}%, es decir {freq} de {del_total}"),
                             false = NA_character_,
                             missing = NA_character_))

p1 <- df_wwtp_mex_cat_perc_dataviz %>%
  ggplot(aes(y = state_mex, x = perc_del_total,
             fill = cumple_min_dilusion_factor,
             color = cumple_min_dilusion_factor,
             label = more_info)) + 
  geom_col() +
  geom_textbox(size = 2, 
               width = unit(50, "pt"), 
               box.padding = unit(rep(2.5,4),"pt"),
               box.r = unit(3,"pt"),
               fill = 'white',
               nudge_x = 5,
               hjust = 0
               ) +
  # geom_vline(xintercept = 150, colour = 'black') +
  facet_wrap(~cumple_min_dilusion_factor,
             labeller = labeller(cumple_min_dilusion_factor = label_wrap_gen(25)) #TIL (Today I Learned)
             ) +
  coord_cartesian(clip = "off", xlim = c(0,140)) +
  scale_x_continuous(breaks = seq(25,100,by=25),
                     labels = scales::label_percent(scale = 1)) + 
  labs(title = "¿Cómo se trata el agua?",
       subtitle = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer in dui malesuada, ornare mi non, pharetra ex. Maecenas porttitor, enim quis viverra pellentesque, magna diam luctus odio, at sollicitudin elit massa ac sapien. Nam a egestas purus, aliquet vestibulum neque. Vivamus fringilla felis at mattis pharetra. Quisque vitae ex et.",
       caption = "_**Nota**: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed placerat posuere ipsum, in facilisis magna luctus eu. Maecenas faucibus sapien._ <br><br>Diseño por Isaac Arroyo (@unisaacarroyov en twitter)<br>#TidyTuesday Week 38: Hydro Wastewater plants") +
  theme(
    # Legend
    legend.position = "none",
    # background
    panel.background = element_rect(fill = 'transparent'),
    # grid
    panel.grid = element_blank(),
    # Title
    plot.title.position = "plot",
    plot.title = element_textbox(size = 20,
                                 width = unit(7.5,"in")),
    # Subtitle
    plot.subtitle = element_textbox(width = unit(7.5,"in")),
    # Caption
    plot.caption.position = "plot",
    plot.caption = element_textbox(hjust = 0,
                                   unit(7.5,"in")),
    # Facets
    strip.text = element_text(size = 10, hjust = 0),
    strip.background = element_rect(fill = 'transparent', color = 'transparent'),
    # Axes
    axis.title = element_blank(),
    axis.text.y = element_text(face = 'bold'),
    axis.ticks.length = unit(0,"in"),
  )

ggsave(filename = "./gallery_2022/2022_week-38_hydro-wwtp.png",
       plot = p1,
       width = 8.5, height = 11, units = "in")

# ------ Data Art ------
# df_wwtp_mex %>%
#   mutate(pais = "Todo",
#          env_min = if_else(condition = dilusion_factor < 10,
#                            true = "No cumple",
#                            false = "Cumple",
#                            missing = "No hay información"),
#          state_no_cumple = if_else(condition = env_min %in% c("No cumple","No hay información"),
#                                    true = State,
#                                    false = NA_character_,
#                                    missing = NA_character_)) %>%
#   make_long(pais, State, env_min) %>%
#   ggplot(aes(x = x, next_x = next_x,
#              node = node, next_node = next_node)) +
#   geom_sankey(aes(fill = node)) +
#   theme(
#     legend.position = "none"
#   )



