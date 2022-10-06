# TidyTuesday Week 38: Wastewater plants
library(tidyverse)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)
library(patchwork)
# devtools::install_github("davidsjoberg/ggsankey")
# library(ggsankey)
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
                                              missing = 3))


# Encontrar proporciones
df_wwtp_mex_cat_perc <- df_wwtp_mex_cat %>%
  group_by(state_mex, cumple_min_dilusion_factor) %>%
  summarise(freq = n()) %>% ungroup(cumple_min_dilusion_factor) %>%
  mutate(perc_del_total = (freq/sum(freq)) * 100) %>% ungroup() %>%
  mutate(cumple_min_dilusion_factor_en = ordered(cumple_min_dilusion_factor, 
                                                 levels = c(1,2,3),
                                                 labels = c("% of WWTP that meet the minimum DF according to the EMA",
                                                            "% of WWTP that do not meet the minimum DF according to the EMA",
                                                            "% of WWTP that do not have information related to the DF")),
         cumple_min_dilusion_factor_es = ordered(cumple_min_dilusion_factor, 
                                                 levels = c(1,2,3),
                                                 labels = c("% de PTAR que cumplen el mínimo DF de acuerdo con la EMA",
                                                            "% de PTAR que no cumplen el mínimo DF de acuerdo con la EMA",
                                                            "% de PTAR de las que no se tiene información")))


# ------ DATA VISUALIZATION ------
# ------ Data wrangling especially for the visualization ------
df_wwtp_mex_cat_perc_dataviz <- df_wwtp_mex_cat_perc %>%
  mutate(state_mex = fct_reorder(state_mex, # Ordenar eje y por numero de wwtp
                                 freq, sum)) %>% 
  # Mostrar mas informacion -> cuantas wwtp del total no cumplen/no hay info
  group_by(state_mex) %>%
  mutate(del_total = sum(freq)) %>% ungroup() %>%
  mutate(more_info_es = if_else(cumple_min_dilusion_factor_es != "% de PTAR que cumplen el mínimo DF de acuerdo con la EMA",
                                true = glue::glue("<b>{round(perc_del_total,2)}%, es decir {freq} de {del_total}</b>"),
                                false = NA_character_,
                                missing = NA_character_),
         more_info_en = if_else(cumple_min_dilusion_factor_en != "% of WWTP that meet the minimum DF according to the EMA",
                                true = glue::glue("<b>{round(perc_del_total,2)}%, meaning {freq} out of {del_total}</b>"),
                                false = NA_character_,
                                missing = NA_character_))

# ------ Title, subtitle and caption texts ------
title_text_en <- "Dilution Factor in Mexican wastewater treatment plants (WWTPs)"
subtitle_text_en <- "The <b>dilution factor (DF)</b> is the ratio between the natural discharge of the receiving waterbody and the WWTP effluent discharge, which has been used to determine ecological risks originating from WWTPs. Dilution factors have been used to predict potential exposure to down-the-drain chemicals from population density, which at a regional level, can help prevent negative effects by identifying zones of high contaminant concentrations. <b>The minimum dilution factor recommended by the European Medicines Agency (EMA) for environmental risk assessments of medicinal products for human use is 10</b>.<br><br>The data visualization shows the percentage of WWTPs for each Mexican state that...<br>&bull; <span style='color:#696E7C'><b>meet the minimum DF according to the EMA</b></span><br>&bull; <span style='color:#6D7128'><b>do not meet the minimum DF according to the EMA</b></span><br>&bull; <span style='color:#006475'><b>do not have information related to the DF</b></span><br><b>Focusing more on the last two categories.</b>"
caption_text_en <- 'Designed by Isaac Arroyo (@unisaacarroyov on twitter)<br>#TidyTuesday Week 38: Hydro Wastewater plants<br>Data provided by Ehalt Macedo, H., Lehner, B., Nicell, J., Grill, G., Li, J., Limtong, A., and Shakya, R. in their article _"Distribution and characteristics of wastewater treatment plants within the global river network"_'

title_text_es <- "Factor de dilución en las plantas de tratamiento de aguas residuales (PTAR) mexicanas"
subtitle_text_es <- "El <b>factor de dilución (DF)</b> es la relación entre la descarga natural del cuerpo de agua receptor y la descarga del efluente de la PTAR, que se ha utilizado para determinar los riesgos ecológicos originados por las PTAR. Los factores de dilución se han utilizado para predecir la exposición potencial a las sustancias químicas del desagüe a partir de la densidad de población, lo que, a nivel regional, puede ayudar a prevenir los efectos negativos mediante la identificación de zonas de altas concentraciones de contaminantes. El <b>factor de dilución mínimo recomendado por la Agencia Europea de Medicamentos (EMA) para la evaluación del riesgo medioambiental de los medicamentos de uso humano es de 10</b>.<br><br>La visualización de datos muestra el porcentaje de PTAR de cada estado mexicano que...<br>&bull; <span style='color:#696E7C'><b>cumplen el DF mínimo según la EMA</b></span><br>&bull; <span style='color:#6D7128'><b>no cumplen con los DF mínimos según la EMA</b></span><br>&bull; <span style='color:#006475'><b>no hay información relacinada al DF</b></span><br><b>Centrándose más en las dos últimas categorías</b>."
caption_text_es <- 'Diseño por Isaac Arroyo (@unisaacarroyov en twitter)<br>#TidyTuesday Week 38: Hydro Wastewater plants<br>Datos por Ehalt Macedo, H., Lehner, B., Nicell, J., Grill, G., Li, J., Limtong, A., y Shakya, R. en su artículo _"Distribution and characteristics of wastewater treatment plants within the global river network"_'

# ------ Typography ------
font_add_google("Oswald","title_font")
font_add_google("Quicksand","body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Colour palette ------
colour_palette <- c(NatParksPalettes::natparks.pals(name = "Banff", n = 8, direction = -1))[c(1,4,8)]

# Setting up theme
theme_update(
  # Legend
  legend.position = "none",
  # background
  panel.background = element_rect(fill = 'transparent', color = 'transparent'),
  plot.background = element_rect(fill = "#FFFBF5", colour = "#FFFBF5"),
  # grid
  panel.grid = element_blank(),
  panel.grid.major.x = element_line(colour = 'gray50',
                                    size = 0.05,
                                    lineend = 'round',
                                    linetype = 'dashed'),
  # Title
  plot.title.position = "plot",
  plot.title = element_textbox(family = title_font,
                               face = 'bold',
                               size = rel(6),
                               lineheight = 0.3,
                               width = unit(7.5,"in")),
  # Subtitle
  plot.subtitle = element_textbox(family = body_font,
                                  size = rel(3),
                                  lineheight = 0.3,
                                  width = unit(7.5,"in")),
  # Caption
  plot.caption.position = "plot",
  plot.caption = element_textbox(family = body_font,
                                 size = rel(2),
                                 lineheight = 0.3,
                                 hjust = 0,
                                 width= unit(7.5,"in")),
  # Facets
  strip.text = element_text(family = title_font,
                            face = 'bold',
                            size = rel(2.5),
                            lineheight = 0.3,
                            hjust = 0),
  strip.background = element_rect(fill = 'transparent', color = 'transparent'),
  # Axes
  axis.title = element_blank(),
  axis.text.y = element_text(family = body_font,
                             size = rel(3),
                             lineheight = 0.1,
                             face = 'bold'),
  axis.text.x = element_text(family = body_font,
                             size = rel(2.65),
                             lineheight = 0.1),
  axis.ticks.length = unit(0,"in"),
)


# ------ DataViz english ------
p1 <- df_wwtp_mex_cat_perc_dataviz %>%
  ggplot(aes(y = state_mex, x = perc_del_total,
             fill = cumple_min_dilusion_factor_en,
             color = cumple_min_dilusion_factor_en,
             label = more_info_en)) + 
  geom_col() +
  geom_textbox(family = body_font,
               size = rel(5.5),
               lineheight = 0.3,
               width = unit(50, "pt"), 
               box.padding = unit(rep(2.5,4),"pt"),
               box.r = unit(3,"pt"),
               fill = 'white',
               nudge_x = 5,
               hjust = 0) +
  facet_wrap(~cumple_min_dilusion_factor_en,
             labeller = labeller(cumple_min_dilusion_factor_en = label_wrap_gen(25)) #TIL (Today I Learned)
             ) +
  coord_cartesian(clip = "off", xlim = c(0,140)) +
  scale_x_continuous(breaks = seq(25,100,by=25),
                     labels = scales::label_percent(scale = 1)) +
  scale_fill_manual(values = colour_palette) +
  scale_colour_manual(values = colour_palette) +
  labs(title = title_text_en,
       subtitle = subtitle_text_en,
       caption = caption_text_en)

ggsave(filename = "./gallery_2022/2022_week-38_hydro-wwtp.png",
       plot = p1,
       width = 8.5, height = 11, units = "in")

# ------ DataViz Español ------
p2 <- df_wwtp_mex_cat_perc_dataviz %>%
  ggplot(aes(y = state_mex, x = perc_del_total,
             fill = cumple_min_dilusion_factor_es,
             color = cumple_min_dilusion_factor_es,
             label = more_info_es)) + 
  geom_col() +
  geom_textbox(family = body_font,
               size = rel(5.5),
               lineheight = 0.3,
               width = unit(50, "pt"), 
               box.padding = unit(rep(2.5,4),"pt"),
               box.r = unit(3,"pt"),
               fill = 'white',
               nudge_x = 5,
               hjust = 0) +
  # geom_vline(xintercept = 150, colour = 'black') +
  facet_wrap(~cumple_min_dilusion_factor_es,
             labeller = labeller(cumple_min_dilusion_factor_es = label_wrap_gen(25)) #TIL (Today I Learned)
  ) +
  coord_cartesian(clip = "off", xlim = c(0,140)) +
  scale_x_continuous(breaks = seq(25,100,by=25),
                     labels = scales::label_percent(scale = 1)) + 
  scale_fill_manual(values = colour_palette) +
  scale_colour_manual(values = colour_palette) +
  labs(title = title_text_es,
       subtitle = subtitle_text_es,
       caption = caption_text_es)


ggsave(filename = "./gallery_2022/2022_week-38_hydro-wwtp_es.png",
       plot = p2,
       width = 8.5, height = 11, units = "in")

# Combine charts (for README repo)
p_combined <- p1 + plot_spacer() + p2 + plot_layout(widths = c(0.49,0.02,0.49))
ggsave(filename = "./gallery_2022/2022_week-38_hydro-wwtp_combined.png",
       plot = p_combined,
       width = 17, height = 11, units = "in")

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



