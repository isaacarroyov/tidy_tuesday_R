# ------ #TidyTuesday Week 36: LEGO -------
# Data Visualization inspired by Cara Thompson (@cararthompson)

# ------- Idea de la visualizacion de datos -------
# Tras notar la visualizacion de Cara Thompson (@cararthompson), pense en visualizar todos los colores 
# de las piezas de LEGO de dos maneras:
# 1 - Como generative art (con ayuda del paquete {aRtsy})
# 2 - De nuevo, como una linea del tiempo circular (amo la geometria de las curvas) donde se ve como se van agregando nuevos colores

library(dplyr)
# library(stringr)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

# Generative Art
library(aRtsy)

# ------- LOAD DATA -------
colours <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
inventory_parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')
inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
sets <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
themes <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')


# ------ DATA WRANGLING 
# Lo que necesito hacer para crear los generative art plots es conocer el 
# codigo HEX de los colores y el año al que pertenecen.
# El año se encuentra en `sets` y los colores en `colours`
# La imagen de las base de datos relacional se puedeencontrar en el siguiente link:
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-09-06/pic1.png
#
# Para obtener el conjunto de datos que una `colours` con `sets` es el siguiente:
# `sets` -> by "set_num" ->`inventories` -> by "id"= "inventory_id" -> `inventory_parts` -> by "color_id"="id" -> color

# ------ JOIN TABLES ------
# Empezamos con `inventory_parts` ya que tiene inventory_id y color_id.
# Hacemos primero left_join con `colours`
df_colours_years <- inventory_parts %>%
  left_join(colours, by = c("color_id" = "id")) %>%
  select(inventory_id, color_id, rgb) %>%
# Ahora con "inventory_id" vamos a hacer el left_join con "id" de `inventories`
  left_join(inventories, by = c("inventory_id" = "id")) %>%
  select(rgb, set_num) %>%
# Por ultimo, usamos "set_num" para hacer un left_join con sets 
  left_join(sets, by = "set_num") %>%
  select(year, rgb) %>%
# Renombramos rgb a hex y agregamos el #
  rename(hex = rgb) %>%
  mutate(hex = paste0('#', hex))


# ------ FILTER '49 AND '20s AND GROUP BY DECADES 
df_colours_decades <- df_colours_years %>%
  filter(year %in% seq(1950,2019)) %>%
  mutate(decade = case_when(year %in% seq(1950, 1959) ~ "50's",
                            year %in% seq(1960, 1969) ~ "60's",
                            year %in% seq(1970, 1979) ~ "70's",
                            year %in% seq(1980, 1989) ~ "80's",
                            year %in% seq(1990, 1999) ~ "90's",
                            year %in% seq(2000, 2009) ~ "00's",
                            year %in% seq(2010, 2019) ~ "10's",
                            T ~ NA_character_)) %>%
  select(decade, hex)


# ------ CREATE VECTORS OF UNIQUE HEX COLOURS PER DECADE
v_50s <- df_colours_decades %>%
  filter(decade == "50's") %>%
  pull(hex) %>%
  unique()

v_60s <- df_colours_decades %>%
  filter(decade == "60's") %>%
  pull(hex) %>%
  unique()

v_70s <- df_colours_decades %>%
  filter(decade == "70's") %>%
  pull(hex) %>%
  unique()

v_80s <- df_colours_decades %>%
  filter(decade == "80's") %>%
  pull(hex) %>%
  unique()

v_90s <- df_colours_decades %>%
  filter(decade == "90's") %>%
  pull(hex) %>%
  unique()

v_00s <- df_colours_decades %>%
  filter(decade == "00's") %>%
  pull(hex) %>%
  unique()

v_10s <- df_colours_decades %>%
  filter(decade == "10's") %>%
  pull(hex) %>%
  unique()

# ------ CREATE INDIVIDUAL GENERATIVE ART PLOTS



