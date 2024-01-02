# ------ #TidyTuesday Week 36: LEGO -------
# Data Visualization inspired by Cara Thompson (@cararthompson)

# ------- Idea de la visualizacion de datos -------
# Tras notar la visualizacion de Cara Thompson (@cararthompson), pense en visualizar todos los colores 
# de las piezas de LEGO de dos maneras:
# 1 - Como generative art (con ayuda del paquete {aRtsy})
# 2 - De nuevo, como una linea del tiempo circular (amo la geometria de las curvas) donde se ve como se van agregando nuevos colores

library(dplyr)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)
library(ggimage)
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

# ------ CREATE INDIVIDUAL GENERATIVE ART PLOTS ------
# The art -> Strokes
# ------ 50s ------
set.seed(13)
p_50s <- canvas_flow(colors = v_50s, background = "transparent",
            lines = 300,
            lwd = 0.8,
            iterations = 2000,
            stepmax = 0.08,
            polar = T
            )
# ------ 60 ------
set.seed(14)
p_60s <- canvas_flow(colors = v_60s, background = "transparent",
                     lines = 300,
                     lwd = 0.8,
                     iterations = 2000,
                     stepmax = 0.08,
                     polar = T)

# ------ 70 ------
set.seed(420)
p_70s <- canvas_flow(colors = v_70s, background = "transparent",
            lines = 300,
            lwd = 0.8,
            iterations = 2000,
            stepmax = 0.08,
            polar = T)

# ------ 80s ------
set.seed(134)
p_80s <- canvas_flow(colors = v_80s, background = "transparent",
                     lines = 300,
                     lwd = 0.8,
                     iterations = 2000,
                     stepmax = 0.08,
                     polar = T)
# ------ 90s ------
set.seed(104)
p_90s <- canvas_flow(colors = v_90s, background = "transparent",
                     lines = 300,
                     lwd = 0.8,
                     iterations = 2000,
                     stepmax = 0.08,
                     polar = T)
# ------ 00s ------
set.seed(184)
p_00s <- canvas_flow(colors = v_00s, background = "transparent",
                     lines = 300,
                     lwd = 0.8,
                     iterations = 2000,
                     stepmax = 0.08,
                     polar = T)
# ------ 10s ------
set.seed(1426)
p_10s <- canvas_flow(colors = v_10s, background = "transparent",
                     lines = 300,
                     lwd = 0.8,
                     iterations = 2000,
                     stepmax = 0.08,
                     polar = T)



# ------ Saving plots ------
ggsave(filename = "./gallery_2022/ignore_2022_week-36_lego_01.png",
       plot = p_50s, width = 3, height = 4, units = "in", bg = "transparent")
ggsave(filename = "./gallery_2022/ignore_2022_week-36_lego_02.png",
       plot = p_60s, width = 3, height = 4, units = "in", bg = "transparent")
ggsave(filename = "./gallery_2022/ignore_2022_week-36_lego_03.png",
       plot = p_70s, width = 3, height = 4, units = "in", bg = "transparent")
ggsave(filename = "./gallery_2022/ignore_2022_week-36_lego_04.png",
       plot = p_80s, width = 3, height = 4, units = "in", bg = "transparent")
ggsave(filename = "./gallery_2022/ignore_2022_week-36_lego_05.png",
       plot = p_90s, width = 3, height = 4, units = "in", bg = "transparent")
ggsave(filename = "./gallery_2022/ignore_2022_week-36_lego_06.png",
       plot = p_00s, width = 3, height = 4, units = "in", bg = "transparent")
ggsave(filename = "./gallery_2022/ignore_2022_week-36_lego_07.png",
       plot = p_10s, width = 3, height = 4, units = "in", bg = "transparent")

# ------ Extract images an create a circular layout ------
# ------ Circular layout ------
angles <- seq(pi/2, by = -2*pi/7, length.out = 7)
radius <- 4
x_pos <- radius * cos(angles)
y_pos <- radius * sin(angles)
path_images <- paste0("./gallery_2022/ignore_2022_week-36_lego_0", 1:7,".png")
description_images <-c(glue::glue("<span style='font-family:title_font;font-size:40px'><b>1950s</b></span><br>{length(v_50s)} colours"),
                       glue::glue("<span style='font-family:title_font;font-size:40px'><b>1960s</b></span><br>{length(v_60s)} colours"),
                       glue::glue("<span style='font-family:title_font;font-size:40px'><b>1970s</b></span><br>{length(v_70s)} colours"),
                       glue::glue("<span style='font-family:title_font;font-size:40px'><b>1980s</b></span><br>{length(v_80s)} colours"),
                       glue::glue("<span style='font-family:title_font;font-size:40px'><b>1990s</b></span><br>{length(v_90s)} colours"),
                       glue::glue("<span style='font-family:title_font;font-size:40px'><b>2000s</b></span><br>{length(v_00s)} colours"),
                       glue::glue("<span style='font-family:title_font;font-size:40px'><b>2010s</b></span><br>{length(v_10s)} colours"))
# ------- create df -------
df <- tibble(x = x_pos,
       y = y_pos,
       image = path_images,
       descriptions = description_images,
       label_x_pos = x_pos * 1.5,
       label_y_pos = y_pos * 1.5)


# ------ Text settings -------
font_add_google("Yeseva One", "title_font")
font_add_google("Josefin Sans", "body_font")
font_add_google("Ubuntu Mono", "code_font")
showtext_auto()
body_font <- 'body_font'
title_font <- 'title_font'

# ------- data viz -------
p1 <- df %>%
  ggplot(aes(x = x, y = y)) +
  geom_image(aes(image = image),
             size = 0.3) +
  geom_textbox(data = tibble(x = 0, y = 0,
                             label = "<span style='font-family:title_font;font-size:60px'><b>LEGO colours over the decades</b></span><br><br>LEGO blocks have evolved sensationally, from basic plastic rectangular construction blocks for kids to big-scale monuments in Downtown Disney, Florida.<br>Much of LEGO's evolution was due to other big projects wanting to be represented by LEGO blocks, like movies and TV shows. Because of that, the colour of the blocks has changed.<br>To represent them, I wanted to use generative art; hence I used <span style='font-family:code_font'><b>{aRtsy}</b></span>, an R package that creates it. The colour palette for each _painting_ is the colours of the LEGO blocks during that decade.<br><br><span style='font-size:25px'>Design by Isaac Arroyo (@unisaacarroyov on twitter)<br>#TidyTuesday Week 36: LEGO<br>Data source: <span style='font-family:code_font'><b>{rebrickable}</b></span></span>"),
               aes(x = x, y = y, label = label),
               family = body_font,
               halign = 0,
               hjust = 0.48,
               size = 10,
               lineheight = 0.33,
               width = unit(2.2,'in'),
               fill = 'transparent',
               box.colour = 'transparent',
               box.padding = unit(0,'in')) +
  geom_textbox(aes(x = label_x_pos, y = label_y_pos, label = descriptions),
               halign = 0.5,
               hjust = 0.5,
               family = body_font,
               size = 10,
               lineheight = 0.33,
               width = unit(0.6,'in'),
               fill = '#FFFFFF',
               alpha = 0.3,
               box.colour = 'black',
               box.padding = unit(0.08,'in')) +
  # fill and box.colour -> transparent
  geom_textbox(aes(x = label_x_pos, y = label_y_pos, label = descriptions),
               halign = 0.5,
               hjust = 0.5,
               family = body_font,
               size = 10,
               lineheight = 0.33,
               width = unit(0.6,'in'),
               fill = 'transparent',
               box.colour = 'transparent',
               box.padding = unit(0.08,'in')) +
  # coord_cartesian(ylim = c(-6.5,6.5), xlim = c(-6.5,6.5)) +
  theme_void()

ggsave(filename = "./gallery_2022/2022_week-36_lego.png",
       plot = p1, 
       bg = "#FDF4DB",
       width = 8.5, height = 8.5, units = "in",
       dpi = 300)

