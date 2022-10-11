# #TidyTuesday Week 40: Product Hunt
library(tidyverse)
library(lubridate)
library(ggtext)
library(sysfonts)
library(showtext)

# ------ LOAD DATA ------
product_hunt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv')
glimpse(product_hunt)

# ------ DATA VIZ IDEAS ------
# La visualizacion sera una combinacion de dos
# 1a - Line charts de # de releases por semana para mostrar el gran cambio en 2017
# 2a - Una bar chart que se enfoque en los tags del 2017

# ------ DATA WRANGLING ------
# Forma de la tabla
# category_tag | decripcion del producto con ese tag (no importa que se repita abajo) | Fecha en la que se publico el producto
df <- product_hunt %>%
  select(category_tags, product_description, release_date) %>%
  drop_na(product_description) %>%
  mutate(category_tags = str_remove_all(category_tags, pattern = "[''\\[\\]]"),
         category_tags = str_split(string = category_tags, pattern = ","),
         release_date = ymd(release_date))

  



