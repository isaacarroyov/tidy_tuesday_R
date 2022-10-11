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
# Se pueden hacer dos tipos de visualizaciones
# 1a - Una network con las conexiones de los n-gramas usadas para describir productos 
#      de ciertas tags
# 2a - Una serie de tiempo observando la cantidad de realises de productos para saber
#      si hay una "estacion" donde hayan mas

# ------ DATA WRANGLING ------
# Forma de la tabla
# category_tag | decripcion del producto con ese tag (no importa que se repita abajo) | Fecha en la que se publico el producto
df <- product_hunt %>%
  select(category_tags, product_description, release_date) %>%
  drop_na(product_description) %>%
  mutate(category_tags = str_remove_all(category_tags, pattern = "[''\\[\\]]"),
         category_tags = str_split(string = category_tags, pattern = ",")) %>%
  unnest_longer(category_tags) %>%
  mutate(category_tags = str_squish(category_tags),
         release_date = ymd(release_date))
