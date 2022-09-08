# ------ #TidyTuesday Week 36: LEGO -------

# ------- Idea de la visualizacion de datos -------
# Tras notar la visualizacion de Cara Thompson (@cararthompson), pense en visualizar todos los colores 
# de las piezas de LEGO de dos maneras:
# 1 - Como generative art (con ayuda del paquete {aRtsy})
# 2 - De nuevo, como una linea del tiempo circular (amo la geometria de las curvas) donde se ve como se van agregando nuevos colores



library(dplyr)
library(stringr)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

# ------- LOAD DATA -------
colours <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
inventory_parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')
inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
sets <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
themes <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')