# TidyTuesday Week 38: Wastewater plants
library(tidyverse)
library(ggtext)

# LOAD DATA
HydroWASTE_v10 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv')

# DESARROLLO DE IDEA
# Para esta visualizacion, lo importante va a ser mostrar la cantidad de WWTP en Mexico
# y señalar las que no cumplen con el DF mínimo. Si se puede, también mostrar donde están
# sus outfalls

# Separar dos conjuntos de datos
# WWTP
wwtp_mex <- HydroWASTE_v10 %>%
  filter(COUNTRY == "Mexico") %>%
  select(LAT_WWTP, LON_WWTP, WWTP_NAME, DF)

wwtp_mex_outfalls <- HydroWASTE_v10 %>%
  filter(COUNTRY == "Mexico") %>%
  select(LAT_OUT, LON_OUT, WWTP_NAME, DF)

# ------ DATA WRANGLING ------
# Encode para colorear los puntos "malos"
# 
