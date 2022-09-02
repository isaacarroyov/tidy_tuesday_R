# Week 35 - Pell Awards
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)
library(MetBrewer)

# LOAD DATA
pell <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv')

# En la data-vis intentare complementar el trabajo de @BlakeRobMills
# La visualizacion constará de kdplots + jitterplots en una serie de tiempo
# Cada punto sera el AWARD/RECIPIENT para obtener un "promedio" de USD per student
# Los puntos que seran resaltados serán las Ivy League
# Con los kde plots se vera la distribucion del dinero por estudiante
# Con esta visualizacion podremos ver la distribucion de todas las univesidades
# pero comparando o resaltando las Ivy
# En esta visualizacion no podre simplificar todas las universidades, esto es algo que se tendra
# que tomar en cuenta en la visualizacion