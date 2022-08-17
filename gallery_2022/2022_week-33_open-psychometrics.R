# #TidyTuesday Week 33: Open Psychometrics by Tanya Shapiro
library(dplyr)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

# Load data
tidy_tuesday_data <- tidytuesdayR::tt_load(2022, week = 33)

# Mostrar en una serie de datavis al estilo Cedric Scherer,
# en la visualizacion de los pinguinos, es decir,
# distribuciones y un scatterplot.
# Para el caso de las distribuciones usare todos los atributos.
# Sin embargo, para el scatter plot, reducire a 2 dimensiones todo el
# conjunto de datos con PCA. Seguidamente, mostrare todos los puntos pero
# solo resaltare los personajes de mis shows favoritos:
# 
# Avatar: The Last Airbender
# Gossip Girl
# Hamilton
# Stranger Things
# The Umbrella Academy


