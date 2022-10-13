library(dplyr)
library(ggplot2)
# install.packages('ggbeeswarm')
library(ggbeeswarm)
library(ggtext)
library(sysfonts)
library(showtext)


# ------ LOAD DATA ------
yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

# ------ DATAVIZ IDEA ------
# Back to hacer algo raro:
# No tengo ni la mas minima idea sobre el tema de yarn, sin embargo es una perfecta 
# oportunidad para hacer una visualizacion poco convencional nada mas por el gusto
# de crear algo.

# ------ DATA WRANGLING ------
df <- yarn %>%
  select(yarn_company_name, rating_average, rating_count,yarn_weight_name) %>%
  tidyr::drop_na()
  # select(yarn_company_name, rating_average, rating_count, grams, yardage, yarn_weight_wpi, yarn_weight_name)




