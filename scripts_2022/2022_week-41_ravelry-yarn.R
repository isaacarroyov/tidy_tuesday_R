library(dplyr)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)


# ------ LOAD DATA ------
yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

# ------ DATAVIZ IDEA ------
# Algo que noté explorando los datos fue que hay ratings que son bastante altos peeeero debido a que 
# han sido calificados muy pocas veces y la gran mayoria (si no es que todos) son calificaciones altas
# Entonces el objetivo de la visualizacion es mostrar las distribuciones de ambos valores y resaltar
# ambos casos extremos. Productos que han sido calificados muy pocas veces muy bien y productos que 
# han sido calificados una exagerada cantidad de veces (muy populares) pero que quizas no tengan 
# calificaciones muy altas

# ------ DATA WRANGLING ------
df <- yarn %>%
  select(yarn_company_name, rating_average, rating_count) %>%
  tidyr::drop_na()
  # select(yarn_company_name, rating_average, rating_count, grams, yardage, yarn_weight_wpi, yarn_weight_name)

df_rating_count_companies <- df %>%
  group_by(yarn_company_name) %>%
  summarise(min_rating_average = min(rating_average),
            avg_rating_average = mean(rating_average),
            median_rating_average = median(rating_average),
            max_rating_average = max(rating_average)) %>%
  tidyr::pivot_longer(cols = 2:5, names_to = "stats_rating_average", values_to = "values_rating_average") %>%
  bind_cols(df %>%
              group_by(yarn_company_name) %>%
              summarise(min_rating_count = min(rating_count),
              avg_rating_count = mean(rating_count),
              median_rating_count = median(rating_count),
              max_rating_count = max(rating_count)) %>%
              tidyr::pivot_longer(cols = 2:5, names_to = "stats_rating_count", values_to = "values_rating_count")) %>%
  rename(yarn_company_name = `yarn_company_name...1`) %>%
  select(yarn_company_name, stats_rating_average, values_rating_average, stats_rating_count, values_rating_count)

df_rating_count_companies


