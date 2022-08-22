# #TidyTuesday Week 33: Open Psychometrics by Tanya Shapiro

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

library(dplyr)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)
library(factoextra)

# ------ LOAD DATA ------
psychometrics_characters <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv")



# ------ SELECT PERSONALITY TRAITS------
vec_personalities <- c("cocky","independent","genius","eloquent","driven","chatty","adventurous",
                       "active","decisive","crazy")

# ------ FIND QUESTIONS FOR THE PERSONALITY TRAITS------
vec_questions <- psychometrics_characters %>%
  filter(personality %in% vec_personalities) %>%
  pull(question) %>% unique()

relevant_dataset <- psychometrics_characters %>%
  filter(question %in% vec_questions) %>%
  select(char_id, char_name, uni_id, uni_name, question, personality, avg_rating)

# ------ DATA WRANGLING------
pca_dataset <- relevant_dataset %>%
  mutate(main_score = case_when(personality %in% vec_personalities ~ avg_rating,
                                       T ~ 100-avg_rating),
         main_personality = case_when(personality == 'timid' ~ 'cocky',
                                      personality == 'codependent' ~ 'independent',
                                      personality == 'dunce' ~ 'genius',
                                      personality == 'unpolished' ~ 'eloquent',
                                      personality == 'unambitious' ~ 'driven',
                                      personality == 'reserved' ~ 'chatty',
                                      personality == 'stick-in-the-mud' ~ 'adventurous',
                                      personality == 'slothful' ~ 'active',
                                      personality == 'hesitant' ~ 'decisive',
                                      personality == 'sane' ~ 'crazy',
                                      T ~ personality
                                      )) %>%
  select(char_id, char_name, uni_id, uni_name, main_personality, main_score) %>%
  tidyr::pivot_wider(names_from = main_personality, values_from = main_score)


# ------ PCA------
numeric_data <- pca_dataset %>% select_if(is.numeric)
pca_results <- prcomp(numeric_data)
fviz_eig(pca_results)

# Keep the firts 3 PCA's
df <- pca_dataset %>%
  bind_cols(pca_results$x %>%
  as_tibble() %>%
  select(PC1,PC2,PC3))

# ------ DATA VISUALIZATION------





