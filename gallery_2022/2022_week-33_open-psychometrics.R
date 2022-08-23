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
library(gghighlight)
library(ggrepel)
library(MetBrewer)
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

# Keep the firts 2 PCA's
df <- pca_dataset %>%
  bind_cols(pca_results$x %>%
  as_tibble() %>%
  select(PC1,PC2))

# ------ DATA VISUALIZATION------
font_add_google("Comfortaa", "titleFont")
font_add_google("Josefin Sans", "bodyFont")
showtext_auto()


title_font <- "titleFont"
body_font <- "bodyFont"


fav_shows <- c('Avatar: The Last Airbender', 'Gossip Girl', 'Hamilton', 'Stranger Things', 'The Umbrella Academy')
title_text <- "The Universe of Personalities"
subtitle_text <- "A fictional character does not have a real personality, but people might perceive it to have one. Thanks to this dataset, we can view the personality spectrum of the characters from more than 50 TV Shows or movies.<br>I selected ten personality traits (cocky, independent, genius, eloquent, driven, chatty, adventurous, active, decisive and crazy). Then, in order to see them in a 2-dimensional plane, I performed a PCA on them.<br>The highlighted points are characters from my favourite TV shows, The Umbrella Academy, Gossip Girl, Avatar: The Last Airbender, Hamilton and The Umbrella Academy."
caption_text <- "Designed by Isaac Arroyo (@unisaacarroyov on Twitter) <br>#TidyTuesday Week 33: Open Psychometrics by Tanya Shapiro (@tanya_shapiro on Twitter)"


p1 <- df %>%
  ggplot(aes(x=PC1, y= PC2, colour= uni_name)) +
  geom_point() +
  gghighlight(uni_name %in% fav_shows, max_highlight = 50,
              unhighlighted_params = list(color='gray50', alpha = 0.5)
              ) +
  geom_label_repel(aes(label=char_name), max.overlaps = 50, seed = 11,
                   force = 2, nudge_x = 10,
                   family = body_font,
                   ) +
  scale_colour_manual(values = met.brewer("Java", n=5, direction = -1)) +
  labs(title= title_text,
       subtitle = subtitle_text,
       caption = caption_text) + 
  theme_void() +
  theme(
    # Background
    plot.background = element_rect(fill = '#F6E3BD', colour = '#F6E3BD'),
    # Title
    plot.title.position = "plot",
    plot.title = element_textbox(family = title_font, face = 'bold',
                                 size = rel(9),
                                 padding = maring(0,0,0,0),
                                 margin = margin(0.125,0,0,0, unit = "in")
                                 ),
    # Subtitle
    plot.subtitle = element_textbox(family = body_font, face = 'plain', size = rel(3.5),
                                    lineheight = 0.2,
                                    width = unit(7.5,'in'),
                                    padding = margin(0,0,0,0),
                                    margin = margin(0,0,0,0, unit = "in")
                                    ),
    # Caption
    plot.caption.position = "plot",
    plot.caption = element_textbox(halign = 0, hjust = 0, family = body_font, face = 'bold',
                                   size = rel(2),
                                   lineheight = 0.5,
                                   padding = margin(0,0,0,0),
                                   margin = margin(0,0,0,0, unit = "in")
                                   )
    
  )
    

ggsave(filename = "./gallery_2022/2022_week-33_open-psychometrics.png",
       plot = p1,
       width = 8.5, height = 11, units = "in",
       dpi = 300)
