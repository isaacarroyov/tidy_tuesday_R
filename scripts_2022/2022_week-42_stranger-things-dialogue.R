# #TidyTuesday Week 42 - Stranger Things (Dialogue)
library(tidyverse)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

# LOAD DATA
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/episodes.csv')
all_dialogue <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv')

datos <- all_dialogue %>%
  left_join(episodes, by = c("season", "episode")) %>%
  rename(title_episode = title) %>%
  select(season, title_episode, raw_text, stage_direction, dialogue)

# DATA VIZ IDEA
# Para esta ocasion, otra network podria funcionar. En este caso, seria ver las palabras que mas 
# se repiten en toda la serie y hacia que episodios van dirigidas. Se puede crear una red gigante de 
# palabras o 4 redes