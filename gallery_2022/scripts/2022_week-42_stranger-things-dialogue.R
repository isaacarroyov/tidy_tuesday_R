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
# palabras o 4 redes (una por cada season)

# ------ TEXT MINING LIBRARIES ------
# install.packages("tidytext")
library(tidytext)


# ------ NETWORK LIBRARIES ------
# library(ggraph)
# library(tidygraph)

df_stage_words_all_seasons <- datos %>%
  select(season, stage_direction) %>%
  drop_na() %>%
  mutate(stage_direction = str_remove_all(stage_direction,"\\[|\\]")) %>%
  unnest_tokens(output = word_stage, input = stage_direction) %>%
  filter(!word_stage %in% stop_words$word)

df_stage_words_all_seasons %>%
  count(word_stage, season, sort = T)
