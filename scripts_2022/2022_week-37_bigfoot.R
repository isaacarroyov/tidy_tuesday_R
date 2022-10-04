# ------ #TidyTuesday Week 37: Bigfoot ------

# ------ DATA VISUALIZATION IDEA ------
# Observando los plots que ha subido la comunidad, en gran parte son mapas de los
# avistamientos de Big foot en el pais. Algunos de ellos se han comparado con peliculas
# relacionadas a Big foot o con areas donde habitan osos.
#
# Para este caso, quiero explorar los bigramas de las descripciones/testinomios
# y la major manera es a traves de una red (network)

# ------LOAD LIBRARIES ------
library(dplyr)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

# ------ TEXT MINING LIBRARIES ------
# install.packages("tidytext")
library(tidytext)


# ------ NETWORK LIBRARIES ------
library(ggraph)
library(tidygraph)

# ------ LOAD DATA ------
data_bigfoot <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv")



# ------ DATA WRANGLING ------
df <- data_bigfoot %>%
  filter(classification %in% c("Class A","Class B")) %>%
  select(classification, observed)
  
# ------ TIDYING THE TEXT ------
# Obtener bigramas
df_bigrams_class_decade <- df %>%
  unnest_tokens(output = bigram,
                input = observed,
                token = "ngrams",
                n = 2)
  
# Quitar bigramas sin stop-words
df_bigrams_class_decade_clean <- df_bigrams_class_decade %>%
  tidyr::separate(col = bigram,
                  into = c("word_1", "word_2"),
                  sep = " ") %>%
  filter(!word_1 %in% stop_words$word,
         !word_2 %in% stop_words$word)


# Filter by adjectives (at least one)
vec_idx_adjectives <- which(parts_of_speech$pos == "Adjective")
vec_words_adjectives <- parts_of_speech$word[vec_idx_adjectives]

df_bigrams_class_decade_clean_adj <- df_bigrams_class_decade_clean %>%
  filter(word_1 %in% vec_words_adjectives | word_2 %in% vec_words_adjectives)

# Create network
graph_bigrams <- df_bigrams_class_decade_clean_adj %>%
  count(word_1, word_2, sort = T) %>%
  filter(n >= 15) %>%
  igraph::graph_from_data_frame() %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(degree_score = centrality_degree(),
         eigen_score = centrality_eigen())


graph_bigrams %>%
  activate(nodes) %>%
  as_tibble() %>%
  arrange(-eigen_score) %>%
  print(n=10)

# ------ DATAVIZ SETTINGS ------
# ------ Texts ------
title_text <- "Describing Big Foot"
subtitle_text <- "Bigrams used in class A and class B sights"
subtitle_text_highlight_bigrams <- "Another text"
caption_text <- "Designed by Isaac Arroyo (@unisaacarroyov on twitter)<br>#TidyTuesday Week 31: Big Foot"
# ------ Colour Palette ------

# ------ Typography ------

# ------ DATA VISUALIZATION ------
set.seed(11)
graph_bigrams %>%
  ggraph(layout = 'fr') +
  geom_edge_arc(edge_width = 0.3,
                strength = 0.2,
                end_cap = circle(3,'pt')
                ) + 
  geom_node_point(aes(size = eigen_score)) +
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text) +
  theme_void() +
  theme(
    legend.position = "none"
  )
# ------ SAVE GGPLOT ------




