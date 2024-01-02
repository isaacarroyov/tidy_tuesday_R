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
  
# Quitar bigramas con stop-words
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

df_bigrams_class_decade_clean_adj_count <- df_bigrams_class_decade_clean_adj %>%
  count(word_1, word_2, sort = T)


df_to_graph <- df_bigrams_class_decade_clean_adj_count %>%
  rename(freq = n) %>%
  filter(freq>=15) %>% 
  mutate(colour_enc = case_when(freq <= quantile(freq, 0.5) ~ 1,
                                freq > quantile(freq, 0.5) & freq <= quantile(freq, 0.8) ~ 2,
                                freq > quantile(freq, 0.8) & freq <= quantile(freq, 0.9) ~ 3,
                                freq > quantile(freq, 0.9) & freq <= quantile(freq, 0.97) ~ 4,
                                T ~ 5))


# ------ CREATE NETWORK ------
graph_bigrams <- df_to_graph %>%
  igraph::graph_from_data_frame() %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(degree_score = centrality_degree()) %>%
  rename(word = name) %>%
  activate(edges) %>%
  mutate(colour_enc = ordered(colour_enc,
                              levels = c(1,2,3,4,5),
                              labels = c("frequency less than 23",
                                         "frequency more than 23 and less than 43",
                                         "frequency more than 43 and less than 62",
                                         "frequency more than 62 and less than 107",
                                         "frequency more than 107")))

# get highest degree scores
vec_words_highest_degree_scores <- graph_bigrams %>%
  activate(nodes) %>%
  as_tibble() %>%
  arrange(-degree_score) %>%
  head(10) %>%
  pull(word)

# get the words for the top mos freq bigrams 
vec_words_top_bigrams <- c(df_to_graph %>% head(11) %>% pull(word_1) %>% unique(),
  df_to_graph %>% head(11) %>% pull(word_2) %>% unique())

# Highlight in graph
graph_bigrams <- graph_bigrams %>%
  activate(nodes) %>%
  mutate(highlight_degree_node = case_when(word %in% vec_words_highest_degree_scores ~ 1, T ~ 0.25),
         highlight_degree_word = case_when(word %in% c(vec_words_highest_degree_scores, vec_words_top_bigrams) ~ word, T ~ NA_character_))




# ------ DATAVIZ SETTINGS ------
# ------ Texts ------
title_text <- "Describing Big Foot"
subtitle_text_degree <- "One of the most famous mysteries in the U.S. is spotting Big Foot. The Big Foot Field Researchers Organization (BFRO) has gathered testimonials over the years and across the country.<br><br>The data visualization displays dots and lines. Each dot is a word, and when it is connected to another through a line, a bigram (a pair of words) is formed. The entire network showcases the most common bigrams used in the testimonial.<br><br><b>The connection between pairs of words:</b> The more connections a word has, the more is used in bigrams, and the bigger the dot (node).<br><br><b>The frequency of the bigrams:</b> It is not enough to know the most used words in bigrams; that's why the frequency of the bigram (connection) is shown as the colour of the links between the nodes."
caption_text <- "_The BFRO has classified the testimonials according to the potential for misinterpretation of what was observed or heard. Therefore, the words shown in the visualization are from testimonials Class A and B, classes with a low degree of misinterpretation._<br><br>Designed by Isaac Arroyo (@unisaacarroyov on twitter)<br>#TidyTuesday Week 37: Big Foot"
# ------ Colour Palette ------

# ------ Typography ------
font_add_google("Libre Baskerville", "title_font")
font_add_google("Source Sans Pro", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ DATA VISUALIZATION ------
# ------ Highlighted network (Degree) ------
set.seed(11)
p1 <- graph_bigrams %>%
  ggraph(layout = 'fr') +
  geom_edge_arc(aes(edge_colour = colour_enc),
                edge_width = 0.75,
                strength = 0.2,
                end_cap = circle(3,'pt')) + 
  geom_node_point(aes(size = degree_score, alpha = highlight_degree_node),
                  colour = "#165C6E") +
  ggrepel::geom_label_repel(aes(x=x,y=y, label = highlight_degree_word),
                            seed = 11,
                            force = 10,
                            force_pull = 0.01,
                            colour = "#0F3D49",
                            family = body_font,
                            fontface = 'bold',
                            size = 9) +
  scale_alpha_identity(guide = "none") +
  scale_edge_color_manual(values = MetBrewer::met.brewer("OKeeffe2", 5, direction = 1),
                          guide = guide_legend(title = NULL,
                                               label.position = "top",
                                               keyheight = unit(0.1,"in"),
                                               override.aes = list(edge_width=2)
                                               )) +
  guides(size = "none") +
  labs(title = title_text, subtitle = subtitle_text_degree, caption = caption_text) +
  coord_fixed() +
  theme_void() +
  theme(
    # Legend
    legend.position = "top",
    legend.key.width = unit(1.5,"in"),
    legend.text = element_textbox(family = body_font,
                                  size = rel(3),
                                  lineheight = 0.3,
                                  width = unit(1,"in")),
    # Tittle
    plot.title.position = "plot",
    plot.title = element_textbox(family = title_font,
                                 face = "bold",
                                 size = rel(8),
                                 lineheight = 0.2,
                                 margin = margin(l = -0.70, r = 0,
                                                 t = 0.25,
                                                 unit = "in")),
    # Subtitle
    plot.subtitle = element_textbox(family = body_font,
                                    size = rel(3.25),
                                    lineheight = 0.3,
                                    width = unit(7.5, units = "in"),
                                    margin = margin(l = -0.70, r = 0,
                                                    t = 0.125, b = 0.125,
                                                    unit = "in"),
                                    padding = margin(0,0,0.25,0, unit = "in")),
    # Caption
    plot.caption = element_textbox(family = body_font,
                                   size = rel(2.5),
                                   lineheight = 0.3,
                                   width = unit(7.5,units = "in"),
                                   halign = 0,
                                   hjust = 0,
                                   margin = margin(l = -0.70, r = 0,
                                                   t = 0.125, b = 0.25,
                                                   unit = "in"),
                                   padding = margin(0,0,0,0, unit = "in"))
    )

ggsave(filename = "./gallery_2022/2022_week-37_bigfoot.png",
       plot = p1,
       width = 8.5, height = 11, units = "in",
       bg = "#FFFBF6")
