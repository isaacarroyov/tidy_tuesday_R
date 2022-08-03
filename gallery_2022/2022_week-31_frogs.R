# TidyTuesday Week 31: Oregon Spotted Frog
library(dplyr)
library(ggplot2)
library(MetBrewer)
# library(tidygraph)
library(ggraph)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

# LOAD DATA
datos <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')
# Columns
names(datos)

# Select columns for the data visualization
df <- datos %>%
  select(Subsite, Water) %>%
  rename(subsite = Subsite, water = Water) %>%
  mutate(subsite = stringr::str_replace_all(subsite, " ",""),
         water = stringr::str_replace_all(water, " ",""),
         )



# Create from->to data (edges)
df_from_to <- df %>%
  mutate(from = paste(subsite, sep = "."),
         to = paste(subsite, water, sep = ".")
         ) %>%
  select(from, to)

# Create vertices
df_vertices <- bind_rows(
  df_from_to %>% count(from) %>% rename(name = from),
  df_from_to %>% count(to) %>% rename(name = to)
)




# DATA VISUALIZATION
gr <- igraph::graph_from_data_frame(df_from_to, vertices = df_vertices)
gr_02 <- tidygraph::tbl_graph(df_vertices, df_from_to)

set.seed(1)
ggraph(gr_02, 'circlepack', weight = n) +
  geom_node_circle(aes(fill=depth)) +
  geom_node_label(aes(label=name, filter = leaf), size=1.5) +
  coord_fixed(clip = "off") +
  theme_minimal()
