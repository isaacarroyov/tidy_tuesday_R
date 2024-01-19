library(tidyverse)

# IDEA:
# Create a network (or 2 networks) where the main animal category
# is the biggest node and the smallers nodes are the second animal category.
# The association of the animals chosen by the person (either is pleasure or
# melancoly) is given by the color of the edges. The size of the edges are
# going to be how many times that "pair" is mentioned.

# = = LOAD DATA = = #
db <- read_csv("./data/2024/2024_week_01-byod-animales-tesina-ruanda.csv") %>%
  # - - Select variables - - #
  select(categoria_placer_02, subcategoria_placer,
         categoria_melancolia_02, subcategoria_melancolia) %>%
  # - - Data cleaning - - #
  mutate(
    subcategoria_placer = case_when(
      subcategoria_placer == "Sinrespuesta" ~ "Sin respuesta",
      subcategoria_placer == "Mantisreligiosa" ~ "Mantis Religiosa",
      subcategoria_placer == "Estrellademar" ~ "Estrella de Mar",
      subcategoria_placer == "Osopanda" ~ "Oso Panda",
      subcategoria_placer == "Osoperezoso" ~ "Oso Perezoso",
      .default = subcategoria_placer),
    subcategoria_melancolia = case_when(
      subcategoria_melancolia == "Sinrespuesta" ~ "Sin respuesta",
      subcategoria_melancolia == "Animalesenpeligrodeextinción" ~ "Animales en Peligro de Extinción", # nolint
      subcategoria_melancolia == "Cualquieranimalqueestésólo" ~ "Cualquier animal que esté sólo", # nolint
      subcategoria_melancolia == "Osopanda" ~ "Oso Panda",
      subcategoria_melancolia == "Osoperezoso" ~ "Oso Perezoso",
      subcategoria_melancolia == "Osopolar" ~ "Oso Polar",
      subcategoria_melancolia == "Pájarocarpintero" ~ "Pájaro Carpintero",
      subcategoria_melancolia == "Rinocerontenegro" ~ "Rinoceronte Negro",
      .default = subcategoria_melancolia))

# = = DATA PROCESSING = = #
# - - Create a graph object from a tibble - - #
# ~ ~ Pleasure ~ ~ #
pairs_pleasure <- count(db, categoria_placer_02, subcategoria_placer) %>%
  mutate(categoria = "Placer") %>%
  rename(tipo = categoria_placer_02, subtipo = subcategoria_placer)

# ~ ~ Melancoly ~ ~ #
pairs_mel <- count(db, categoria_melancolia_02, subcategoria_melancolia) %>%
  mutate(categoria = "Melancolía") %>%
  rename(tipo = categoria_melancolia_02, subtipo = subcategoria_melancolia)

# ~ ~ full data ~ ~ #
df2graph <- bind_rows(pairs_pleasure, pairs_mel) %>% rename(freq = n)
