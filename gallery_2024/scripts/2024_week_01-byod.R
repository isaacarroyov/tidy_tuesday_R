library(tidyverse)
library(ggtext)
library(sysfonts)
library(showtext)

# LOAD DATA
df <- read_csv("./data/2023/2023_week_01-byod-animales-tesina-ruanda.csv") %>%
  select(categoria_placer_02, subcategoria_placer, categoria_melancolia_02, subcategoria_melancolia) %>%
  mutate(subcategoria_placer = case_when(subcategoria_placer == "Sinrespuesta" ~ "Sin respuesta",
                                         subcategoria_placer == "Mantisreligiosa" ~ "Mantis Religiosa",
                                         subcategoria_placer == "Estrellademar" ~ "Estrella de Mar",
                                         subcategoria_placer == "Osopanda" ~ "Oso Panda",
                                         subcategoria_placer == "Osoperezoso" ~ "Oso Perezoso",
                                         T ~ subcategoria_placer),
         subcategoria_melancolia = case_when(subcategoria_melancolia == "Sinrespuesta" ~ "Sin respuesta",
                                             subcategoria_melancolia == "Animalesenpeligrodeextinción" ~ "Animales en Peligro de Extinción",
                                             subcategoria_melancolia == "Cualquieranimalqueestésólo" ~ "Cualquier animal que esté sólo",
                                             subcategoria_melancolia == "Osopanda" ~ "Oso Panda",
                                             subcategoria_melancolia == "Osoperezoso" ~ "Oso Perezoso",
                                             subcategoria_melancolia == "Osopolar" ~ "Oso Polar",
                                             subcategoria_melancolia == "Pájarocarpintero" ~ "Pájaro Carpintero",
                                             subcategoria_melancolia == "Rinocerontenegro" ~ "Rinoceronte Negro",
                                             T ~ subcategoria_melancolia))

# Pasar de tabla a network
df_2_graph <- df %>%
  count(categoria_placer_02, subcategoria_placer) %>%
  mutate(categoria = "Placer") %>%
  rename(tipo = categoria_placer_02, subtipo = subcategoria_placer) %>%
  bind_rows(df %>%
              count(categoria_melancolia_02, subcategoria_melancolia) %>%
              mutate(categoria = "Melancolía") %>%
              rename(tipo = categoria_melancolia_02, subtipo = subcategoria_melancolia)) %>%
  rename(freq = n)


