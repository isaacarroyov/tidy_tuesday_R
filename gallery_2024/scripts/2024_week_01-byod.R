library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggtext)

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
vec_tipos <- distinct(.data = df2graph, tipo) %>% pull(tipo)

# ~ ~ From tibble to graph ~ ~ #
graph_animals <- df2graph %>%
  rename(from = tipo, to = subtipo) %>%
  igraph::graph_from_data_frame() %>%
  as_tbl_graph() %>%
  to_undirected() %>%
  activate(edges) %>%
  # Freq categories
  mutate(
    freq_cat = case_when(
      freq == 1 ~ "1",
      between(x = freq, left = 2, right = 4) ~ "2-4",
      between(x = freq, left = 5, right = 9) ~ "5-9",
      freq >=  10 ~ "+10"),
    freq_cat = ordered(x = freq_cat, levels = c("1", "2-4", "5-9", "+10"))
  ) %>%
  activate(nodes) %>%
  mutate(tipo = if_else(name %in% vec_tipos, name, NA_character_))

# = = DATA VISUALIZATION = = #

# - - Edge colours - - #
col_palette <- c("gray70", MetBrewer::met.brewer(name = "Tam", n = 8)[c(3, 5, 8)]) # nolint

# - - Plot - - #
set.seed(11)
p1 <- ggraph(graph = graph_animals, layout = "fr") +
  geom_edge_loop(
    mapping = aes(edge_colour = freq_cat),
    edge_width = 1) +
  geom_edge_arc(
    mapping = aes(edge_colour = freq_cat),
    edge_width = 1) +
  geom_node_point() +
  geom_node_text(
    mapping = aes(label = tipo),
    nudge_y = -0.3,
    fontface = "italic") +
  facet_edges(vars(categoria)) +
  labs(
    title = "¿Qué animal viene a tu mente cuando piensas en...?",
    subtitle = "Resultados de una tesina en Artes Visuales",
    caption = 'Ruanda Zapata, Licenciada en Artes Visuales, en su tesina "Placer y Melancolía: Estudio y representación gráfica desde la calcografía", realiza a una encuesta a poco más de 70 personas sobre sus perspectivas del placer y la melancolía a través de colores, arte y <b>animales</b>. Las respuestas tuvieron más de 60 animales distintos, estos estan siendo representados en grafos (redes) para cada sentimiento<br><br>Isaac Arroyo (@unisaacarroyov) | #TidyTuesday Week 1: Bring Your Own Data (BYOD)') + # nolint
  scale_edge_color_manual(values = col_palette) +
  # scale_edge_width_manual(values = c(0.2, 0.75, 1.25, 1.75), guide = "none") +
  guides(
    edge_color = guide_legend(
      title.position = "top",
      title = "Menciones",
      label.position = "bottom",
      override.aes = list(edge_width = 2),
      )
    ) +
  theme_void() +
  theme(
    # Background
    plot.background = element_rect(fill = "#E6EBEE", color = "transparent"),
    # Title, Subtitle & Caption
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.title = element_textbox(
      width = unit(8, "in"),
      face = "bold",
      fill = "gray80",
      halign = 0.5,
      hjust = 0.5,
      ),
    plot.subtitle = element_textbox(
      width = unit(8, "in"),
      fill = "gray80",
      halign = 0.5,
      hjust = 0.5,
      ),
    plot.caption = element_textbox(
      width = unit(11.5, "in"),
      fill = "gray80",
      halign = 0.5,
      hjust = 0.5,
      ),
    # Legend
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold",
      margin = margin(b = -0.1, unit = "in")
      ),
    legend.text = element_text(
      face = "italic",
      margin = margin(t = -0.12, unit = "in")
      ),
    legend.key.width = unit(1, "in"),

    # Facets (strips)
    strip.text = element_text(face = "bold"),

  )

# ~ ~ Preview ~ ~ #
tgutil::ggpreview(
  plot = p1,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300
  )
