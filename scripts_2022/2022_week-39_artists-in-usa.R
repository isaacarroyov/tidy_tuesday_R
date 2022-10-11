# Week 39 - Artist in the USA
# Author: Isaac Arroyo

# ------ LOAD PACKAGES ------
library(tidyverse)
library(ggtext)
# remotes::install_github("AllanCameron/geomtextpath")
library(geomtextpath) # TIL
library(sysfonts)
library(showtextdb)
library(showtext)
library(MetBrewer)

# ------ LOAD DATA ------
artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')

# ------ EXPLORE DATA ------
glimpse(artists)

# ------ DATA WRANGLING ------
artists_cat <- artists %>%
  filter(race == "Hispanic") %>%
  replace_na(list(artists_n = 0, location_quotient = 0)) %>%
  mutate(art_expression = case_when(type %in% c("Designers", "Fine Artists, Art Directors, And Animators", "Photographers") ~ "Visual",
                                    type %in% c("Architects","Landscape Architects") ~ "Architecture",
                                    type %in% c("Writers And Authors") ~ "Literature",
                                    type %in% c("Musicians", "Music Directors And Composers") ~ "Music",
                                    type %in% c("Dancers And Choreographers") ~ "Dance",
                                    type %in% c("Producers And Directors", "Entertainers","Actors", "Announcers") ~ "Performance",
                                    T ~ "Another art expression"),
         art_expression = case_when(art_expression == "Architecture" ~ "**Architecture** (_Architects and Landscape Architects)_",
                                    art_expression == "Dance" ~ "**Dance** (_Dancers and Choreographers_)",
                                    art_expression == "Literature" ~ "**Literature** (_Writers and Authors_)",
                                    art_expression == "Music" ~ "**Music** (_Musicians, Music Directors and Composers_)",
                                    art_expression == "Performance" ~ "**Performance** (_Actors, Announcers, Entertainers, Producers and Directors_)",
                                    art_expression == "Visual" ~ "**Visual** (_Designers, Photographers, Fine Artists, Art Directors and Animators_)",
                                    T ~ "Another art expression"))

df_percentage_of_hispanic_artists <- artists_cat %>%
  group_by(state, art_expression) %>%
  summarise(
    all_workers = max(all_workers_n),
    number_of_artists = sum(artists_n),
    percentage_of_artists = 100 * (number_of_artists / all_workers)
  ) %>% ungroup() %>% rename(state_usa = state)

# ------ DATA VISUALIZATION ------
# Text
title_text <- "Where the artistas hispanos at?"
subtitle_text <- "Hispanic artists represent less than 1% of the state's workers on average; however, some places are **THE PLACE** for artists: **New York** and **California**.<br>&bull; **New York** has the highest proportion of hispanic artists among the workers, with approximately 1.5% (27,240 artists out of 1,819,490 workers).<br>&bull; **California** has the highest number of hispanic artists, with 74,650."
caption_text <- "Designed by Isaac Arroyo (@unisaacarroyov on twitter)<br>#TidyTuesday Week 39: (Hispanic) Artists in the USA.<br>Data source: _Artists in the Workforce: National and State Estimates for 2015-2019_ via arts.gov"


# Typography
font_add_google("Lora", "title_font")
font_add_google("Lato", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# Settings
theme_set(theme_void())
theme_update(
  # General plot
  plot.margin = unit(rep(0,4), "in"),
  # Title
  plot.title.position = "plot",
  plot.title = element_textbox(family = title_font,
                               lineheight = 0.2,
                               size = rel(8),
                               face = 'bold',
                               halign = 0.5,
                               hjust = 0.5,
                               width = unit(8,"in"),
                               padding = margin(0.125,0,0,0,"in"),
                               margin = margin(0,0,0,0,"in")),
  # Subtitle
  plot.subtitle = element_textbox(family = body_font,
                                  lineheight = 0.4,
                                  size = rel(3.3),
                                  halign = 0,
                                  hjust = 0.5,
                                  width = unit(8,"in"),
                                  padding = margin(0.125,0.125,0.3,0.125,"in"),
                                  margin = margin(0,0,0,0,"in")),
  # Legend
  legend.position = "top",
  legend.title = element_markdown(family = title_font,
                                  lineheight = 0.2,
                                  size = rel(4),
                                  face = "bold",
                                  halign = 0,
                                  hjust = 0.5),
  legend.text = element_textbox(family = body_font,
                                lineheight = 0.33,
                                size = rel(2.5),
                                valign = 0.5,
                                width = unit(2,"in"),
                                margin = margin(0,0,0.0625,0,"in")),
  legend.spacing.x = unit(0.125,"in"),
  legend.spacing.y = unit(0.0625, "in"),
  # Caption
  plot.caption.position = "plot",
  plot.caption = element_textbox(family = body_font,
                                 lineheight = 0.4,
                                 size = rel(2),
                                 halign = 0.5,
                                 hjust = 0.5,
                                 width = unit(8,"in"),
                                 padding = margin(0,0,0.125,0,"in"),
                                 margin = margin(0,0,0,0,"in")),
  # Axis
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
)

# ------ Data viz (% of workers) ------
p1 <- df_percentage_of_hispanic_artists %>%
  ggplot(aes(x = state_usa, y = percentage_of_artists, fill = art_expression)) +
  geom_texthline(label = "1% of workers",
                 family = body_font,
                 spacing = -250,
                 size = 7, 
                 colour = 'gray70',
                 textcolour = 'gray70',
                 yintercept = 1,
                 lineend = "round",
                 linetype = 'dashed') + 
  geom_col() +
  geom_textpath(aes(label = state_usa, y = 1.75 , angle = 90),
                family = title_font,
                spacing = -280,
                size = 7) +
  ylim(c(-0.2, 1.75)) +
  guides(fill = guide_legend(title = "Art Expression", title.position = "top")) +
  scale_fill_manual(values = met.brewer(name = "Juarez", n = 6)) +
  coord_polar(theta = "x", start = 0) +
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text)

ggsave(filename = "./gallery_2022/2022_week-39_artists-in-usa.png",
       plot = p1,
       width = 8.5, height = 11, units = "in", bg = "#FAFBFC")



# ------ IGNORE IT (tried to make an alluvial/sankey)
# library(ggsankey)
# install.packages("ggalluvial")
# library(ggalluvial)
# 
# ------ DATA WRANGLING (focused on alluvial/sankey)------
# Note: a concentration_labor_force >= 1 is that the state's labor force in an occupation
#       is +100 percent greater than the occupation's national labor force share
# df_hispanic_artists <- artists %>%
#   filter(race == 'Hispanic') %>%
#   drop_na(artists_share, location_quotient) %>%
#   mutate(artists_share_perc = artists_share * 100,
#          concentration_labor_force = (location_quotient - 1)*100) %>%
#   select(state, type, artists_share_perc, concentration_labor_force) %>%
#   # Group into 6-7 big categories: "Art expression"
#   mutate(art_expression = case_when(type %in% c("Designers", "Fine Artists, Art Directors, And Animators", "Photographers") ~ "Visual",
#                                     type %in% c("Architects","Landscape Architects") ~ "Architecture",
#                                     type %in% c("Writers And Authors") ~ "Literature",
#                                     type %in% c("Musicians", "Music Directors And Composers") ~ "Music",
#                                     type %in% c("Dancers And Choreographers") ~ "Dance",
#                                     type %in% c("Producers And Directors", "Entertainers","Actors", "Announcers") ~ "Performance",
#                                     T ~ "Another art expression"),
#          art_expression = case_when(art_expression == "Architecture" ~ "Architecture (Architects and Landscape Architects)",
#                                            art_expression == "Dance" ~ "Dance (Dancers and Choreographers)",
#                                            art_expression == "Literature" ~ "Literature (Writers and Authors)",
#                                            art_expression == "Music" ~ "Music (Musicians, Music Directors and Composers)",
#                                            art_expression == "Performance" ~ "Performance (Actors, Announcers, Entertainers, Producers and Directors)",
#                                            art_expression == "Visual" ~ "Visual (Designers, Photographers, Fine Artists, Art Directors and Animators)",
#                                            T ~ "Another art expression"),
#          # Discretize % of artists in the workforce : "Percentage of all workers"
#          artists_share_perc_bins = case_when(artists_share_perc < 0.2 ~ 1,
#                                              artists_share_perc >= 0.2 & artists_share_perc <= 0.4 ~ 2,
#                                              artists_share_perc > 0.4 & artists_share_perc <= 0.6 ~ 3,
#                                              artists_share_perc > 0.6 & artists_share_perc <= 0.8 ~ 4,
#                                              T ~ 5),
#          artists_share_perc_bins = ordered(artists_share_perc_bins,
#                                            levels = c(1,2,3,4,5),
#                                            labels = c("Less than 0.2% of all workers in the state",
#                                                       "Between 0.2% - 0.4% of all workers in the state",
#                                                       "Between 0.4% - 0.6% of all workers in the state",
#                                                       "Between 0.6% - 0.8% of all workers in the state",
#                                                       "Between 0.8% - 1% of all workers in the state")),
#          # Discretize concentration_labor_force: "Percentage of occupation's national labor force share. Negative values mean less than the national labor force share and positive the opposite"
#          concentration_labor_force_bins = case_when(concentration_labor_force <= -50 ~ 1,
#                                                     concentration_labor_force > -50 & concentration_labor_force <= 0 ~ 2,
#                                                     concentration_labor_force > 0 & concentration_labor_force <= 50 ~ 3,
#                                                     concentration_labor_force > 50 & concentration_labor_force <= 100 ~ 4,
#                                                     T ~ 5),
#          concentration_labor_force_bins = ordered(concentration_labor_force_bins,
#                                                   levels = c(1,2,3,4,5),
#                                                   labels = c("Between 50% and 100% <b>less</b> than the national labor force share",
#                                                              "Between 0% and 50% <b>less</b> than the national labor force share",
#                                                              "Between 0% and 50% <b>more</b> than the national labor force share",
#                                                              "Between 50% and 100% <b>more</b> than the national labor force share",
#                                                              "<b>More</b> than 100% of the national labor force share")))
# 
# 
# ------ DATA VISUALIZATION (ggalluvial) ------
# ------ specific changes for dataviz ------
# df_hispanic_artists_alluvial_all <- df_hispanic_artists %>%
#   count(artists_share_perc_bins, art_expression, concentration_labor_force_bins) %>%
#   rename(freq = n)
# 
# df_hispanic_artists_alluvial_all %>%
#   ggplot(aes(y = freq,
#              axis1 = artists_share_perc_bins,
#              axis2 = art_expression,
#              axis3 = concentration_labor_force_bins)) +
#   geom_alluvium(aes(fill = art_expression),
#                 width = 0.5, alpha = 0.55) +
#   geom_stratum(width = 0.5, alpha = 0.4) +
#   geom_textbox(stat = 'stratum',
#                aes(label = after_stat(stratum))) +
#   theme_void() +
#   theme(
#     legend.position = "none"
#   )
  
# ------ DATA VISUALIZATION (ggsankey) ------
# df_hispanic_artists_sankey <- df_hispanic_artists %>%
#   make_long(artists_share_perc_bins, art_expression, concentration_labor_force_bins)
# 
# 
# df_hispanic_artists_sankey %>%
#   ggplot(aes(x = x, 
#              next_x = next_x, 
#              node = node, 
#              next_node = next_node,
#              fill = node
#   )) +
#   geom_sankey(space = 25) +
#   geom_sankey_label(aes(label=node)) +
#   scale_fill_manual(values = colours_fill)
