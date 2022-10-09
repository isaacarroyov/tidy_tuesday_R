# Week 39 - Artist in the USA
# Author: Isaac Arroyo

# ------ LOAD PACKAGES ------
library(tidyverse)
library(ggtext)
# library(ggsankey)
# install.packages("ggalluvial")
library(ggalluvial)
library(sysfonts)
library(showtextdb)
library(showtext)
library(MetBrewer)

# ------ LOAD DATA ------
artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')

# ------ EXPLORE DATA ------
glimpse(artists)

# ------ DATA WRANGLING ------
# Note: a concentration_labor_force >= 1 is that the state's labor force in an occupation
#       is +100 percent greater than the occupation's national labor force share
df_hispanic_artists <- artists %>%
  filter(race == 'Hispanic') %>%
  drop_na(artists_share, location_quotient) %>%
  mutate(artists_share_perc = artists_share * 100,
         concentration_labor_force = (location_quotient - 1)*100) %>%
  select(state, type, artists_share_perc, concentration_labor_force) %>%
  # Group into 6-7 big categories: "Art expression"
  mutate(art_expression = case_when(type %in% c("Designers", "Fine Artists, Art Directors, And Animators", "Photographers") ~ "Visual",
                                    type %in% c("Architects","Landscape Architects") ~ "Architecture",
                                    type %in% c("Writers And Authors") ~ "Literature",
                                    type %in% c("Musicians", "Music Directors And Composers") ~ "Music",
                                    type %in% c("Dancers And Choreographers") ~ "Dance",
                                    type %in% c("Producers And Directors", "Entertainers","Actors", "Announcers") ~ "Performance",
                                    T ~ "Another art expression"),
         art_expression = case_when(art_expression == "Architecture" ~ "Architecture (Architects and Landscape Architects)",
                                           art_expression == "Dance" ~ "Dance (Dancers and Choreographers)",
                                           art_expression == "Literature" ~ "Literature (Writers and Authors)",
                                           art_expression == "Music" ~ "Music (Musicians, Music Directors and Composers)",
                                           art_expression == "Performance" ~ "Performance (Actors, Announcers, Entertainers, Producers and Directors)",
                                           art_expression == "Visual" ~ "Visual (Designers, Photographers, Fine Artists, Art Directors and Animators)",
                                           T ~ "Another art expression"),
         # Discretize % of artists in the workforce : "Percentage of all workers"
         artists_share_perc_bins = case_when(artists_share_perc < 0.2 ~ 1,
                                             artists_share_perc >= 0.2 & artists_share_perc <= 0.4 ~ 2,
                                             artists_share_perc > 0.4 & artists_share_perc <= 0.6 ~ 3,
                                             artists_share_perc > 0.6 & artists_share_perc <= 0.8 ~ 4,
                                             T ~ 5),
         artists_share_perc_bins = ordered(artists_share_perc_bins,
                                           levels = c(1,2,3,4,5),
                                           labels = c("Less than 0.2% of all workers in the state",
                                                      "Between 0.2% - 0.4% of all workers in the state",
                                                      "Between 0.4% - 0.6% of all workers in the state",
                                                      "Between 0.6% - 0.8% of all workers in the state",
                                                      "Between 0.8% - 1% of all workers in the state")),
         # Discretize concentration_labor_force: "Percentage of occupation's national labor force share. Negative values mean less than the national labor force share and positive the opposite"
         concentration_labor_force_bins = case_when(concentration_labor_force <= -50 ~ 1,
                                                    concentration_labor_force > -50 & concentration_labor_force <= 0 ~ 2,
                                                    concentration_labor_force > 0 & concentration_labor_force <= 50 ~ 3,
                                                    concentration_labor_force > 50 & concentration_labor_force <= 100 ~ 4,
                                                    T ~ 5),
         concentration_labor_force_bins = ordered(concentration_labor_force_bins,
                                                  levels = c(1,2,3,4,5),
                                                  labels = c("Between 50% and 100% <b>less</b> than the national labor force share",
                                                             "Between 0% and 50% <b>less</b> than the national labor force share",
                                                             "Between 0% and 50% <b>more</b> than the national labor force share",
                                                             "Between 50% and 100% <b>more</b> than the national labor force share",
                                                             "<b>More</b> than 100% of the national labor force share")))


# ------ IGNORE IT (tried to make an alluvial/sankey)
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
