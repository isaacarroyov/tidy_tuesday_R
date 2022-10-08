# Week 39 - Artist in the USA
# Author: Isaac Arroyo

# ------ LOAD PACKAGES ------
library(tidyverse)
library(ggtext)
library(ggsankey)
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
         # Discretize % of artists in the workforce : "Percentage of all workers"
         artists_share_perc_bins = case_when(artists_share_perc <= 0.2 ~ 1,
                                             artists_share_perc > 0.2 & artists_share_perc <= 0.4 ~ 2,
                                             artists_share_perc > 0.4 & artists_share_perc <= 0.6 ~ 3,
                                             artists_share_perc > 0.6 & artists_share_perc <= 0.8 ~ 4,
                                             T ~ 5),
         artists_share_perc_bins = ordered(artists_share_perc_bins,
                                           levels = c(1,2,3,4,5),
                                           labels = c("≤ 0.2%", "0.2% - 0.4%", "0.4% - 0.6%", "0.6% - 0.8%", "0.8% - 1%")),
         # Discretize concentration_labor_force: "Percentage of occupation's national labor force share. Negative values mean less than the national labor force share and positive the opposite"
         concentration_labor_force_bins = case_when(concentration_labor_force <= -50 ~ 1,
                                                    concentration_labor_force > -50 & concentration_labor_force <= 0 ~ 2,
                                                    concentration_labor_force > 0 & concentration_labor_force <= 50 ~ 3,
                                                    concentration_labor_force > 50 & concentration_labor_force <= 100 ~ 4,
                                                    T ~ 5),
         concentration_labor_force_bins = ordered(concentration_labor_force_bins,
                                                  levels = c(1,2,3,4,5),
                                                  labels = c("Between -100% and -50%",
                                                             "Between -50% and 0%",
                                                             "Between 0% and 50%",
                                                             "Between 50% and 100%",
                                                             "More than 100%")))


# ------ DATA VISUALIZATION (ggsankey) ------
df_hispanic_artists_sankey <- df_hispanic_artists %>%
  make_long(artists_share_perc_bins, art_expression, concentration_labor_force_bins)

artist_share_perc_colours <- c(met.brewer(name = "VanGogh3", n = 5))
art_expression_colours <- c(met.brewer("Archambault", n = 6))
concentration_labor_force_colours <- c(met.brewer(name = "OKeeffe1", n = 5))

colours_fill <- c("≤ 0.2%" = artist_share_perc_colours[1],
                  "0.2% - 0.4%" = artist_share_perc_colours[2],
                  "0.4% - 0.6%" = artist_share_perc_colours[3],
                  "0.6% - 0.8%" = artist_share_perc_colours[4],
                  "0.8% - 1%" = artist_share_perc_colours[5],
                  "Architecture" = art_expression_colours[1],
                  "Visual" = art_expression_colours[2],
                  "Performance" = art_expression_colours[3],
                  "Dance" = art_expression_colours[4],
                  "Music" = art_expression_colours[5],
                  "Literature" = art_expression_colours[6],
                  "Between -100% and -50%" = concentration_labor_force_colours[1],
                  "Between -50% and 0%" = concentration_labor_force_colours[2],
                  "Between 0% and 50%" = concentration_labor_force_colours[3],
                  "Between 50% and 100%" = concentration_labor_force_colours[4],
                  "More than 100%" = concentration_labor_force_colours[5])

df_hispanic_artists_sankey %>%
  ggplot(aes(x = x, 
             next_x = next_x, 
             node = node, 
             next_node = next_node,
             fill = node
  )) +
  geom_sankey(space = 25) +
  geom_sankey_label(aes(label=node)) +
  scale_fill_manual(values = colours_fill)
