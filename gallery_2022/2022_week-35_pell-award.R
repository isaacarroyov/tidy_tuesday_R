# Week 35 - Pell Awards
library(dplyr)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)
library(MetBrewer)

# LOAD DATA
pell <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv')
pell <- pell %>% janitor::clean_names()
# En la data-vis intentare complementar el trabajo de @BlakeRobMills
# Seran 3 visualizaciones, todas seran kdplots + jitterplots en una serie de tiempo
# Cada punto sera el AWARD/RECIPIENT para obtener un radio de USD per student
# Con los kde plots se vera la distribucion del dinero por estudiante

# Como se menciono, seran 3 visualizaciones
# 01 -> usd_per_student en los estados, los puntos que seran resaltados serán
# los estados donde haya al menos una Ivy League
# 02 -> usd_per_student en los estados con al menos una Ivy League. Los puntos 
# resaltados seran las propias Ivy League

# NOTA
# En esta visualizacion no podre simplificar todas las universidades, ya que algunas 
# se repiten pero con nombres diferentes. Esto es algo que se tendra
# que tomar en cuenta en la visualizacion

# ------- DATA WRANGLING -------

# 01 - Encontrar los estados donde hayan Ivy Leagues
# Find states that have at least one Ivy
# According to mastersportal.com, the Ivy League universities/colleges are:
# Harvard University (Massachusetts)
# Yale University (Connecticut)
# Princeton University (New Jersey)
# Columbia University (New York)
# Brown University (Rhode Island)
# Dartmouth College (New Hampshire)
# University of Pennsylvania (Pennsylvania)
# Cornell University (New York)

# Filtrar only us states according to states.abb
pell <- pell %>%
  filter(state %in% state.abb)


# CASO 01 -> usd_per_student en us states
# Encontrar los estados donde hayan Ivy Leagues
ivy_league_states <- c("Massachusetts", "Connecticut",
                       "New Jersey","New York", "Rhode Island",
                       "New Hampshire", "New York")
ivy_league_states_abb <- state.abb[match(ivy_league_states,state.name)]


df_usd_per_student_states <- pell %>%
  group_by(state, year) %>%
  summarise(sum_award = sum(award),
            sum_recipient = sum(recipient)) %>%
  ungroup() %>%
  mutate(usd_per_student = sum_award/sum_recipient,
         is_it_ivy = case_when(state %in% ivy_league_states ~ "Ivy",
                               T ~ NA_character_))


# CASO 02 -> usd_per_student en us states donde hay Ivy League
# Code from @BlakeRobMills
ivy_league_unis <-  c("Harvard University", "Columbia University", "Columbia University in the City of New y",
                      "Columbia University in the City of New York","Brown University",
                      "University of Pennsylvania", "Yale University", "Princeton University",
                      "Dartmouth College", "Cornell University")

# Relation between unis and states they belong to
relation_states_ivy_abb <- pell %>%
  filter(state %in% ivy_league_states_abb) %>%
  mutate(name = case_when(name == ivy_league_unis[1] ~ "Harvard",
                          name %in% ivy_league_unis[2:4] ~ "Columbia",
                          name == ivy_league_unis[5] ~ "Brown",
                          name == ivy_league_unis[6] ~ "UPenn",
                          name == ivy_league_unis[7] ~ "Yale",
                          name == ivy_league_unis[8] ~ "Princeton",
                          name == ivy_league_unis[9] ~ "Dartmouth",
                          name == ivy_league_unis[10] ~ "Cornell",
                          T ~ name)) %>%
  select(name, state) %>%
  group_by(name) %>%
  summarise(state_belong = max(state))



df_usd_per_student_states_unis <- pell %>%
  filter(state %in% ivy_league_states_abb) %>%
  mutate(name = case_when(name == ivy_league_unis[1] ~ "Harvard",
                          name %in% ivy_league_unis[2:4] ~ "Columbia",
                          name == ivy_league_unis[5] ~ "Brown",
                          name == ivy_league_unis[6] ~ "UPenn",
                          name == ivy_league_unis[7] ~ "Yale",
                          name == ivy_league_unis[8] ~ "Princeton",
                          name == ivy_league_unis[9] ~ "Dartmouth",
                          name == ivy_league_unis[10] ~ "Cornell",
                          T ~ name)) %>%
  group_by(name, year) %>%
  summarise(sum_award = sum(award),
            sum_recipient = sum(recipient)) %>%
  ungroup() %>%
  mutate(usd_per_student = sum_award/sum_recipient,
         is_it_ivy = case_when(name %in% c("Harvard","Columbia","Brown","UPenn","Yale","Princeton","Dartmouth","Cornell") ~ "Ivy",
                               T ~ NA_character_)) %>%
  left_join(relation_states_ivy_abb, by = "name")


# ------ DATA VISUALIZATION -------
# Colour palette

# Text
title_text <- "Hermosa grafica"
subtitle_text <- "Hermosa descripcion"
caption_text <- "Hermosa caption"

# Typography



# PLOT OF ALL STATES
# set.seed(11)
# p1 <- df %>%
#   ggplot(aes(x=year, y=usd_per_student)) +
#   ggdist::stat_slab(data = df, aes(x=year, y=usd_per_student)) +
#   geom_jitter(color = 'gray90', width = 0.1, size = 0.5) +
#   geom_jitter(data = df %>% filter(is_it_ivy == "Ivy"),
#               aes(x=year, y=usd_per_student, colour = state),
#               width = 0.1) +
#   scale_x_continuous(breaks = seq(1991,2017)) +
#   labs(title = title_text, subtitle = subtitle_text, caption = caption_text) +
#   theme_void() +
#   theme(
#     legend.position = "top",
#   )

