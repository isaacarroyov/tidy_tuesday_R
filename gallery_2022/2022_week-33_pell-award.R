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
  

# 02 - Encontrar los estados donde hayan Ivy Leagues
ivy_league_states <- c("Massachusetts", "Connecticut",
                       "New Jersey","New York", "Rhode Island",
                       "New Hampshire", "New York")
ivy_league_states_abb <- state.abb[match(ivy_league_states,state.name)]



# df <- df_usd_per_student_states %>%
#   filter(state %in% state.abb) %>%
#   mutate(is_it_ivy = case_when(state %in% ivy_league_states_abb ~ "Ivy",
#                                T ~ NA_character_))
# 
# df_usd_per_student_states <- pell %>%
#   group_by(state, year) %>%
#   summarise(sum_award = sum(award),
#             sum_recipient = sum(recipient)) %>%
#   ungroup() %>%
#   mutate(usd_per_student = sum_award/sum_recipient)
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

