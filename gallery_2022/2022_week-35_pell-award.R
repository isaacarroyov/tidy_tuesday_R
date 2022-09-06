# Week 35 - Pell Awards
library(dplyr)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)
library(MetBrewer)
library(patchwork)

# LOAD DATA
pell <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv')
pell <- pell %>% janitor::clean_names()
# En la data-vis intentare complementar el trabajo de @BlakeRobMills
# Seran 2 visualizaciones, todas seran kdplots + jitterplots en una serie de tiempo
# Cada punto sera el AWARD/RECIPIENT para obtener un radio de USD per student
# Con los kde plots se vera la distribucion del dinero por estudiante

# Como se menciono, seran 2 visualizaciones
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

# Code from @BlakeRobMills
ivy_league_unis <-  c("Harvard University", "Columbia University", "Columbia University in the City of New y",
                      "Columbia University in the City of New York","Brown University",
                      "University of Pennsylvania", "Yale University", "Princeton University",
                      "Dartmouth College", "Cornell University")
ivy_league_unis_abb <- c("Harvard","Columbia","Brown","UPenn","Yale","Princeton","Dartmouth","Cornell")


# Filtrar only us states according to states.abb
pell <- pell %>%
  filter(state %in% state.abb, award > 0, recipient > 0)


# CASO 01 -> usd_per_student en us states
# Encontrar los estados donde hayan Ivy Leagues
ivy_league_states <- c("Massachusetts", "Connecticut",
                       "New Jersey","New York", "Rhode Island",
                       "New Hampshire", "Pennsylvania")
ivy_league_states_abb <- state.abb[match(ivy_league_states,state.name)]


df_usd_per_student_states <- pell %>%
  group_by(state, year) %>%
  mutate(usd_per_student = award/recipient) %>%
  summarise(avg_usd_per_student = mean(usd_per_student)) %>%
  ungroup() %>%
  mutate(is_it_ivy = case_when(state %in% ivy_league_states_abb ~ "Ivy",
                               T ~ NA_character_),
         state = case_when(state == ivy_league_states_abb[1] ~ ivy_league_states[1],
                           state == ivy_league_states_abb[2] ~ ivy_league_states[2],
                           state == ivy_league_states_abb[3] ~ ivy_league_states[3],
                           state == ivy_league_states_abb[4] ~ ivy_league_states[4],
                           state == ivy_league_states_abb[5] ~ ivy_league_states[5],
                           state == ivy_league_states_abb[6] ~ ivy_league_states[6],
                           state == ivy_league_states_abb[7] ~ ivy_league_states[7],
                           T ~ state),
         year = factor(year))


# CASO 02 -> usd_per_student en us states donde hay Ivy League
df_usd_per_student_states_unis <- pell %>%
  filter(state %in% ivy_league_states_abb) %>%
  mutate(name = case_when(name == ivy_league_unis[1] ~ ivy_league_unis_abb[1],
                          name %in% ivy_league_unis[2:4] ~ ivy_league_unis_abb[2],
                          name == ivy_league_unis[5] ~ ivy_league_unis_abb[3],
                          name == ivy_league_unis[6] ~ ivy_league_unis_abb[4],
                          name == ivy_league_unis[7] ~ ivy_league_unis_abb[5],
                          name == ivy_league_unis[8] ~ ivy_league_unis_abb[6],
                          name == ivy_league_unis[9] ~ ivy_league_unis_abb[7],
                          name == ivy_league_unis[10] ~ ivy_league_unis_abb[8],
                          T ~ name),
         state = case_when(state == ivy_league_states_abb[1] ~ ivy_league_states[1],
                           state == ivy_league_states_abb[2] ~ ivy_league_states[2],
                           state == ivy_league_states_abb[3] ~ ivy_league_states[3],
                           state == ivy_league_states_abb[4] ~ ivy_league_states[4],
                           state == ivy_league_states_abb[5] ~ ivy_league_states[5],
                           state == ivy_league_states_abb[6] ~ ivy_league_states[6],
                           state == ivy_league_states_abb[7] ~ ivy_league_states[7],
                           T ~ NA_character_)) %>%
  group_by(state, name, year) %>%
  mutate(usd_per_student = award/recipient) %>%
  summarise(avg_usd_per_student = mean(usd_per_student)) %>%
  ungroup() %>%
  mutate(is_it_ivy = case_when(name %in% ivy_league_unis_abb ~ "Ivy",
                               T ~ NA_character_),
         year = factor(year))

# Checar que todos los numeros de los estados cuadren
pell %>%
  filter(state %in% ivy_league_states_abb) %>%
  group_by(state, year) %>%
  mutate(test = award/recipient) %>%
  summarise(avg_test = mean(test))


df_usd_per_student_states %>%
  filter(state %in% ivy_league_states)


df_usd_per_student_states_unis %>%
  group_by(state, year) %>%
  summarise(avg_test = mean(avg_usd_per_student))



# ------ DATA VISUALIZATION -------
# Colour palette
colour_palette <- met.brewer("Archambault", n = 7)

# Text
# title_text <- "Hermosa grafica"
# subtitle_text <- "Hermosa descripcion"
# caption_text <- "Hermosa caption"

# Typography
font_add(family = 'body_font', regular = './../free_fonts/apfelGrotezk/ApfelGrotezk-Regular.otf')
font_add_google('Playfair Display', 'title_font')
showtext_auto()


body_font <- 'body_font'
title_font <- 'title_font'

theme_set(theme_classic(base_family = body_font))



# PLOT OF ALL STATES
set.seed(11)
p1 <- df_usd_per_student_states %>%
  ggplot(aes(x=year, y=avg_usd_per_student)) +
  ggdist::stat_slab(data = df_usd_per_student_states,
                    aes(x=year, y=avg_usd_per_student),
                    width = 1,
                    trim = F) +
  geom_jitter(color = 'gray90', width = 0.1, size = 0.5) +
  geom_jitter(data = df_usd_per_student_states %>% filter(is_it_ivy == "Ivy"),
              aes(x=year, y=avg_usd_per_student, colour = state),
              width = 0.1) +
  geom_textbox(data = tribble(~x,~y,~label,
                              factor(2010), 2500, "This chart showcases the average amount of dollars per recipient a State received -from Pell Grants- in a given year. The highlighted dots are the states where there is at least one Ivy League school.",
                              factor(2000), 4500, "The list of Ivy League schools includes some of the oldest educational institutions, with well-respected professors, generous research grants and significant financial aid resources: <span style='color:#88A0DC'><b>Connecticut</b></span>, <span style='color:#381A61'><b>Massachusetts</b></span>, <span style='color:#7C4C73'><b>New Hampshire</b></span>, <span style='color:#ED968B'><b>New Jersey</b></span>, <span style='color:#AB3329'><b>New York</b></span>, <span style='color:#E78429'><b>Pennsylvania</b></span> and <span style='color:#F9D14A'><b>Rhode Island</b></span>"),
               aes(x=x,y=y,label=label),
               halign = 0, hjust = 0,
               fill = 'transparent',
               ) +
  scale_colour_manual(values = colour_palette) +
  coord_cartesian(ylim = c(1500,5000)) #+ labs(title = title_text, subtitle = subtitle_text, caption = caption_text) +


p1 +
  theme(
    legend.position = "none",
    # Background
    plot.background = element_rect(fill = '#FEFBF8'),
    panel.background = element_blank(),
    # Title
    # Subtitle
    # Caption
    # Axis (text)
    # Axis (lines)
    # Grid
  )

set.seed(11)
df_usd_per_student_states_unis %>%
  ggplot(aes(x = year, y = avg_usd_per_student, colour = state)) +
  geom_jitter(width = 0.1, size = 0.1, alpha = 0.1) +
  # stat_summary(func = 'mean', geom = "point", size = 1, colour = 'black') +
  geom_point(data = df_usd_per_student_states %>% filter(is_it_ivy=='Ivy'),
             aes(x=year, y=avg_usd_per_student),
             colour = 'black',
             size = 1
             ) +
  scale_colour_manual(values = colour_palette) +
  coord_cartesian(ylim = c(1500,5000)) +
  facet_wrap(~state, nrow = 2) +
  theme(
    legend.position = "none"
  )


# Agregar labels de Ivy 
df_usd_per_student_states_unis %>%
  mutate(label_ivy = case_when(is_it_ivy == "Ivy" ~ name,
                               T ~ NA_character_)) %>%
  ggplot(aes(x = year, y = avg_usd_per_student, colour = state)) +
  geom_jitter(width = 0.1, size = 0.1, alpha = 0.1) +
  geom_point(data = df_usd_per_student_states %>% filter(is_it_ivy=='Ivy'),
             aes(x=year, y=avg_usd_per_student),
             colour = 'black',
             size = 1) +
  ggrepel::geom_label_repel(aes(label=label_ivy)) +
  scale_colour_manual(values = colour_palette) +
  coord_cartesian(ylim = c(1500,5000)) +
  facet_wrap(~state, nrow = 2) +
  theme(
    legend.position = "none"
  )


