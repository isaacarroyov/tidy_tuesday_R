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
title_text_01 <- "Pell Grants: Where are the Ivy League schools?"
subtitle_text_01 <- "First, for those unfamiliar with these terms (like me), let's start with <b>Ivy League</b>. These universities/colleges are considered the most sought-after institutions of higher learning in the country and worldwide. Ivy League schools have been known for their highly selective admissions process, academic excellence and promising career opportunities for those who attend (well-rounded student-athletes, future presidents, Nobel Prize winners and other high-achieving graduates).<br><br>Secondly, The <b>Pell Grant</b> is a form of need-based federal financial aid that typically does not have to be repaid, which makes it highly desirable. It is awarded by the U.S. Department of Education to help eligible low-income students pay for college costs, including tuition, fees, room and board, and other educational expenses.<br><br>The data visualization showcases what public data informs about the money the universities/colleges (who received Pell Grants) and states get and the number of recipients, focusing on the Ivy League schools."
subtitle_text_02 <- "The following chart expands the view of the previous one by displaying the distribution of the average amount of dollars per recipient -from Pell Grants- the universities/colleges received in a given year. The bigger dots are the ones from the previous visualization (the average amount of dollars per recipient a U.S. State received in a given year). The highlighted points are the Ivy League schools."
caption_text <- "_Note:_ Due to the extensive and diverse names of schools, simplifying them was laborious. So, some schools are written differently, leading to unprecise averages. It was easier to focus on Ivy League schools.<br><br>Designed by Isaac Arroyo (@unisaacarroyov on twitter).<br>#TidyTuesday Week 35: Pell Award.<br>Data source: US Department of Education"



# Typography
font_add(family = 'body_font', regular = './../free_fonts/apfelGrotezk/ApfelGrotezk-Regular.otf')
font_add(family = 'title_font', regular = './../free_fonts/Faune_Fonts/Otf/Faune-TextRegular.otf')
showtext_auto()


body_font <- 'body_font'
title_font <- 'title_font'


# ------ Theme settings ------
theme_set(theme_classic(base_family = body_font))
theme_update(
  legend.position = "none",
  # Background
  plot.background = element_rect(fill = '#FFFFFF'),
  panel.background = element_blank(),
  # Title
  plot.title.position = "plot",
  plot.title = element_textbox(family = title_font, face = 'bold',
                               size = rel(9),
                               width = unit(10,'in'),
                               padding = margin(0,0,0,0),
                               margin = margin(t =0.25, b = 0, unit = 'in')),
  # Subtitle
  plot.subtitle = element_textbox(size = rel(3.5),
                                  lineheight = 0.4,
                                  width = unit(10,'in'),
                                  padding = margin(0,0,0,0),
                                  margin = margin(t = 0.35, b = 0.35, unit = 'in')),
  # Caption
  plot.caption.position = "plot",
  plot.caption = element_textbox(face = 'bold',
                                 size = rel(2.7),
                                 lineheight = 0.4,
                                 width = unit(6.5,'in'),
                                 halign = 0.5,
                                 hjust = 0.5,
                                 padding = margin(0,0,0,0),
                                 margin = margin(t = 20, b = 10)),
  # Axis (text)
  axis.title = element_blank(),
  axis.text = element_markdown(size = rel(3), lineheight = 0.3),
  axis.text.x = element_markdown(margin = margin(t = 3)),
  axis.text.y = element_markdown(margin = margin(r = 3)),
  # Axis (lines and ticks)
  axis.ticks.length = unit(4,'pt'),
  # Grid
  panel.grid.major.y = element_line(size = 0.15, linetype = 'dashed', colour = 'gray80'),
  # Facets
  strip.background = element_blank(),
  strip.text = element_markdown(face = 'bold', family = title_font,
                                halign = 0.5, hjust = 0.5,
                                size = rel(4))
  )


# ------ P0 ------
p0 <- ggplot() +
  geom_textbox(data = tribble(~x,~y,~label,
                           -0.15,0,subtitle_text_01),
            aes(x=x,y=y,label=label),
            size = rel(13),
            family = body_font,
            lineheight = 0.4,
            fill = 'white',
            box.colour = 'white',
            width = unit(3.7,'in'),
            halign = 0, hjust =0, valign = 1, vjust = 1,
            box.padding = margin(0,0,0,0),
            box.margin = margin(0,0,0,0),
            ) +
  coord_cartesian(ylim = c(-0.25,0), xlim = c(0,1), clip = "off") +
  theme_void(base_family = body_font) +
  theme(
    # Title
    plot.title.position = "plot",
    plot.title = element_textbox(family = title_font, face = 'bold',
                                 size = rel(9),
                                 lineheight = 0.2,
                                 width = unit(5,'in'),
                                 padding = margin(0,0,0,0),
                                 margin = margin(t =0.25, unit = 'in')),
    )




# ------ P1 ------
set.seed(11)
p1 <- df_usd_per_student_states %>%
  ggplot(aes(x=year, y=avg_usd_per_student)) +
  ggdist::stat_slab(data = df_usd_per_student_states,
                    aes(x=year, y=avg_usd_per_student),
                    width = 1,
                    alpha = 1,
                    fill = '#CBB289',
                    trim = F) +
  geom_jitter(color = 'gray40', width = 0.1, size = 0.2) +
  geom_jitter(data = df_usd_per_student_states %>% filter(is_it_ivy == "Ivy"),
              aes(x=year, y=avg_usd_per_student, colour = state),
              width = 0.1) +
  geom_textbox(data = tribble(~x,~y,~label,
                              factor(2010), 2750, "This chart showcases the average amount of dollars per recipient a U.S. State received -from Pell Grants- in a given year. The highlighted dots are the states where there is at least one Ivy League school.",
                              factor(2000), 4750, "The list of Ivy League schools includes some of the oldest educational institutions, with well-respected professors, generous research grants and significant financial aid resources: <span style='color:#88A0DC'><b>Connecticut</b></span>, <span style='color:#381A61'><b>Massachusetts</b></span>, <span style='color:#7C4C73'><b>New Hampshire</b></span>, <span style='color:#ED968B'><b>New Jersey</b></span>, <span style='color:#AB3329'><b>New York</b></span>, <span style='color:#E78429'><b>Pennsylvania</b></span> and <span style='color:#DAA807'><b>Rhode Island</b></span>"),
               aes(x=x,y=y,label=label),
               halign = 0, hjust = 0, valign = 1, vjust = 1,
               fill = '#FFFDFC',
               box.colour = '#000000',
               box.r = unit(5,'pt'),
               box.padding = margin(8,8,8,8),
               family = body_font,
               size = rel(10),
               lineheight = 0.4,
               width = unit(2,'in'),
               ) +
  scale_colour_manual(values = colour_palette) +
  scale_y_continuous(breaks = seq(1500,5000,500), labels = scales::label_dollar(big.mark = ',', suffix = "<br>per student")) +
  scale_x_discrete(labels = c("'99",paste0("'0",seq(0,9)),paste0("'",seq(10,17)))) +
  coord_cartesian(ylim = c(1500,5000), clip = 'off')

# # ------ P2 ------
set.seed(11)
p2 <- df_usd_per_student_states_unis %>%
  ggplot(aes(x = year, y = avg_usd_per_student)) +
  geom_jitter(aes(colour = state), width = 0.1,
              size = 1,
              shape = 21,
              colour = 'gray90',
              fill = 'white',
              alpha = 0.1) +
  geom_jitter(data = df_usd_per_student_states_unis %>% filter(is_it_ivy=='Ivy', name != "Cornell"),
              aes(x = year, y = avg_usd_per_student, colour = state),
              width = 0.1,
              shape = 21,
              fill = 'white',
              stroke = 0.75,
              size = 1) +
  geom_jitter(data = df_usd_per_student_states_unis %>% filter(is_it_ivy=='Ivy', name == "Cornell"),
              aes(x = year, y = avg_usd_per_student, colour = state),
              width = 0.1,
              shape = 23,
              fill = 'white',
              stroke = 0.75,
              size = 1) +
  geom_point(data = df_usd_per_student_states %>% filter(is_it_ivy=='Ivy'),
             aes(x=year, y=avg_usd_per_student, colour = state),
             size = 2) +
  ggrepel::geom_label_repel(data = df_usd_per_student_states_unis %>% filter(year == 2009, !is.na(is_it_ivy)),
                            aes(label=name, colour = state),
                            family = body_font,
                            size = 10,
                            fontface = 'bold',
                            seed = 1,
                            force_pull = 0.1,
                            force = 100,
                            min.segment.length = 0,
                            direction = "both",
                            label.padding = 0.15,
                            point.padding = 0.25,
                            box.padding = 0.15,
                            nudge_x = -0.5,
                            ) +
  scale_colour_manual(values = colour_palette) +
  scale_fill_manual(values = colour_palette) +
  scale_y_continuous(breaks = seq(1500,5000,500), labels = scales::label_dollar(big.mark = ',', suffix = "<br>per student")) +
  scale_x_discrete(labels = c("'99",paste0("'0",seq(0,9)),paste0("'",seq(10,17)))) +
  coord_cartesian(ylim = c(1500,5000), clip = 'on') +
  facet_wrap(~state, nrow = 2, scales = "free_x") +
  labs(subtitle = subtitle_text_02, caption = caption_text) +
  theme(
    axis.text = element_markdown(size = rel(1.7)),
    plot.subtitle = element_textbox(margin = margin(t = 10, b = 15, l = 12,))
  )
  


final_01 <- (p0 | p1) + plot_layout(widths = c(0.37,0.6))
final <- final_01/p2 + 
  plot_layout(heights = c(0.45,0.55)) +
  plot_annotation(title = title_text_01)


ggsave(filename = "./gallery_2022/2022_week-35_pell-award.png",
       plot = final,
       width = 11, height = 15, units = "in",
       dpi = 300)
