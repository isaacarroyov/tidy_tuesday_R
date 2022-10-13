library(dplyr)
library(ggplot2)
library(MetBrewer)
# install.packages('ggbeeswarm')
library(ggbeeswarm)
library(ggtext)
library(sysfonts)
library(showtext)


# ------ LOAD DATA ------
yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

# ------ DATAVIZ IDEA ------
# Back to hacer algo raro:
# No tengo ni la mas minima idea sobre el tema de yarn, sin embargo es una perfecta 
# oportunidad para hacer una visualizacion poco convencional nada mas por el gusto
# de crear algo.

# ------ DATA WRANGLING ------
df <- yarn %>%
  select(yarn_company_name, rating_average, rating_count,yarn_weight_name) %>%
  tidyr::drop_na()
  # select(yarn_company_name, rating_average, rating_count, grams, yardage, yarn_weight_wpi, yarn_weight_name)

# ------ (creative) DATA VISUALIZATION ------
df_dataviz <- df %>%
  group_by(yarn_company_name, yarn_weight_name) %>%
  summarise(avg_rating_average = mean(rating_average),
            avg_rating_count = mean(rating_count)) %>%
  ungroup()

# Typography
font_add_google("Vollkorn", "title_font")
font_add_google("Raleway", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# Text
title_text <- "Dots and yarn data"
subtitle_text <- "There are no mistakes, only happy accidents.<br>Bob Ross"
caption_text <- "Almost the entire data visualization (the use of points and the colour palette) is inspired by the painting **Notre-Dame-de-la-Garde by Paul Signac** due to the _Pointillist style_.<br>Designed by Isaac Arroyo (@unisaacarroyov on twitter).<br>#TidyTuesday Week 41: Ravelry Yarn via **{ravelRy}**"

# Chart
p1 <- df_dataviz %>%
  ggplot(aes(x = avg_rating_count, y = avg_rating_average, colour = yarn_weight_name)) +
  geom_point(alpha = 0.75) +
  scale_x_log10(limits =c(1,1e5)) +
  scale_colour_manual(values = met.brewer(name = "Signac", n = 15)) +
  coord_polar() +
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text) +
  theme_void() +
  theme(
    legend.position = "none",
    text = element_text(colour = "gray90"),
    # Title
    plot.title.position = "plot",
    plot.title = element_textbox(family = title_font,
                                 face = 'bold',
                                 lineheight = 0.2,
                                 size = rel(10),
                                 width = unit(8,"in"),
                                 margin = margin(0.125,0,0.125,0,"in"),
                                 hjust = 0.5,
                                 halign = 0.5),
    # Subtitle
    plot.subtitle = element_textbox(family = body_font,
                                    face = 'italic',
                                    lineheight = 0,
                                    size = rel(4),
                                    width = unit(8,"in"),
                                    margin = margin(0,0,-0.5,0,"in"),
                                    hjust = 0.5,
                                    halign = 0.5),
    # Caption
    plot.caption.position = "plot",
    plot.caption = element_textbox(family = body_font,
                                   lineheight = 0.37,
                                   size = rel(2),
                                   width = unit(4,"in"),
                                   margin = margin(-0.5,0,0.125,0,'in'),
                                   hjust = 0.5,
                                   halign = 0.5)
  )
ggsave(filename = "./gallery_2022/2022_week-41_ravelry-yarn.png",
       plot = p1, bg = "#07152A",
       width = 8.5, height = 8.5, units = "in")

