# #TidyTuesday Week 40: Product Hunt
library(tidyverse)
library(lubridate)
library(ggtext)
library(MetBrewer)
library(cowplot)
library(geomtextpath)
library(sysfonts)
library(showtext)

# ------ LOAD DATA ------
product_hunt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv')
glimpse(product_hunt)

# ------ DATA VIZ IDEAS ------
# La visualizacion sera una combinacion de dos
# 1a - Line charts de # de releases por semana para mostrar el gran cambio en 2017
# 2a - Una bar chart que se enfoque en los tags del 2017

# ------ DATA WRANGLING ------
# Forma de la tabla
# category_tag | decripcion del producto con ese tag (no importa que se repita abajo) | Fecha en la que se publico el producto
df <- product_hunt %>%
  select(category_tags, product_description, release_date) %>%
  drop_na(product_description) %>%
  mutate(category_tags = str_remove_all(category_tags, pattern = "[''\\[\\]]"),
         category_tags = str_split(string = category_tags, pattern = ","),
         release_date = ymd(release_date))

# ------ DATA WRANGLING (data viz 1) ------
df_dataviz_1 <- df %>%
  mutate(week_of_year = week(release_date),
         year_date = year(release_date)) %>%
  count(year_date, week_of_year) %>%
  filter(week_of_year != 53) %>%
  rename(number_of_releases = n)

# ------ DATA WRANGLING (data viz 2) ------
# Focusing on 2017 weeks 33, 34 and 35
df_dataviz_2 <- df %>%
  mutate(date_year = year(release_date),
         week_of_year = week(release_date)) %>%
  filter(date_year == 2017,
         week_of_year %in% 33:35) %>%
  unnest_longer(category_tags) %>%
  mutate(category_tags = str_squish(category_tags),
         category_tags = str_to_title(category_tags),
         category_tags = if_else(category_tags == "Iphone", "iPhone", category_tags)) %>%
  count(category_tags, sort = T) %>%
  rename(category_tags_times_used = n) %>%
  mutate(category_tags_times_used_perc = 100 * (category_tags_times_used/313),
         label_info = paste0("appeared ",round(category_tags_times_used_perc,1),"% of the times"))



# ------ DATA VIZ ------  

# ------ typography ------  
font_add_google("Merriweather", "title_font")
font_add_google("Mulish", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"


# ------ text ------  
title_text_1 <- "Changes in Product Hunt: 3 weeks in 2017"
subtitle_text_1 <- "Product Hunt is the internet's largest social network and clearinghouse for apps. It's mainly used to promote tech products.<br>Like every new idea, at first, the inflow of products and releases is low, but eventually, it increases. So when did it happen to Product Hunt? During August and September 2017 (between the 33rd and 35th week of the year), the site registered a significant increase in product releases."
subtitle_text_2 <- "What do the tags tell us about the releases during those three weeks? Mostly **Tech**, **Productivity**, **Web Apps** and **iPhone**."
caption_text <- "Designed by Isaac Arroyo (@unisaacarroyov on twitter)<br>#TidyTuesday Week 40: Product Hunt<br>Dataset provided by components.one"

# ------ theme ------ 
theme_set(theme_minimal())
theme_update(
  legend.position = "none",
  # Title
  plot.title.position = "plot",
  plot.title = element_textbox(family = title_font,
                               face = "bold",
                               lineheight = 0.2,
                               size = rel(8),
                               width = unit(625, "pt"),
                               hjust = 0.5),
  # Subtitle
  plot.subtitle = element_textbox(family = body_font,
                                  lineheight = 0.3,
                                  size = rel(4),
                                  width = unit(625, "pt"),
                                  hjust = 0.5,
                                  halign = 0),
  # Caption
  plot.caption.position = "plot",
  plot.caption = element_textbox(family = body_font,
                                 lineheight = 0.4,
                                 size = rel(3),
                                 width = unit(625,"pt"),
                                 hjust = 0.5,
                                 halign = 1),
  # Axis
  axis.title = element_blank(),
  # Grid
  panel.grid = element_blank(),
)

# ------ line chart ------  
p1 <- df_dataviz_1 %>%
  mutate(year_date = factor(year_date)) %>%
  ggplot(aes(x = week_of_year, y = number_of_releases, colour = year_date)) +
  geom_textsegment(label = "50 releases", x = 0, xend = 52, y = 50, yend = 50, family = body_font, size = 4.5, spacing = -100, hjust = 0, vjust = -0.1, linewidth = 0.2, colour = 'gray60', linecolour = 'gray50', linetype = 'dashed') +
  geom_textsegment(label = "150 releases", x = 0, xend = 52, y =150, yend = 150, family = body_font, size = 4.5, spacing = -100, hjust = 0, vjust = -0.1, linewidth = 0.2, colour = 'gray60', linecolour = 'gray50', linetype = 'dashed') +
  geom_textsegment(label = "250 releases", x = 0, xend = 52, y = 250, yend = 250, family = body_font, size = 4.5, spacing = -100, hjust = 0, vjust = -0.1, linewidth = 0.2, colour = 'gray60', linecolour = 'gray50', linetype = 'dashed') +
  geom_line() +
  gghighlight::gghighlight(number_of_releases > 0,
                           use_direct_label = F,
                           unhighlighted_params = list(colour = 'gray80', size = 0.5, alpha = 0.3)) +
  scale_x_continuous(breaks = seq(10,40,10), labels = paste("week\n",seq(10,40,10))) +
  scale_colour_manual(values = met.brewer(name = "Tam", n = 8)) +
  facet_wrap(~year_date, ncol = 2, scales = "free") +
  labs(title = title_text_1, subtitle = subtitle_text_1) +
  theme(
    # Axis*
    axis.text.y = element_blank(),
    axis.line.x = element_line(colour = 'gray60', size = 0.4),
    axis.ticks.x = element_line(colour = 'gray60',
                                size = 0.6,
                                arrow = arrow(length = unit(1,"pt"),
                                              ends = "last",
                                              angle = 30,
                                              type = "closed")),
    axis.text.x = element_text(family = body_font,
                             lineheight = 0.3,
                             size = rel(2.5)),
    # Facets*
    strip.text = element_text(family = title_font,
                              face = "bold",
                              lineheight = 0,
                              size = rel(3)),
    panel.spacing.y = unit(-10,"pt"),
    panel.spacing.x = unit(20,"pt")
  )

# ------ bar chart ------
p2 <- df_dataviz_2 %>%
  head(10) %>%
  mutate(category_tags = fct_reorder(category_tags, category_tags_times_used_perc)) %>%
  ggplot(aes(y = category_tags)) +
  geom_segment(aes(x=0, xend = category_tags_times_used_perc, yend = category_tags),
               size = 0.75) +
  geom_point(aes(x = category_tags_times_used_perc),
             size = 8, colour = "#de4f33") +
  geom_textbox(aes(label = label_info, x = 0),
               hjust = 0,
               size = 10,
               family = body_font,
               fontface = 'italic',
               minwidth = unit(10,"pt"),
               colour = "#de4f33",
               box.colour = 'transparent',
               box.padding = unit(3,"pt"),
               nudge_y = -0.18) +
  labs(title = NULL, subtitle = subtitle_text_2, caption = caption_text) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = title_font, 
                               lineheight = 0.3,
                               size = rel(4),
                               face = 'bold',
                               colour = 'black',
                               margin = margin(r = -20))
  )


# ------ Combine charts ------
final <- plot_grid(p1, p2,ncol = 1)
ggsave(filename = "./gallery_2022/2022_week-40_product-hunt.png",
       width = 1080, height = 1920, units = "px",
       plot = final,
       bg = "#FFFDF9",
       dpi = 300, scale = 2.5)
