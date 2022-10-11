# #TidyTuesday Week 40: Product Hunt
library(tidyverse)
library(lubridate)
library(ggtext)
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
  rename(category_tags_times_used = n)



# ------ DATA VIZ ------  

# ------ typography ------  

# ------ text ------  
title_text_1 <- "Title 1"
subtitle_text_1 <- "Subtitle 1"
title_text_2 <- "Title 2"
subtitle_text_2 <- "Subtitle 2"
caption_text <- "Designed by Isaac Arroyo (@unisaacarroyov on twitter)<br>#TidyTuesday Week 40: Product Hunt<br>Dataset provided by components.one"

# ------ theme ------  

# ------ line chart ------  
df_dataviz_1 %>%
  mutate(year_date = factor(year_date)) %>%
  ggplot(aes(x = week_of_year, y = number_of_releases, colour = year_date)) +
  geom_line() +
  gghighlight::gghighlight(number_of_releases > 0, use_direct_label = F) +
  facet_wrap(~year_date, ncol = 2) +
  labs(title = title_text_1, subtitle = subtitle_text_1, caption = caption_text) +
  theme(
    legend.position = "none",
    # Plot
    # Title
    plot.title.position = "plot",
    plot.title = element_text(),
    # Subtitle
    plot.subtitle = element_textbox(),
    # Caption
    plot.caption.position = "plot",
    plot.caption = element_textbox(),
    # Axis
    axis.title = element_blank(),
    # Facets
  )

# ------ bar chart ------
