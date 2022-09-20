# ------ #TidyTuesday Week 37: Bigfoot ------

# ------ DATA VISUALIZATION IDEA ------
# Observando los plots que ha subido la comunidad, en gran parte son mapas de los
# avistamientos de Big foot en el pais. Algunos de ellos se han comparado con peliculas
# relacionadas a Big foot o con areas donde habitan osos.
#
# Para este caso, quiero explorar las diferencias entre los avistamientos de 
# Class A and B, ya que es algo completamente nuevo para mi.
#
# Se pueden realizar diferentes comparaciones pero intentaremos visualizar palabras 
# que se usan para describir.

# ------LOAD LIBRARIES ------
library(dplyr)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

# ------ LOAD DATA ------
data_bigfoot <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv")



# ------ DATA WRANGLING ------
df <- data_bigfoot %>%
  filter(classification %in% c("Class A","Class B"),
         !is.na(date)) %>%
  mutate(date_year = lubridate::year(date)) %>%
  filter(date_year >= 1950) %>%
  mutate(date_decade = case_when(date_year %in% seq(1950,1959) ~ "1950s",
                                 date_year %in% seq(1960,1969) ~ "1960s",
                                 date_year %in% seq(1970,1979) ~ "1970s",
                                 date_year %in% seq(1980,1989) ~ "1980s",
                                 date_year %in% seq(1990,1999) ~ "1990s",
                                 date_year %in% seq(2000,2009) ~ "2000s",
                                 date_year %in% seq(2010,2019) ~ "2010s",
                                 date_year %in% seq(2020,2029) ~ "2020s",
                                 T ~ NA_character_)) %>%
  select(classification, observed, date_decade)
  
  
# ------ DATAVIZ SETTINGS
# ------ Texts ------

# ------ Colour Palette ------

# ------ Typography ------

# ------ DATA VISUALIZATION ------

# ------ SAVE GGPLOT ------




