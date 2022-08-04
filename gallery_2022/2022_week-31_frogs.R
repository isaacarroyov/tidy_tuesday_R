# TidyTuesday Week 31: Oregon Spotted Frog
library(dplyr)
library(ggplot2)
library(MetBrewer)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

# LOAD DATA
datos <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')
# Columns
names(datos)

# Select columns for the data visualization
v_ypos <- c()
for(x in 16:1){
  if (x==1) {
    v_ypos <- append(v_ypos, rep(x, 11))
  } else {
    v_ypos <- append(v_ypos, rep(x, 20))
  }
}

df <- datos %>%
  rename(water_type = Type) %>%
  select(water_type) %>%
  arrange(water_type) %>%
  mutate(xpos = c(rep(1:20,15), 1:11),
         ypos = v_ypos)
