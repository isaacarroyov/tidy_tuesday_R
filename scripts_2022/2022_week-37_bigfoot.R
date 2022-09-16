# ------ #TidyTuesday Week 37: Bigfoot ------
library(dplyr)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

# LOAD DATA
data_bigfoot <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv")

