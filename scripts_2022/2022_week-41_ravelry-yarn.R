library(dplyr)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)


# ------ LOAD DATA ------
yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

yarn
