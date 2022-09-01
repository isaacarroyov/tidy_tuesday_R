# Week 35 - Pell Awards
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)
library(MetBrewer)

# LOAD DATA
pell <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv')

pell
