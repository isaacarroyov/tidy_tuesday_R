# #TidyTuesday Week 32: Ferris Wheels 

library(dplyr)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)
# install.packages("FactoMineR")
# install.packages("factoextra")
library(FactoMineR)
library(factoextra)

# Cargamos datos 
# remotes::install_github("emilhvitfeldt/ferriswheels")
library(ferriswheels)

datos <- wheels %>%
  filter(diameter > 0, hourly_capacity > 0, ride_duration_minutes > 0) %>%
  filter(status %in% c("Operating", "Under Construction")) %>%
  mutate(radius = diameter/2) %>%
  select(name, country, status, height, radius,
         hourly_capacity, ride_duration_minutes)




