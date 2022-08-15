# #TidyTuesday Week 32: Ferris Wheels 
library(dplyr)
library(ggplot2)
library(MetBrewer)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

# Cargamos datos 
# remotes::install_github("emilhvitfeldt/ferriswheels")
library(ferriswheels)

datos <- wheels %>%
  filter(diameter > 0, hourly_capacity > 0, ride_duration_minutes > 0) %>%
  filter(status == "Operating") %>%
  mutate(radius = diameter/2) %>%
  mutate(max_height_view = height + radius) %>%
  select(name, country, height, radius, max_height_view)

# Data Visualization -> bubble plot + bar charts
p1 <- datos %>%
  ggplot(aes(x=radius, y = max(max_height_view) - max_height_view, colour = country)) +
  geom_curve(aes(x=radius, xend = radius, y = max(max_height_view), yend = max(max_height_view) - max_height_view),
             curvature = 0.1, size = 0.3) +
  geom_point(aes(size = radius)) +
  scale_size_area(max_size = 10) +
  labs(title = "Wachen el paisaje, hommies (Check out the new view, hommies)",
       subtitle = "Description",
       caption = "Designed by Isaac Arroyo (@unisaacarroyov on twitter)<br>#TidyTuesday Week 32: Ferris Wheels<br>Data Source: Emil Hvitfeldt (@Emil_Hvitfeldt on twitter) via <span style='font-family:mono'>ferriswheels</span>") +
  theme_void() +
  theme(
    legend.position = "none",
    # Bacground
    # Title
    plot.title.position = 'plot',
    plot.title = element_textbox_simple(face = 'bold'),
    # Subtitle
    plot.subtitle = element_textbox_simple(),
    # CAption
    plot.caption.position = 'plot',
    plot.caption = element_textbox_simple(),
  )

p1

