# #TidyTuesday Week 32: Ferris Wheels 
library(dplyr)
library(ggplot2)
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
  geom_textbox(data = tribble(~x,~y,~label,
                              25,300, '<b>Note about the title</b><br>Well, whenever I am at a spot with a great view of the city or landscape, I remember a scene from "Blood In Blood Out." When the characters are looking at the whole city from far away, one of them says, "Check out the view, homies." However, in the Spanish version, they translated that line into "Wachen el paisaje, homies." Nowadays, that scene and line in Spanish have been turned into a meme.'
                              ),
               aes(x=x,y=y,label=label),
               size = 10, color = 'black'
               ) +
  geom_point(aes(size = radius)) +
  scale_size_area(max_size = 10) +
  labs(title = "Wachen el paisaje, homies (Check out the new view, homies)",
       subtitle = "Ferris wheels are a well-known attraction in many cities around the globe due to the panoramic view the people can enjoy. The visualization shows some of the most famous Ferris wheels currently operating. <br>The circles' radiuses are size encoded (but not at scale); the maximum height the viewer can be is the distance from the beginning of the line (top) to the center of the circle. Finally, the countries are colour encoded. However, all the information is labelled.",
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

ggsave(filename = "./gallery_2022/2022_week-32_ferris-wheels.png",
       plot = p1,
       height = 8.5, width = 11, units = "in",
       dpi = 300)

