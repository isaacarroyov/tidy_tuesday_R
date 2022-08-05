# TidyTuesday Week 31: Oregon Spotted Frog
library(dplyr)
library(ggplot2)
library(MetBrewer)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

font_add_google("Montserrat", "bodyFont")
showtext_auto()

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
  arrange(forcats::fct_infreq(water_type)) %>%
  mutate(xpos = c(rep(1:20,15), 1:11),
         ypos = v_ypos)

# Data Visualization
p1 <- df %>%
  ggplot(aes(x=xpos,y=ypos, color=water_type)) +
  geom_point(size = 6) +
  scale_color_manual(values = c("Reservoir" = "#AF2C02",
                                "Marsh/Pond" = "#DAA520",
                                "Stream/Canal" = "#4E937A",
                                "Non-aquatic" = "#EBEBEB")) +
  theme_void() +
  coord_fixed(ratio = 1, clip = "off") +
  labs(title = "",
       subtitle = "Hello hello, everybody, Gossip Girl here. Did you miss me? I know I've missed you. You may have thought that my knowledge only covers information about the scandalous life of Upper East Siders. Nonetheless, you're partially mistaken. In order to deliver the best news is vital to know how to about data, so I decided to visit other places to take a peek at their data. On this occasion, I came up with frogs.<br><br>According to my dear friend, the internet, Mark Twain said, _If it's your job to eat a frog, it's best to do it first thing in the morning. And If it's your job to eat two frogs, it's best to eat the biggest one first_. I don't know about you, but the only time I would put a frog close to my lips, it would be better to become a prince or the heir of a successful company.<br><br>However, the U.S. Geological Survey, a.k.a. USGS, has taken the job of not eating but locating the fantabulous Oregon Spotted Frog from September to November 2018.<br><br>Who would have thought that 175 were on Reservoirs, 93 on Marshes or Ponds, 38 on Streams or Canals and 5 on Non-aquatic places? I don't.<br><br>Remember to keep your eyes open; you don't know what nature will throw at you...XOXO, Gossip Girl.",
       caption = "Visualization by Isaac Arroyo.<br>#TidyTuesday Week 31: Oregon Spotted Frogs.<br>Data source: U.S. Geological Survey") +  theme(
    # Background
    plot.background = element_rect(fill = "#15161E", colour = "#15161E"),
    panel.background = element_blank(),
    # Colour Text
    text = element_text(colour = "#FCF199", family = "bodyFont"),
    # Legend 
    legend.position = "none",
    # Title and subtitle
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(size = rel(2.8), width = unit(5,"in"), halign = 0.5, lineheight = 0.4, margin = margin(1.5,0,0.5,0, unit = "in")),
    # Caption
    plot.caption = element_textbox_simple(size = rel(2.5), width = unit(5,"in"), halign = 0.5, lineheight = 0.4, margin = margin(0.5,0,0.5,0, unit = "in")),
  )


ggsave(filename = "./gallery_2022/2022_week-31_frogs.png",
       plot = p1,
       width = 8.5, height = 11, units = "in",
       dpi = 300, scale = 1
       )
