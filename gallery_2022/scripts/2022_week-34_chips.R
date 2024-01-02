# TidyTuesday Week 34: CHIP dataset
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)
library(NatParksPalettes)
library(patchwork)

# ------ LOAD DATA, SELECT VARIABLES AND FILTER NA VALUES ------
datos <- readr::read_csv("./data/2022/ignore_chip_dataset.csv")
datos_relevantes <- datos %>%
  rename(cpu_or_gpu = Type,
         release_date = `Release Date`,
         tdp_w = `TDP (W)`,
         vendor = Vendor,
         die_size_mm2 = `Die Size (mm^2)`) %>%
  select(cpu_or_gpu, release_date, tdp_w, vendor, die_size_mm2) %>%
  tidyr::drop_na()

# ------ DATA WRANGLING ------
df <- datos_relevantes %>%
  mutate(release_date = ymd(release_date),
         vendor = factor(vendor)
         )


# ------ DATA VISUALIZATION ------
colour_palette <- natparks.pals("DeathValley", n = 5)
title_text_01 <- "Non-conventional view of the evolution of CPUs"
subtitle_text_01 <- "Every year, different companies aspire to create better CPUs. The visualization showcases the evolution with an unusual approach by circularly wrapping the x-axis. The time starts at the top (2001) and ends at the top-left side of the visualization (2021). Further points from the center mean better performance, higher temperatures, and more power consumption. The colours represent the vendors: <span style='color:#8D2A0E'><b>AMD</b></span>, <span style='color:#C5682F'><b>ATI</b></span>, <span style='color:#122F5A'><b>Intel</b></span>, <span style='color:#69424E'><b>NVIDIA</b></span> and <span style='color:#B57E82'><b>Others</b></span>."
title_text_02 <- "Non-conventional view of the evolution of GPUs"
subtitle_text_02 <- "Every year, different companies aspire to create better GPUs. The visualization showcases the evolution with an unusual approach by circularly wrapping the x-axis. The time starts at the top (2001) and ends at the top-left side of the visualization (2021). Further points from the center mean better performance, higher temperatures, and more power consumption. The colours represent the vendors: <span style='color:#8D2A0E'><b>AMD</b></span>, <span style='color:#C5682F'><b>ATI</b></span>, <span style='color:#122F5A'><b>Intel</b></span>, <span style='color:#69424E'><b>NVIDIA</b></span> and <span style='color:#B57E82'><b>Others</b></span>."
caption_text <- "Designed by Isaac Arroyo (@unisaacarroyov on twitter)<br>#TidyTuesday Week 34: CHIP Dataset.<br>Data from The CHIP Dataset: _chip-dataset.vercel.app_"

font_add_google("Permanent Marker","font_title")
font_add_google('Overpass', 'font_body')
showtext_auto()

# Set theme
theme_set(theme_void())
theme_update(
  # General typography
  text = element_text(family = 'body_font'),
  # Background
  plot.background = element_rect(fill = '#FFFFFF', colour = 'transparent'),
  panel.background = element_blank(),
  # Title
  plot.title.position = 'plot',
  plot.title = element_textbox(halign = 0.5, hjust = 0.5,
                               family = 'font_title',
                               size = rel(5),
                               lineheight = 0.2,
                               face = 'bold',
                               width = unit(250,'pt'),
                               padding = margin(0,0,0,0),
                               margin = margin(t = 15, b = 5, l = 0, r = 0)
                               ),
  # Subtitle
  plot.subtitle = element_textbox(halign = 0.5, hjust = 0.5,
                                  size = rel(1.5),
                                  lineheight = 0.4,
                                  width = unit(200,'pt'),
                                  padding = margin(0,0,0,0),
                                  margin = margin(t = 0, b = -35, l = 0, r = 0)
                                  ),
  # Caption
  plot.caption.position = 'plot',
  plot.caption = element_textbox(halign = 0.5, hjust = 0.5,
                                 size = rel(1),
                                 lineheight = 0.4,
                                 face = 'bold',
                                 width = unit(200,'pt'),
                                 padding = margin(0,0,0,0),
                                 margin = margin(t = -15, b = 15, l = 0, r = 0)
                                 ),
  # Legend
  legend.position = "none",
)


p1 <- df %>%
  ggplot(aes(x =  release_date, y = tdp_w,
             colour = vendor, size = die_size_mm2)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = colour_palette) +
  gghighlight::gghighlight(cpu_or_gpu == 'CPU',
                           unhighlighted_params = list(alpha = 0.1, colour = 'gray80')) +
  scale_y_log10() +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y", limits = c(as.Date('2000-01-01'), as.Date('2026-01-01'))) +
  coord_polar(clip = "off", ) +
  labs(title = title_text_01, subtitle = subtitle_text_01, caption = caption_text) +
  guides(size = "none")

p2 <- df %>%
  ggplot(aes(x =  release_date, y = tdp_w,
             colour = vendor, size = die_size_mm2)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = colour_palette) +
  gghighlight::gghighlight(cpu_or_gpu == 'GPU',
                           unhighlighted_params = list(alpha = 0.1, colour = 'gray80')) +
  scale_y_log10() +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y", limits = c(as.Date('2000-01-01'), as.Date('2026-01-01'))) +
  coord_polar(clip = "off", ) +
  labs(title = title_text_02, subtitle = subtitle_text_02, caption = caption_text) +
  guides(size = "none")

# ------ SAVE PLOT ------

# CPU plot
ggsave(filename = "./gallery_2022/2022_week-34_chips_01.png",
       plot = p1,
       width = 1080, height = 1350, units = "px",
       dpi = 300)
# GPU plot
ggsave(filename = "./gallery_2022/2022_week-34_chips_02.png",
       plot = p2,
       width = 1080, height = 1350, units = "px",
       dpi = 300)

# Both plots
final <- p1 + p2
ggsave(filename = "./gallery_2022/2022_week-34_chips.png",
       plot = final,
       width = 1080*2, height = 1350, units = "px",
       dpi = 300)
