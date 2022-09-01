# TidyTuesday Week 34: CHIP dataset
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)
library(NatParksPalettes)


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



p1 <- df %>%
  ggplot(aes(x =  release_date, y = tdp_w,
             colour = vendor, size = die_size_mm2)) +
  geom_point() +
  scale_colour_manual(values = colour_palette) +
  gghighlight::gghighlight(cpu_or_gpu == 'CPU',
                           unhighlighted_params = list(alpha = 0.1, colour = 'gray80')) +
  scale_y_log10() +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y", limits = c(as.Date('2000-01-01'), as.Date('2026-01-01'))) +
  coord_polar(clip = "off", ) +
  labs(title = title_text_01, subtitle = subtitle_text_01, caption = caption_text) +
  guides(size = "none") +
  theme_void() +
  theme(
    # Background
    plot.background = element_rect(fill = '#ffffff', colour = '#ffffff'),
    panel.background = element_blank(),
    # Title
    plot.title.position = 'plot',
    plot.title = element_textbox(halign = 0.5, hjust = 0.5,
                                 face = 'bold',
                                 width = unit(250,'pt')
                                 ),
    # Subtitle
    plot.subtitle = element_textbox(halign = 0.5, hjust = 0.5,
                                    width = unit(250,'pt'),
                                    ),
    # Caption
    plot.caption.position = 'plot',
    plot.caption = element_textbox(halign = 0.5, hjust = 0.5,
                                   face = 'bold',
                                   width = unit(250,'pt'),
                                   ),
    # Legend
    legend.position = "none",
  )
# ------ SAVE PLOT ------


# CPU plot
ggsave(filename = "./gallery_2022/2022_week-34_chips.png",
       plot = p1,
       width = 1080, height = 1350, units = "px",
       dpi = 300)
# GPU plot
# ggsave(filename = "./gallery_2022/2022_week-34_chips.png",
#        plot = p2,
#        width = 6, height = 6, units = "in",
#        dpi = 300)
# Both plots


