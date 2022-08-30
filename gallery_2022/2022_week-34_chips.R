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
title_text <- "Artsy title"
subtitle_text <- "Little description"
caption_text <- "Designed by Isaac Arroyo (@unisaacarroyov on twitter)<br>#TidyTuesday Week 34: CHIP Dataset<br>Paper: Sun, Y., Agostini, N. B., Dong, S., & Kaeli, D. (2019). Summarizing CPU and GPU design trends with product data. _arXiv preprint arXiv:1911.11313_.<br>Data from The CHIP Dataset: _https://chip-dataset.vercel.app_"



p1 <- df %>%
  ggplot(aes(x =  release_date, y = tdp_w,
             colour = vendor, size = die_size_mm2)) +
  geom_point() +
  scale_colour_manual(values = colour_palette) +
  scale_y_log10() +
  coord_polar(clip = "off") +
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text) +
  facet_wrap(~cpu_or_gpu, nrow = 2) +
  theme_void() +
  theme(
    legend.position = "none",
  )

# ------ SAVE PLOT ------
# Both plots

# CPU plot

# GPU plot


