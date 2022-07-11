library(dplyr)
library(scales)
library(ggplot2)
library(ggtext)
library(MetBrewer)
library(sysfonts)
library(showtext)

font_add_google("Bitter", "Bitter")
font_add_google("Poppins", "Poppins")
showtext_auto()

# LOAD DATA
data_rents <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv")

# Visualize the price of Beds + Baths combinations in the most published 
# nhood from Craigslist


# Step 1 - Extract data that where beds, bats are complete
# Step 2 - Label combination of beds and baths
# Step 3 - Make them factors
df_combinations_bed_baths <- data_rents %>%
  select(nhood, year, price, beds, baths) %>%
  filter(beds > 0, baths > 0) %>%
  mutate(combination_beds_baths = case_when(
    beds == 1 & baths > 0 ~ "1 bed and 1 or more baths",
    beds == 2 & baths < 2 ~ "2 beds and less than 2 baths",
    beds == 2 & baths >= 2 ~ "2 beds and 2 or more baths",
    beds == 3 & baths < 2 ~ "3 beds and less than 2 baths",
    beds == 3 & baths >= 2 ~ "3 beds and 2 or more baths",
    beds >= 4 & baths > 0 ~ "4 beds or more and more than 1 bath"
  )) %>%
  mutate(combination_beds_baths = factor(combination_beds_baths,
                                         levels = c("1 bed and 1 or more baths", "2 beds and less than 2 baths",
                                                    "2 beds and 2 or more baths", "3 beds and less than 2 baths",
                                                    "3 beds and 2 or more baths", "4 beds or more and more than 1 bath"))) %>% 
  as_tibble()

# Step 4 - Extract the top 5 most published 
# neightbourhoods according to Craigslist
popular_nhood <- df_combinations_bed_baths %>%
  count(nhood) %>%
  arrange(-n) %>%
  head(6) %>%
  pull(nhood)

# Step 5 - Filter by the top 5 most published 
# neightbourhoods according to Craigslist
df_combinations_bed_baths_nhoods <- df_combinations_bed_baths %>%
  filter(nhood %in% popular_nhood)

# Step 6 - Find the most and least expensive nhood
df_combinations_bed_baths_nhoods %>%
  group_by(nhood) %>%
  summarise(median_rental_price = median(price)) %>%
  arrange(-median_rental_price)


# Step 7 - Create the data visualization
df_combinations_bed_baths_nhoods %>%
  ggplot(aes(x=price, y=combination_beds_baths,color=combination_beds_baths)) +
  geom_vline(aes(xintercept=median(df_combinations_bed_baths$price)), color='black', size = 0.5) +
  geom_boxplot(color = "grey40", fill = "transparent", outlier.alpha = 0) +
  geom_jitter(height = 0.1, alpha=0.2) + 
  coord_cartesian(clip = "off") +
  scale_colour_manual(values = met.brewer("Java",6)) +
  guides(color = "none") +
  scale_x_continuous(trans = "log10", breaks = breaks_log(n = 6, base = 10) , labels = label_dollar(scale_cut = cut_short_scale())) + 
  scale_y_discrete(labels = function(x) stringr::str_wrap(x,15)) +
  facet_wrap(~nhood, nrow = 2) + 
  labs(title = "Most published neighbourhoods on Craigslist: Their rental prices and the number of beds + baths.",
       subtitle = "The data visualization showcases the distribution of rental prices according to the number of beds + baths in each neighbourhood.<br><br>The dots represent an observation, and every combination of the number of beds and number of baths is colour-encoded. The <span style='color:black'><b>black line</b></span> is the total median rental price of the entire of the dataset –where the number of beds and baths were specified– which is $2,000.<br><br>In order to get a better view of the rental prices, the x-axis had to be scaled to logarithm base 10. <br><br>Observations according to the data:<br>• The median rental price of <span style='color:#0B7156'><b>4-bed and 1-bath-or-more properties</b></span> appears to be higher than the total median rental price in all neighbourhoods.<br>• Almost all IQRs of <span style='color:#663171'><b>1-bed and 1-bath properties</b></span> lay beneath the total median rental price (left side of the black line).<br>• The neighbourhood with the highest median rental price is <span style='font-family:Bitter'><b>san jose south<</b>/span> ($1,968); and <span style='font-family:Bitter'><b>belmont / san carlos</b></span> has the lowest median rental price ($1,400).",
       caption = "<b>#TidyTuesday Week 27: San Francisco Rents<br>Data: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018. Retrieved from <em>github.com/katepennington/historic_bay_area_craigslist_housing_posts/blob/master/clean_2000_2018.csv.zip</em><br>Visualization by Isaac Arroyo (@unisaacarroyov)</b>") +
  theme_minimal() +
  theme(
    # Background
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    plot.background = element_rect(fill = "white", colour = "transparent"),
    # Title
    plot.title.position = "plot",
    plot.title = element_textbox_simple(family = "Bitter", face = "bold", size = 80, lineheight = 0.3, margin = margin(10,15,10,15)),
    # Subtitle
    plot.subtitle = element_textbox_simple(family = "Poppins", size = 35, lineheight = 0.4, margin = margin(0,15,20,15)),
    # Caption
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(family = "Poppins", color = "black", size = 20, lineheight = 0.45,halign = 0.5, margin = margin(15,0,5,0)),
    # Legend
    # legend.position = c(0.2,1),
    # legend.text = element_textbox_simple(maxwidth = unit(6,"lines")),
    # legend.key.height = unit(1,"lines"),
    # legend.key.width = unit(8,"lines"),
    # Grid
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(colour = "grey80", size = 0.2, linetype = "dashed", lineend = "round"),
    panel.grid.minor.x = element_line(colour = "grey80", size = 0.2, linetype = "dashed", lineend = "round"),
    # Facet wrap
    panel.spacing = unit(1.5,"line"),
    strip.text = element_text(colour = "black", face = "bold", family = "Bitter", size = 40, lineheight = 0.1),
    # Axis
    axis.title = element_blank(),
    axis.text.y = element_text(colour = "black", family = "Poppins", face = "bold", size = 30, lineheight = 0.25),
    axis.text.x = element_text(colour = "grey30", family = "Poppins", face = "italic", size = 30, lineheight = 0.1),
  )

# Save the data visualization
ggsave("./gallery_2022/2022_week-27_san-francisco-rents.png",
       width = 8.5, height = 11, units = "in", dpi = 300)