# Week 29 
library(dplyr)
library(ggplot2)
library(geofacet)
library(countrycode)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

font_add_google("Merriweather", "titleFont")
font_add_google("Lato", "bodyFont")
showtext_auto()

# LOAD DATA
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')
data <- data %>%
  rename(category_sector = category,
         group_sector = group,
         variable_name = variable,
         date_year = year) %>%
  select(iso3c, variable_name,
         category_sector, group_sector, date_year, value)


# Filter date and variables of interest
# LATAM countries
countries <- c("Mexico", "Cuba", "Dominican Republic", "Guatemala", "Haiti", "Puerto Rico", "El Salvador", "Honduras", "Nicaragua", "Costa Rica", "Panama", "Colombia", "Venezuela", "Guyana", "Ecuador", "Peru", "Brazil", "Chile", "Bolivia", "Paraguay", "Argentina", "Uruguay")
data_countries_iso3c <- map_data("world") %>%
  tibble() %>%
  mutate(iso3c = countrycode(region,
                             destination = "iso3c",
                             origin = "country.name")) %>%
  select(region, iso3c) %>%
  filter(region %in% countries) %>%
  distinct()
countries_iso3c <- data_countries_iso3c %>%
  pull(iso3c)

# Filter by countries and date + add their country names
data_01 <- data %>%
  filter(iso3c %in% countries_iso3c,
         date_year >= 1990) %>%
  mutate(country_name = countrycode(iso3c,
                                    destination = "country.name",
                                    origin = "iso3c"))


# Focused on Non-Tech > Agriculture > Land forest and planted
data_02 <- data_01 %>%
  filter(variable_name %in% c("pct_ag_ara_land") ) %>%
  select(iso3c, variable_name, date_year, value)

# Mutate variables
df <- data_02 %>%
  left_join(data_countries_iso3c,
            by = "iso3c") %>%
  tidyr::pivot_wider(names_from = "variable_name", values_from = "value") %>%
  mutate(complement_pct_ag_ara_land = 100 - pct_ag_ara_land) %>%
  rename(country_name = region) %>%
  tidyr::pivot_longer(cols = c(4,5), names_to = "variable_name", values_to = "value") %>%
  mutate(variable_label = case_when(variable_name == "pct_ag_ara_land" ~ "Percentage of arable land in agricultural land",
                                    variable_name == "complement_pct_ag_ara_land" ~ "Percentage of agricultural land that is not arable"),
         country_name = factor(country_name),
         variable_name = factor(variable_name),
         )
  
# Find countries that exceeded 50%
df %>%
  filter(variable_name == "pct_ag_ara_land", value >= 50) %>%
  select(country_name, value, date_year) %>%
  count(country_name)

# Compute mean and median of arable land in agriculural land
df %>%
  filter(variable_name == "pct_ag_ara_land") %>%
  summarise(mean_pct_ag_ara_land = mean(value),
          median_pct_ag_ara_land = median(value))


# DATA VISUALIZATION
# Create grid
mygrid <- data.frame(
  row = c(1, 1, 1, 2, 1, 2, 3, 3, 4, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9),
  col = c(1, 5, 7, 2, 6, 5, 2, 3, 3, 3, 4, 5, 6, 7, 4, 5, 6, 4, 5, 6, 5, 6),
  code = c("MEX", "CUB", "DOM", "GTM", "HTI", "PRI", "SLV", "HND", "NIC", "CRI", "PAN", "COL", "VEN", "GUY", "ECU", "PER", "BRA", "CHL", "BOL", "PRY", "ARG", "URY"),
  name = c("Mexico", "Cuba", "Dominican Republic", "Guatemala", "Haiti", "Puerto Rico", "El Salvador", "Honduras", "Nicaragua", "Costa Rica", "Panama", "Colombia", "Venezuela", "Guyana", "Ecuador", "Peru", "Brazil", "Chile", "Bolivia", "Paraguay", "Argentina", "Uruguay"),
  stringsAsFactors = FALSE
)
# geofacet::grid_preview(mygrid)

p1 <- df %>%
  ggplot(aes(x=date_year, y=value, fill = variable_name)) +
  geom_bar(position = "stack", stat = "identity", color = 'transparent', size = 0.05) +
  #geom_area() +
  scale_y_continuous(breaks = seq(25,75,25), labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks = c(1994,2004,2014)) +
  scale_fill_manual(values = MetBrewer::met.brewer(name = "Cassatt1", n = 2, type = "discrete")) +
  facet_geo(~country_name, grid = mygrid, scales = "free") +
  labs(title = "Evolution: Share of arable land in agricultural land",
       subtitle = "We can think of agricultural land as a designated area for agricultural purposes. Perhaps some parts of this area are unsuitable for agricultural purposes. However, arable land refers to whether a parcel of land is suited for agriculture, has adequate soil or can support various agricultural activities.<br><br><span style='font-family:titleFont;font-size:30pt'><b>How to read the visualization</b></span><br>The data visualization showcases the change over time, starting in 1990 and ending in 2018, in the <span style='color:#9292B5'><b>percentage of arable land in agricultural land</b></span> and <span style='color:#E3ABA7'><b>the percentage of agricultural land that is not arable</b></span>. Each mini-area chart corresponds to a country in Latin America (LATAM).<br><br><span style='font-family:titleFont;font-size:30pt'><b>Insights</b></span><br>Most LATAM countries did not surpass 50% of arable land in agricultural land between 1990 and 2018, the mean is approximately 25%, and the median is 21.57%. Nevertheless, the countries that did exceed 50% at some point in time were Cuba and Haiti – for 22 and 25 years, respectively.",
       caption = '#TidyTuesday Week 29: Technology Adoption<br>Visualization by Isaac Arroyo (@unisaacarroyov)<br>Data: Charles Kenny and George Yang. 2022. _Technology and Development: An Exploration of the Data._ CGD Working Paper 617. Washington, DC: Center for Global Development.') +
  theme_minimal() + 
  theme(
    # Background
    plot.background = element_rect(fill = "#292929", color = "transparent"),
    # Legend
    legend.position = "none",
    # Axis
    axis.title = element_blank(),
    axis.ticks = element_line(colour = "gray60", arrow = arrow(length = unit(0.01,"inches"), ends = "last", angle = 20,type = "closed")),
    axis.text = element_text(family = "bodyFont", size = 15, colour = "gray60", face = "italic"),
    # Grid
    panel.grid = element_blank(),
    # Facets
    strip.background = element_rect(fill = "transparent", colour = "transparent"),
    strip.text = element_textbox_simple(family = "titleFont", size = 30, lineheight = 0.2, colour = "white", face = "bold", maxwidth = unit(1,"in"), halign = 0.5, vjust = 0),
    # Title
    #plot.title.position = "plot",
    plot.title = element_textbox_simple(family = "titleFont", size = 70, colour = "white", face = "bold", margin = margin(15,0,0,0)),
    # Subtitle
    plot.subtitle = element_textbox_simple(family = "bodyFont", size = 27, lineheight = 0.4, colour = "white", margin = margin(5,0,15,0)),
    # Caption
    plot.caption = element_textbox_simple(family = "bodyFont",size = 20, lineheight = 0.5, face = "bold", colour = "white", maxwidth = unit(8.4,"in"), margin = margin(0,0,10,0)),
  )
ggsave("./gallery_2022/2022_week-29_technology-adoption.png",
       plot = p1, width = 8.5, height = 11, units = 'in',
       dpi = 300)