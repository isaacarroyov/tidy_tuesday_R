# Week 29 
library(dplyr)
library(ggplot2)
library(geofacet)
library(countrycode)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

# font_add_google("", "titleFont")
# font_add_google("", "bodyFont")
# showtext_auto()

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
  



# DATA VISUALIZATION
# Create grid
mygrid <- data.frame(
  row = c(1, 1, 1, 2, 2, 2, 3, 3, 4, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9),
  col = c(1, 5, 6, 2, 5, 6, 2, 3, 3, 3, 4, 5, 6, 7, 4, 5, 6, 4, 5, 6, 5, 6),
  code = c("MEX", "CUB", "DOM", "GTM", "HTI", "PRI", "SLV", "HND", "NIC", "CRI", "PAN", "COL", "VEN", "GUY", "ECU", "PER", "BRA", "CHL", "BOL", "PRY", "ARG", "URY"),
  name = c("Mexico", "Cuba", "Dominican Republic", "Guatemala", "Haiti", "Puerto Rico", "El Salvador", "Honduras", "Nicaragua", "Costa Rica", "Panama", "Colombia", "Venezuela", "Guyana", "Ecuador", "Peru", "Brazil", "Chile", "Bolivia", "Paraguay", "Argentina", "Uruguay"),
  stringsAsFactors = FALSE
)
geofacet::grid_preview(mygrid)


p1 <- df %>%
  ggplot(aes(x=date_year, y=value, fill = variable_name)) +
  geom_area() +
  facet_geo(~country_name, grid = mygrid) +
  labs(title = "Title",
       subtitle = "Subtitle and description",
       caption = '#TidyTuesday Week 29: Technology Adoption<br>Visualization by Isaac Arroyo (@unisaacarroyov)<br>Data: Charles Kenny and George Yang. 2022. _Technology and Development: An Exploration of the Data._ CGD Working Paper 617. Washington, DC: Center for Global Development.') +
  theme_minimal() + 
  theme(
    # Legend
    legend.position = "none",
    # Axis
    axis.title = element_blank(),
    # Grid
    # Facets
    # Title
    plot.title = element_textbox_simple(size = 30),
    # Subtitle
    plot.subtitle = element_textbox_simple(size = 15),
    # Caption
    plot.caption = element_textbox_simple(size = 10),
  )


ggsave("./gallery_2022/2022_week-29_technology-adoption.png",
       plot = p1, width = 8.5, height = 11, units = 'in',
       dpi = 300)

