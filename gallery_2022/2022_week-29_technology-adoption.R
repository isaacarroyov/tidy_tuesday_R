# Week 29 
library(dplyr)
# library(forcats)
# library(lubridate)
library(ggplot2)
library(countrycode)
# library(ggtext)
# library(sysfonts)
# library(showtext)

# font_add_google("", "titleFont")
# font_add_google("", "bodyFont")
# showtext_auto()

# LOAD DATA
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')
data <- data %>%
  mutate(category_sector = factor(category),
         group_sector = factor(group),
         variable_name = factor(variable),
         variable_label = factor(label)
         ) %>%
  rename(date_year = year) %>%
  select(iso3c, variable_name, variable_label,
         category_sector, group_sector, date_year, value)

# Filter date and variables of interest
# Most populated LATAM countries https://beef2live.com/story-ranking-latin-american-countries-population-0-213654
countries <- c("Brazil", "Mexico", "Colombia", "Argentina", "Peru")
countries_iso3c <- map_data("world") %>%
  tibble() %>%
  mutate(iso3c = countrycode(region,
                             destination = "iso3c",
                             origin = "country.name")) %>%
  select(region, iso3c) %>%
  filter(region %in% countries) %>%
  distinct() %>%
  pull(iso3c)

# Filter by countries and date + add their country names
data_01 <- data %>%
  filter(iso3c %in% countries_iso3c,
         date_year >= 1990) %>%
  mutate(country_name = countrycode(iso3c,
                                    destination = "country.name",
                                    origin = "iso3c")) %>%
  mutate(country_name = factor(country_name))


# Focused on Non-Tech > Agriculture > Land related
data_02 <- data_01 %>%
  filter(group_sector == "Non-Tech",
         category_sector == "Agriculture",
         variable_name %in% c("ag_land","araland","forest","forest_planted") ) %>%
  select(iso3c, variable_name, variable_label, date_year, value)



# Not all agricultural land is actually arable land. Thus, we're
# going to subtract that part. 

# First: change from long to wide format
# Second: calculate agricultural land that is not arable
# Third: rename araland to arable in agricultural land
# Fourth: change from wide to long format
# Last : create (again) variable_legends
data_03 <- data_02 %>%
  select(iso3c, variable_name, date_year, value) %>%
  tidyr::pivot_wider(names_from = "variable_name", values_from = "value") %>%
  mutate(not_arable_in_ag_land = ag_land - araland,
         arable_in_ag_land = araland) %>%
  select(iso3c, date_year, not_arable_in_ag_land, arable_in_ag_land, forest, forest_planted) %>%
  tidyr::pivot_longer(cols = 3:6, names_to = "variable_name", values_to = "value") %>%
  mutate(variable_label = case_when(
    variable_name == "not_arable_in_ag_land" ~ "**Agricultural** land that is **not arable**",
    variable_name == "arable_in_ag_land" ~ "**Arable** land that is not **agricultural**",
    variable_name == "forest" ~ "Land naturally regenerated",
    variable_name == "forest_planted" ~ "Land planted"
  ))

data_03 %>%
  ggplot(aes(x=date_year, y=value, color = variable_name)) + 
  geom_point() +
  facet_wrap(~iso3c)


data_03 %>%
  filter(date_year==2018, iso3c=="MEX") %>%
  pull(value) %>% sum()




