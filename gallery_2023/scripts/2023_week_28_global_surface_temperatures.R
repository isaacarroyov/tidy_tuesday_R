library(tidyverse)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)
library(patchwork)

# = = Datos TidyTuesday = = #
df_tt_global <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv") %>%
  select(!c("J-D","D-N", "DJF", "MAM", "JJA", "SON")) %>%
  pivot_longer(cols = 2:13, names_to = "month", values_to = "anomaly_temp") %>%
  mutate(date_year_month = ymd(paste0(Year,month,15)),
         lugar = "Global") %>%
  select(lugar, date_year_month, anomaly_temp)

df_tt_nh <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/nh_temps.csv") %>%
  select(!c("J-D","D-N", "DJF", "MAM", "JJA", "SON")) %>%
  pivot_longer(cols = 2:13, names_to = "month", values_to = "anomaly_temp") %>%
  mutate(date_year_month = ymd(paste0(Year,month,15)),
         lugar = "Hemisferio Norte") %>%
  select(lugar, date_year_month, anomaly_temp)


df_tt <- bind_rows(df_tt_global, df_tt_nh)



# = = Datos N+ Focus = = #
df_nmasfocus <- read_csv("https://raw.githubusercontent.com/nmasfocusdatos/desplazamiento-climatico/main/datos/ee_terraclimate_db/ts_nac-ent_month_terraclimate.csv") %>% 
  select(date_year_month, nombre_estado, anomaly_tmmx_mean)

# = = DataVis = = #

df_tt %>% 
  #filter(year(date_year_month) >= 1960) %>%
  ggplot(aes(x = date_year_month, fill = anomaly_temp)) +
  geom_tile(aes(y = 1), color = "transparent") +
  scale_fill_gradient2(low = "#4a74b4", mid = "#f1efe3", midpoint = 0, high = "#ff3640") +
  facet_wrap(vars(lugar)) +
  theme_void()

df_nmasfocus %>%
  filter(nombre_estado != "Nacional") %>%
  ggplot(aes(x = date_year_month, fill = anomaly_tmmx_mean)) +
  geom_tile(aes(y = 1), color = "transparent") +
  scale_fill_gradient2(low = "#4a74b4", mid = "#f1efe3", midpoint = 0, high = "#ff3640") +
  facet_wrap(vars(nombre_estado), ncol = 4) +
  theme_void()


