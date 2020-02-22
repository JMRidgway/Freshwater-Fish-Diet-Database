library(brms)
library(tidyverse)
library(ggridges)
library(lubridate)
library(httr)
library(cowplot)
library(janitor)
library(repmis)
library(readxl)
library(rlist)
library(ggmap)

data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
  mutate_all(as.character) %>% 
  mutate(measure_numeric = as.numeric(measure_numeric)) %>% 
  remove_empty("rows")

sites <- data_fish %>% distinct(site_name,lat,lon) %>% filter(!is.na(lat)) %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

world <- map_data("world")


ggplot() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) + 
  coord_quickmap()+
  geom_point(data = sites, aes(x = lon, y = lat),size = 3,
             shape = 21,
             fill = "yellow") + 
  theme_void()


