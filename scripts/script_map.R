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

