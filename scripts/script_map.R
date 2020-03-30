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

#load most recent data frame
data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
  mutate_all(as.character) %>% 
  mutate(measure_numeric = as.numeric(measure_numeric)) %>% 
  remove_empty("rows")


#count up the number of samples per site (e.g., "n = 22" means there are 22 fish samples at those lat/long coords)
sites <- data_fish %>% 
  group_by(lat,lon) %>%
  distinct(fish_id) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(!is.na(lat)) %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         n = as.numeric(n))


#get layer for the world map
world <- map_data("world")


#make a map
(map_fish <- ggplot() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey60") +  #fill is the land color
  coord_quickmap() +
  geom_point(data = sites, aes(x = lon, y = lat, size = n),                             #fill is the data colors. n is the size of the points  
             shape = 21, fill = "yellow") + 
  theme_void() +
  theme(panel.background = element_rect(fill = "black")))                               #adjust the ocean color


#save the map
ggsave(map_fish, file = "map_fish.jpg", dpi = 600)



