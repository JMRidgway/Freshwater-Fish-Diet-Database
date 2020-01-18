library(tidyverse)
library(ggmap)
library(lubridate)
library(maps)
library(usmap)


# Load data ---------------------------------------------------------------

lat_long_list <- data_fish %>% 
  filter(is.na(lat),!is.na(site_name)) %>% 
  distinct(site_name) %>% 
  mutate_geocode(site_name)

test <- left_join(data_fish %>% select(-lat, -lon), site_name)

world <- map_data("world")

plot_coords <- test %>% distinct(site_name, .keep_all = T) %>% filter(!is.na(lat))

ggplot() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) + 
  coord_quickmap()+
  geom_point(data = plot_coords, aes(x = lon, y = lat),size = 3,
             shape = 21,
             fill = "yellow")
