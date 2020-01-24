library(tidyverse)
library(ggridges)
library(lubridate)
library(ggmap)
library(httr)
library(cowplot)
library(janitor)
library(repmis)
library(readxl)
library(rlist)
library(RCurl)

#load master data_frame
data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
  mutate_all(funs('as.character')) %>% 
  remove_empty("rows")


missing_latlon <- data_fish %>% select(lat, lon, citation, site_name, author, year) %>% 
  filter(is.na(lat)) %>% distinct(site_name, .keep_all =T) %>% 
  select(-lat, -lon) %>% 
  mutate(site_name_new = case_when(grepl("Ero Res", site_name) ~ "Ekiti, Nigeria",
                                   grepl("Cable", author) ~ paste(site_name,", SD"),
                                   grepl("Beaver", site_name) ~ "Beaver Reservoir, White River, AR",
                                   grepl("BULL SHOALS", site_name) ~ "Bull Shoals Reservoir, White River, AR",
                                   grepl("Eagle Lake", site_name) ~ "Warren County, MS",
                                   TRUE ~ site_name)) %>% 
  drop_na(site_name_new) %>% 
  mutate_geocode(site_name_new) %>% 
  mutate_all(funs('as.character'))


add_latlon <- missing_latlon %>% select(lat, lon, site_name, site_name_new) %>% 
  rename(new_lat = lat,
         new_lon = lon)

data_fish <- data_fish %>% full_join(add_latlon) %>% 
  mutate(lat = case_when(is.na(lat) ~ new_lat,
                         TRUE ~ lat),
         lon = case_when(is.na(lon) ~ new_lon,
                         TRUE ~ lon)) 

saveRDS(data_fish, file = "database/data_fish.rds")
