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

# Stack all new_data files ----------------------------------------------

# make list of filenames to import ***double check that folder is assigned correctly ***
filenames_combine_csv <- list.files("database/data_to_add/", pattern = "*.csv")

# empty object to put data into - one for ,csv + one for .xlsx
data_combine_csv.list <- NULL

# add files to empty objects
for(name in seq_along(filenames_combine_csv)){
  data_combine_csv.list[[name]] <- read.csv(paste0("database/data_to_add/",filenames_combine_csv[name]),na.strings=c("","NA"))
}

fish_mutate <- function(dt) {
  dt <- dt  %>% 
    mutate_each(funs("as.character"))}

data_to_add <- bind_rows(unclass(lapply(data_combine_csv.list, FUN = fish_mutate))) 

#get lat/lon (requires API from google and internet connection - if it doesn't work, skip this step)
lat_lon <- data_to_add %>% 
  # filter(!is.na(site_name)) %>% 
  distinct(site_name) %>% 
  mutate_geocode(site_name) %>% 
  mutate_all(funs('as.character'))

#add lat/lon and taxa info to data
new_data_latlon <- data_to_add %>% 
  full_join(lat_lon, by = "site_name") %>% 
  left_join(prey_taxa_all) %>% 
  left_join(fish_taxon_add) %>% 
  mutate(dateadded = as.character(Sys.Date()))


# Append new data to existing data ----------------------------------------

#load master data frame and save a backup
data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
  mutate_all(funs('as.character')) 
write.csv(data_fish,file = paste0("database/data_backups/data_fish", Sys.Date(),".csv"),row.names = F)

#stack new data to old data
data_fish_updated <- bind_rows(data_fish, new_data_latlon)

#save updated data as "data_fish.rds"
saveRDS(data_fish_updated, file = "database/data_fish.rds")



# Clear files -------------------------------------------------------------
# Move csv's that have been added to a folder called "data_already_added"
move_files_from <- list.files(path = "./database/data_to_add",pattern = "*.csv")
move_files_to <- paste0("./database/data_already_added/", move_files_from)
file.rename(from = paste0("./database/data_to_add/",move_files_from), to = move_files_to)
