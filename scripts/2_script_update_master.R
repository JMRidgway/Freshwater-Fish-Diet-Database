#Use this script to add data to the master data frame. It will also make a backup of the current master data frame.
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
    mutate_all(as.character)}

data_to_add <- bind_rows(unclass(lapply(data_combine_csv.list, FUN = fish_mutate))) 

#get lat/lon (requires API from google and internet connection - if it doesn't work, skip this step)

missing_site_names <- data_to_add %>% 
  filter(is.na(site_name))

write.csv(missing_site_names, file = paste0("database/data_to_add/re_do/missing_site_names",Sys.Date(), ".csv"))


lat_lon <- data_to_add %>% 
  filter(!is.na(site_name)) %>% 
  distinct(site_name) %>% 
  mutate_geocode(site_name) %>% 
  mutate_all(as.character)

#add lat/lon and taxa info to data
new_data_latlon <- as_tibble(data_to_add) %>% 
  full_join(lat_lon, by = "site_name") %>% 
  mutate(dateadded = as.character(Sys.Date()),
         measure_numeric = as.numeric(measurement)) %>%
  mutate(measure_numeric = case_when(measurement == "trace" ~ 0.0001,
                                     measurement == "-" ~ 0,
                                     measurement == "0-0" ~ 0,
                                     # measurement == "0  12" ~ 0.12,
                                     # measurement == "check" & author == "Keast" ~ 11,
                                     # measurement == "check" ~ 0,
                                     is.na(measurement) ~ 0,
                                     TRUE ~ measure_numeric)) %>% 
  unite(measurement_typeunits, c(measurement_type, measurement_units), sep = " ", remove = F) %>% 
  mutate(type_temp = case_when(grepl("umber", measurement_typeunits) ~ "abundance",
                               grepl("bundanc", measurement_typeunits) ~ "abundance",
                               grepl("umeri", measurement_typeunits) ~ "abundance",
                               grepl("omposit", measurement_typeunits) ~ "abundance",
                               grepl('area', measurement_typeunits) ~ "area",
                               grepl("individ", measurement_typeunits) ~ "abundance",
                               grepl("total", measurement_typeunits) ~ "abundance",
                               grepl("rganis", measurement_typeunits) ~ "abundance",
                               grepl("cm3", measurement_typeunits) ~ "volume",
                               grepl("olume", measurement_typeunits) ~ "volume",
                               grepl("eight", measurement_typeunits) ~ "biomass",
                               grepl("iomass", measurement_typeunits) ~ "biomass",
                               grepl("Mass", measurement_typeunits) ~ "biomass",
                               grepl("mg", measurement_typeunits) ~ "biomass"),
         units_temp = case_when(grepl("ercen", measurement_typeunits) ~ "percent",
                                grepl("roport", measurement_typeunits) ~ "proportion",
                                grepl("umber", measurement_typeunits) ~ "individual",
                                grepl("bundanc", measurement_typeunits) ~ "individual",
                                grepl("umeri", measurement_typeunits) ~ "individual",
                                grepl("cm3", measurement_typeunits) ~ "cm3",
                                grepl("grams", measurement_typeunits) ~ "g",
                                grepl("mg", measurement_typeunits) ~ "mg",
                                grepl("mL", measurement_typeunits) ~ "ml"),
         measurement_typeunits = paste(type_temp, units_temp, sep = "_")) %>% 
  mutate_all(as.character)


# Append new data to existing data ----------------------------------------

# # load master data frame and save a backup
# data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>%
#   mutate_all(as.character) %>%
#   remove_empty("rows")
# 
# write.csv(data_fish,file = paste0("database/data_backups/data_fish", Sys.Date(),".csv"),row.names = F)
# 

#stack new data to old data and create a numeric column of measures (check for new cases)
data_fish_update <- bind_rows(data_fish, new_data_latlon) 
  # mutate(fish_id = case_when(fish_id == "NA" ~ as.numeric(as.factor(fish_id_new)),
  #                            TRUE ~ as.numeric(as.factor(fish_id_add))))

percent_not_100 <- data_fish_update %>% 
  distinct() %>% 
  unite(author_year_tbl, c("author", "year", "table_figure"),sep = "_", remove = F) %>% 
  distinct() %>% 
  filter(grepl("percent", measurement_typeunits)) %>% 
  group_by(fish_id,  citation, author_year_tbl, sample_size, start_date) %>% 
  summarize(total = sum(as.numeric(measure_numeric))) %>% 
  filter(total <= 98 | total >= 102) %>% 
  ungroup() %>% 
  distinct(fish_id,  total, citation, author_year_tbl, sample_size, start_date) %>% 
  arrange(-total) 

write.csv(percent_not_100, file = paste0("database/data_to_add/re_do/percent_not_100",Sys.Date(),".csv"), row.names = F)

data_fish_update2 <- anti_join(data_fish_update, percent_not_100, by = "fish_id")

data_fish_old <- data_fish
data_fish <- data_fish_update2
#save updated data as "data_fish.rds"
saveRDS(data_fish, file = "database/data_fish.rds")



# Clear files -------------------------------------------------------------
# Move csv's that have been added to a folder called "data_already_added"
move_files_from <- list.files(path = "./database/data_to_add",pattern = "*.csv")
move_files_to <- paste0("./database/data_already_added/", move_files_from)
file.rename(from = paste0("./database/data_to_add/",move_files_from), to = move_files_to)

# Move csv's that have been added to a folder called "data_already_added"
move_folders_from <- list.files(path = paste0("./database/data_to_add/", folder))
move_files_to <- paste0("./database/data_already_added/", folder)
file.rename(from = paste0("./database/data_to_add/",move_files_from), to = move_files_to)

# dir.create(paste0("./database/data_already_added/", folder))
# 
# folder_old_path = "C:/Users/abc/Downloads/managerA"
# path_new = "C:/User/abc/Desktop/managerA"
# file.copy(from = move_folders_from, to = move_files_to, recursive = FALSE, copy.mode = TRUE)


# PUSH CHANGES TO GITHUB 
