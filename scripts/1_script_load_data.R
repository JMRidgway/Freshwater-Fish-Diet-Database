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

#Use this script to: add new data to the master data frame, add lat/lon info, and exctract stage information

# Load master data frame *** make sure its the most recent one *** ---------------------------------------------------
data_fish_current <- readRDS("~/GitHub/Freshwater-Fish-Diet-Database/database/data_fish.rds") %>% 
  mutate_all(funs("as.character")) 

#folder to import from - name of folder in the working directory that contains extracted csvs to add
folder <- "data_already_added/2020-01-18"

# Functions ---------------------------------------------------------------
#fish_gather cleans and gathers the imported data and adds life stage information
fish_gather <- function(dt) {
  dt <- dt  %>% 
    clean_names() %>% 
    remove_empty("rows") %>% 
    rename_at(1, ~"site_name") %>%
    gather(prey_taxon, measurement, -"site_name":-"notes") %>% 
    mutate_each(funs("as.character")) %>% 
    mutate(prey_stage = case_when(grepl("arva", prey_taxon) ~"larvae",
                                  grepl("adult",prey_taxon) ~ "adults",
                                  grepl("ymph", prey_taxon) ~ "larvae",
                                  grepl("upae", prey_taxon) ~ "pupae",
                                  grepl("pupa", prey_taxon) ~ "pupae",
                                  grepl("Pupa", prey_taxon) ~ "pupae",
                                  grepl("immatur", prey_taxon) ~ "larvae",
                                  TRUE ~ "unknown"))}

# make list of filenames to import ***double check that folder is assigned correctly ***
filenames_csv <- list.files(paste("database/data_to_add/",folder, sep = ""), pattern = "*.csv")
filenames_xlsx <- list.files(paste("database/data_to_add/",folder, sep = ""), pattern = "*.xlsx")

# empty object to put data into - one for ,csv + one for .xlsx
data_csv.list <- NULL
data_xlsx.list <- NULL

# add files to empty objects
for(name in seq_along(filenames_csv)){
  data_csv.list[[name]] <- read.csv(paste("database/data_to_add/",folder,"/",filenames_csv[name], sep = ""),na.strings=c("","NA"))
}

for(name in seq_along(filenames_xlsx)){
  data_xlsx.list[[name]] <- read_excel(paste("database/data_to_add/",folder,"/", filenames_xlsx[name], sep = ""))
}

# fun the fish_gather function on each file, then make it into a dataframe
new_csv <- bind_rows(unclass(lapply(data_csv.list, FUN = fish_gather))) 
new_xlsx <- bind_rows(unclass(lapply(data_xlsx.list, FUN = fish_gather))) 

#combine csv and excel dataframes
combine_data <- bind_rows(new_csv, new_xlsx) %>%
#   mutate(start_date = parse_date_time(start_date, orders = c("ymd","dmy")),
#          end_date = parse_date_time(end_date, orders = c("ymd","dmy"))) %>% 
  mutate_all(funs('as.character')) 


#get lat/lon (requires API from google and internet connection - if it doesn't work, skip this step)
lat_lon <- combine_data %>% 
  filter(!is.na(site_name)) %>% 
  distinct(site_name) %>% 
  mutate_geocode(site_name) %>% 
  mutate_all(funs('as.character'))

#add lat/lon to data
new_data <- combine_data %>% left_join(lat_lon)

#append new data to master data frame and extract prey stage information
data_fish <- bind_rows(new_data, data_fish_current) 

#save updated version of master data frame
saveRDS(data_fish, file = "database/data_fish.rds")
#now take this data to open refine to clean
