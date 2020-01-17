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

# Load master data frame ---------------------------------------------------
data_fish_current <- readRDS("~/GitHub/Freshwater-Fish-Diet-Database/database/data_fish.rds") %>% 
  mutate_all(funs("as.character"))


# Functions ---------------------------------------------------------------
fish_gather <- function(dt) {
  dt <- dt  %>% 
    clean_names() %>% 
    rename_at(1, ~"site_name") %>% 
    gather(prey_taxon, measurement, -"site_name":-"notes") %>% 
    mutate_each(funs("as.character"))}


# Import data -------------------------------------------------------------

#folder to import from
folder <- "2020-01-06"

# make list of filenames from the data in your folder
filenames_csv <- list.files(paste("database/data_to_add/",folder, sep = ""), pattern = "*.csv")
filenames_xlsx <- list.files(paste("database/data_to_add/",folder, sep = ""), pattern = "*.xlsx")

# empty object to put data into - one for ,csv + one for .xlsx
data_csv.list <- NULL
data_xlsx.list <- NULL

# add files to list
for(name in seq_along(filenames_csv)){
  data_csv.list[[name]] <- read.csv(paste("database/data_to_add/",folder,"/",filenames_csv[name], sep = ""))
}

for(name in seq_along(filenames_xlsx)){
  data_xlsx.list[[name]] <- read_excel(paste("database/data_to_add/",folder,"/", filenames_xlsx[name], sep = ""))
}

# make a new data frame + add columns to it that are in the master data frame
new_csv <- bind_rows(unclass(lapply(data_csv.list, FUN = fish_gather))) 
new_xlsx <- bind_rows(unclass(lapply(data_xlsx.list, FUN = fish_gather))) 
new_data <- bind_rows(new_csv, new_xlsx) %>% 
  mutate(start_date = parse_date_time(start_date, orders = c("ymd","dmy")),
         end_date = parse_date_time(end_date, orders = c("ymd","dmy"))) %>% 
  mutate_all(funs('as.character'))


#append data to master data frame
data_fish <- bind_rows(new_data, data_fish_current) 

#save updated version of master data frame
saveRDS(data_fish, file = "database/data_fish.rds")


