library(brms)
library(tidyverse)
library(ggridges)
library(lubridate)
library(httr)
library(cowplot)
library(janitor)
library(repmis)

# Load data ---------------------------------------------------
data_fish_current <- readRDS("~/GitHub/Freshwater-Fish-Diet-Database/database/data_fish.rds")
#get column names
cols <- names(data_fish_current)


# Functions ---------------------------------------------------------------
fish_gather <- function(dt) {
  dt <- dt  %>% 
    clean_names() %>% 
    gather(prey_taxon, measurement, -"fish_genus_species":-"data_sorted_by") %>% 
    mutate(measurement = as.numeric(measurement))}

#function to add columns to the new data sheets if not already included
fish_cols <- function(data, cname) {
  add <- cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
}

# make list of filenames in your data folder
filename <- list.files("database/data_to_add/2020-01-15")

# empty object to put data into
data.list <- NULL

# add files to list
for(name in seq_along(filename)){
  data.list[[name]] <- read.csv(paste("database/data_to_add/2020-01-15/", filename[name], sep = ""),fileEncoding="UTF-8-BOM")
}

# make a new data frame + add columns to it that are in the master data frame
new_data <- bind_rows(unclass(lapply(data.list, FUN = fish_gather))) %>% 
  fish_cols(cols)

#append data to master data frame
data_fish <- rbind(new_data, data_fish_current)

#save updated version
saveRDS(data_fish, file = "database/data_fish.rds")




