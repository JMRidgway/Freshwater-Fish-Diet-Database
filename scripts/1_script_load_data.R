library(brms)
library(tidyverse)
library(ggridges)
library(lubridate)
library(httr)
library(cowplot)
library(janitor)
library(repmis)

# Load data ---------------------------------------------------
data_fish <- readRDS("~/GitHub/Freshwater-Fish-Diet-Database/database/data_fish.rds")

#get column names
cols <- names(data_fish)


# Functions ---------------------------------------------------------------
fish_gather <- function(dt) {
  dt <- dt %>% gather(prey_taxon, measurement, -"fish_genus_species":-"data_sorted_by") %>% 
      clean_names()}

#function to add columns to the new data sheets if not already included
fish_cols <- function(data, cname) {
  add <- cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
}

# make list of filenames in your data folder
filename <- list.files("./data_editedRaw/2020-01-13")

# empty object to put data into
data.list <- NULL

# for loop
for(name in seq_along(filename)){
  data.list[[name]] <- read.csv(paste("data_editedRaw/2020-01-13/", filename[name], sep = ""),fileEncoding="UTF-8-BOM")
}

# below this you could combine to one call _____________________________
new_data <- lapply(data.list, FUN = fish_gather)






test <- rbind.fill(data_fish, community_adite_1997_tbl3)

