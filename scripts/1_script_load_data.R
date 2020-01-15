library(brms)
library(tidyverse)
library(ggridges)
library(lubridate)
library(httr)
library(cowplot)
library(janitor)
library(repmis)

# Load most recent data from github ---------------------------------------------------
data_fish <- readRDS("~/GitHub/Freshwater-Fish-Diet-Database/database/data_fish.rds")

#get column names
cols <- names(data_fish)

#function to add columns to the new data sheets data_sheet if not already included
fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
}

# make list of filenames in your data folder
filename <- list.files("./database/data_to_add")

# empty object to put data into
data.list <- NULL

# for loop
for(name in seq_along(filename)){
  data.list[[name]] <- read.csv(paste("database/data_to_add/", filename[name], sep = ""))
}

# below this you could combine to one call _____________________________
new_data <- lapply(data.list, FUN = fishTidy)

gather(prey_taxon, measurement, -"site_name":-"notes")

setdiff(centr_cols, cols)

