library(brms)
library(tidyverse)
library(ggridges)
library(lubridate)
library(httr)
library(cowplot)
library(janitor)
library(repmis)

# Load data ---------------------------------------------------
data_fish_current <- readRDS("~/GitHub/Freshwater-Fish-Diet-Database/database/data_fish.rds") %>% 
  mutate_all(funs("as.character"))
#get column names
cols <- names(data_fish_current)


# Functions ---------------------------------------------------------------
fish_gather <- function(dt) {
  dt <- dt  %>% 
    clean_names() %>% 
    gather(prey_taxon, measurement, -"site_name":-"notes") %>% 
    mutate_each(funs("as.character"))}

# make list of filenames from the data in your folder
filename <- list.files("database/data_already_added/2019-12-14")

# empty object to put data into
data.list <- NULL

# add files to list
for(name in seq_along(filename)){
  data.list[[name]] <- read.csv(paste("database/data_already_added/2019-12-14/", filename[name], sep = ""))
}

# make a new data frame + add columns to it that are in the master data frame

new_data <- bind_rows(unclass(lapply(data.list, FUN = fish_gather))) 

#append data to master data frame
data_fish <- bind_rows(new_data, data_fish_current) %>% 
  mutate(type_of_fish = case_when(is.na(type_of_fish) ~ fish_genus_species,
                                  TRUE ~ type_of_fish))

#save updated version
saveRDS(data_fish, file = "database/data_fish.rds")

distinct(data_fish, author)



