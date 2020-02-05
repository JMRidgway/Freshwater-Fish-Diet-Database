#Use this script to tidy newly exctracted data, add lat/lon info, extract stage and taxanomic info

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



# Load master data set ----------------------------------------------------
data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
  mutate_all(funs('as.character')) %>% 
  remove_empty("rows")

#make a list of taxa names to append later for prey and fish ----------------------------------------------------------
#makes two sets, one with original spelling, and the other in sentence case.
prey_taxa_all <- data_fish %>% select(prey_taxon, prey_type, prey_kingdom, prey_phylum, prey_class, prey_order, prey_family, prey_genus, prey_species) %>% 
  distinct(prey_taxon, .keep_all = TRUE) %>% 
  mutate(prey_taxon_sent = str_to_sentence(prey_taxon)) %>% 
  gather(key, prey_taxon, c(prey_taxon, prey_taxon_sent)) %>% 
  select(-key)

fish_taxa_all <- data_fish %>% select(type_of_fish, fish_order, fish_family, fish_genus_species) %>% 
  distinct(type_of_fish, .keep_all = TRUE) %>% 
  mutate(type_of_fish_sent = str_to_sentence(type_of_fish)) %>% 
  gather(key, type_of_fish, c(type_of_fish, type_of_fish_sent)) %>% 
  select(-key)


#set folder to import from - name of folder in the working directory that contains extracted csvs to add
#MANUALLY CHANGE THE FOLDER NAME BELOW #
folder <- "2020-01-27jeff"

# Functions ---------------------------------------------------------------
#fish_gather cleans and gathers the imported data and extracts life stage information
fish_gather <- function(dt) {
  dt <- dt  %>% 
    clean_names() %>% 
    remove_empty("rows") %>% 
    # rename_at(1, ~"site_name") %>%
    gather(prey_taxon, measurement, -"site_name":-"notes") %>% 
    mutate_each(funs("as.character")) %>% 
    mutate(prey_stage = case_when(grepl("arva", prey_taxon) ~"larvae",
                                  grepl("adult",prey_taxon) ~ "adults",
                                  grepl("ymph", prey_taxon) ~ "larvae",
                                  grepl("upae", prey_taxon) ~ "pupae",
                                  grepl("pupa", prey_taxon) ~ "pupae",
                                  grepl("Pupa", prey_taxon) ~ "pupae",
                                  grepl("immatur", prey_taxon) ~ "larvae",
                                  grepl("notadult", prey_taxon) ~ "larvae/pupae",
                                  TRUE ~ "unknown"))}



# Get files, tidy, and stack ----------------------------------------------

# make lists of filenames to import ***double check that folder is assigned correctly ***
filenames_csv <- list.files(paste("database/data_to_add/",folder, sep = ""), pattern = "*.csv")
filenames_xlsx <- list.files(paste("database/data_to_add/",folder, sep = ""), pattern = "*.xlsx")

# empty objects to put data into - one for ,csv + one for .xlsx
data_csv.list <- NULL
data_xlsx.list <- NULL

# add files to empty objects
for(name in seq_along(filenames_csv)){
  data_csv.list[[name]] <- read.csv(paste("database/data_to_add/",folder,"/",filenames_csv[name], sep = ""),na.strings=c("","NA"))
}

for(name in seq_along(filenames_xlsx)){
  data_xlsx.list[[name]] <- read_excel(paste("database/data_to_add/",folder,"/", filenames_xlsx[name], sep = ""))
}

# run the fish_gather function on each file, then make it into a dataframe
new_csv <- bind_rows(unclass(lapply(data_csv.list, FUN = fish_gather))) 
new_xlsx <- bind_rows(unclass(lapply(data_xlsx.list, FUN = fish_gather))) 

#combine csv and excel dataframes
combine_data <- bind_rows(new_csv, new_xlsx) %>% 
  mutate_all(funs('as.character'))

#save a copy as a csv
write.csv(combine_data, file = paste0("database/data_to_add/",folder,".csv"),row.names=FALSE)


#----------REPEAT FOR ALL FOLDERS WITH FILES TO ADD----------------

