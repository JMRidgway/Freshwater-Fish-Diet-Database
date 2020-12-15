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
library(stringi)

data_to_add_ridgway <- read_csv("database/data_to_add.csv") %>% 
  select(-X, -X1) %>% select(-lat, -long)

data_to_add_ridgway %>% filter(grepl("Amblop", type_of_fish)) %>% distinct(type_of_fish,  author, citation)
new_data_latlon %>% filter(grepl("Amblop", type_of_fish)) %>% distinct(type_of_fish,  author, citation)
data_fish %>% filter(grepl("Amblop", type_of_fish)) %>% distinct(type_of_fish,  author, citation)


#get lat/lon (requires API from google and internet connection - if it doesn't work, skip this step)

missing_site_names_ridgway <- data_to_add_ridgway %>% 
  filter(is.na(site_name))

write.csv(missing_site_names_ridgway, file = paste0("database/data_to_add/missing_site_names",Sys.Date(), ".csv"))


lat_lon <- data_to_add_ridgway %>% 
  filter(!is.na(site_name)) %>% 
  distinct(site_name) %>% 
  mutate_geocode(site_name) %>% 
  mutate_all(as.character) 	


#add lat/lon and taxa info to data
#LATEST dataset as of 2020-12-14
new_data_latlon_fishized_preyized_fixed_dups_fixed_prey <- readRDS("database/new_data_latlon_fishized_preyized_fixed_dups_fixed_prey.rds")



# new_data_latlon <- as_tibble(data_to_add_ridgway) %>% 
#   left_join(lat_lon, by = "site_name") %>% 
#   mutate(dateadded = as.character(Sys.Date()),
#          measure_numeric = as.numeric(measurement)) %>%
#   mutate(measure_numeric = case_when(measurement == "trace" ~ 0.0001,
#                                      measurement == "-" ~ 0,
#                                      measurement == "0-0" ~ 0,
#                                      # measurement == "0  12" ~ 0.12,
#                                      # measurement == "check" & author == "Keast" ~ 11,
#                                      # measurement == "check" ~ 0,
#                                      is.na(measurement) ~ 0,
#                                      TRUE ~ measure_numeric)) %>% 
#   unite(measurement_typeunits, c(measurement_type, measurement_units), sep = " ", remove = F) %>% 
#   mutate(type_temp = case_when(grepl("umber", measurement_typeunits) ~ "abundance",
#                                grepl("bundanc", measurement_typeunits) ~ "abundance",
#                                grepl("umeri", measurement_typeunits) ~ "abundance",
#                                grepl("omposit", measurement_typeunits) ~ "abundance",
#                                grepl('area', measurement_typeunits) ~ "area",
#                                grepl("individ", measurement_typeunits) ~ "abundance",
#                                grepl("total", measurement_typeunits) ~ "abundance",
#                                grepl("rganis", measurement_typeunits) ~ "abundance",
#                                grepl("cm3", measurement_typeunits) ~ "volume",
#                                grepl("olume", measurement_typeunits) ~ "volume",
#                                grepl("eight", measurement_typeunits) ~ "biomass",
#                                grepl("iomass", measurement_typeunits) ~ "biomass",
#                                grepl("Mass", measurement_typeunits) ~ "biomass",
#                                grepl("mg", measurement_typeunits) ~ "biomass"),
#          units_temp = case_when(grepl("ercen", measurement_typeunits) ~ "percent",
#                                 grepl("roport", measurement_typeunits) ~ "proportion",
#                                 grepl("umber", measurement_typeunits) ~ "individual",
#                                 grepl("bundanc", measurement_typeunits) ~ "individual",
#                                 grepl("umeri", measurement_typeunits) ~ "individual",
#                                 grepl("cm3", measurement_typeunits) ~ "cm3",
#                                 grepl("grams", measurement_typeunits) ~ "g",
#                                 grepl("mg", measurement_typeunits) ~ "mg",
#                                 grepl("mL", measurement_typeunits) ~ "ml"),
#          measurement_typeunits = paste(type_temp, units_temp, sep = "_")) %>% 
#   mutate_all(as.character)

new_data_latlon <- new_data_latlon %>% select(-measurement_type_number_weight_other, -measurement_units_total_percent_other)

saveRDS(new_data_latlon, file = "database/data_to_add/new_data_latlon.rds")

names_new <- names(new_data_latlon)
names_old <- names(data_fish)

setdiff(names_new, names_old)
setdiff(names_old, names_new)

# Check fish species
new_data_latlon %>% filter(grepl("cryso", type_of_fish)) %>% distinct(type_of_fish,  author, citation)
data_fish %>% filter(grepl("cryso", type_of_fish)) %>% distinct(type_of_fish,  author, citation)




# Check columns -------------------------
unique(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$sample_size) #good as of 2020-12-13)
unique(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$start_date) #good as of 2020-12-13
unique(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$end_date) #good as of 2020-12-13
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$measurement_type)) # TODOs Reduce categories to those in data_fish (2012-12-13)
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$measurement_typeunits)) # TODOs Reduce categories to those in data_fish (2012-12-13)
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$measurement_units)) # TODOs Reduce categories to those in data_fish (2012-12-13)
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$year)) #good as of 2020-12-13
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$habitat)) # TODOs Reduce categories to those in data_fish (2012-12-13)
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$habitat_general)) #STOPPED HERE ON 12-13-2020 - RESUME HERE LATER
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$table_figure)) #good as of 2020-12-13
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$measurement)) #Fixed on 2020-12-14 - GOOD
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$author)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$citation)) #good, but need to clean up as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$prey_stage)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$sampling_interval)) #Fixed on 2020-12-14 - GOOD
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$lat)) #Fixed on 2020-12-14 - GOOD
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$lon)) #Fixed on 2020-12-14 - GOOD
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$journal)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$site_name)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$predator_stage)) #good, but clean up in final file
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$length_measure)) #Fixed on 2020-12-14 - GOOD
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$length_units)) #good as of 2020-12-14, but units are in cm, mm, in, etc
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$predator_min_length)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$predator_max_length)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$type_of_fish)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$microhabitat)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$author_year_tbl)) #empty as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$prey_taxon)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$predator_average_length)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$fish_id)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$data_sorted_by)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$dateadded)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$prey_kingdom)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$prey_class)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$prey_family)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$prey_order)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$prey_species)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$fish_genus)) #good as of 2020-12-14, but why is it here?
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$fish_species)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$fish_family)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$fish_class)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$fish_order)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$fish_superclass)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$sample_id)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$type_temp)) #good as of 2020-12-14
unique(sort(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey$units_temp)) #good as of 2020-12-14

library(dplyr)
#fixes to make
temp <- new_data_latlon_fishized_preyized_fixed_dups_fixed_prey %>% 
  mutate(measure_numeric = case_when(measurement == "2F34" ~ "21.34", 
                                     measurement == "Fl" ~ "1.1",
                                     measurement == "g" ~ "48",
                                     measurement == "n" ~ "74",
                                     measurement == "2.1." ~ "2.1", 
                                     TRUE ~ measure_numeric),
         measure_numeric = str_trim(measure_numeric),
         measure_numeric = case_when(measurement == "24.  6" ~ "24.6",
                                     measurement == "31.  6" ~ "31.6",
                                     measurement == "37,5" ~ "37.5",
                                     measurement == "4,2" ~ "4.2",
                                     measurement == "7.  2" ~ "7.2",
                                     measurement == "I" ~ "1",
                                     TRUE ~ measure_numeric),
         measure_numeric = case_when(grepl("tr", measurement) ~ "0.0001", 
                                     grepl("Tr", measurement) ~ "0.0001", 
                                     grepl("t", measurement) ~ "0.0001",
                                     grepl("<1", measurement) ~ "0.5",
                                     grepl("<0.1", measurement) ~ "0.05",
                                     grepl("<0.01", measurement) ~ "0.005",
                                     grepl("<0.001", measurement) ~ "0.0005",
                                     TRUE ~ measure_numeric)) %>% 
  mutate(sampling_interval = case_when(sampling_interval == "10/1/2009" ~ "NA", TRUE ~ sampling_interval),
         lat = case_when(is.na(lat) ~ "3.266667", TRUE ~ lat),
         lon = case_when(is.na(lon) ~ "60.583333", TRUE ~ lon)) %>% 
  mutate(length_measure = case_when(grepl("total", length_measure) ~ "total length",
                                    length_measure == "SL" ~ "standard length",
                                    length_measure == "TL" ~ "total length",
                                    TRUE ~ length_measure)) %>% 
  mutate(prey_order = case_when(prey_order == "Bdelloidea" ~ NA_character_,
                                prey_order == "Bacillariophyceae" ~ "Bacillariales",
                                TRUE ~ prey_order)) %>% 
  mutate(prey_class = case_when(prey_order == "Calanoida" ~ "Hexanauplia", 
                                TRUE ~ prey_class)) %>% 
  mutate(fish_superclass = case_when(fish_superclass == "Actinopterygii" ~ "Osteichthyes",
                                     TRUE ~ fish_superclass)) %>% 
  filter(fish_order != "Eulipotyphla")


intersect(temp$fish_class, temp$fish_superclass)

temp %>% filter(fish_order  == "Eulipotyphla") %>% distinct(type_of_fish)


# temp %>% distinct(length_measure)
# data_fish %>% distinct(length_measure)

# temp %>%
#   filter(is.na(measure_numeric)) %>% 
#   group_by(measurement, citation, fish_id, sample_id, table_figure) %>% 
#   tally() %>% 
#   mutate(temp = parse_number(measurement)) %>% 
#   View()

# temp %>% 
#   filter(sampling_interval == "10/1/2009") %>% 
#   distinct(citation)


new_data_latlon_fishized_preyized_fixed_dups_fixed_prey_CLEAN <- temp
saveRDS(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey_CLEAN, file = 
          "database/new_data_latlon_fishized_preyized_fixed_dups_fixed_prey_CLEAN.rds")  

saveRDS(data_fish, file = paste0("data_fish", Sys.Date(), ".rds"))

data_fish <- data_fish %>% bind_rows(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey_CLEAN)

saveRDS(data_fish, file = "database/data_fish.rds")

# Add Taxa ----------------------------------------------------------------

data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>%
  mutate_all(as.character) %>%
  remove_empty("rows")


fish_we_have <- data_fish %>% select(contains("fish")) %>%
  distinct(type_of_fish, fish_genus, fish_family, fish_species, fish_class, fish_superclass, fish_order)

fish_we_need <- new_data_latlon %>% distinct(type_of_fish) %>% arrange()

fish_added <- fish_we_need %>% 
  left_join(fish_we_have) %>% distinct(type_of_fish, .keep_all = T) %>% 
  filter(!is.na(fish_order))

new_data_latlon_fishized <- new_data_latlon %>% left_join(fish_added)


# prey species

prey_we_have <- data_fish %>% select(contains("prey")) %>% 
  distinct(prey_taxon, .keep_all = T)

prey_we_need <- new_data_latlon_fishized %>% distinct(prey_taxon)

prey_added <- prey_we_need %>% 
  left_join(prey_we_have) %>% distinct(prey_taxon, .keep_all = T)

new_data_latlon_fishized_preyized <- new_data_latlon_fishized %>% left_join(prey_added) %>% 
  filter(type_of_fish != "Galemys pyrenaicu") #remove a mammal

prey_still_needed <- new_data_latlon_fishized_preyized %>% filter(is.na(prey_kingdom)) %>% distinct(prey_taxon)
fish_still_needed <- new_data_latlon_fishized_preyized %>% filter(is.na(fish_class)) %>% distinct(type_of_fish)


write.csv(prey_still_needed, file = "database/prey_still_needed.csv")
write.csv(fish_still_needed, file = "database/fish_still_needed.csv")
write.csv(prey_we_have, file = "database/prey_we_have.csv")
write.csv(fish_we_have, file = "database/fish_we_have.csv")


# Taxize Fish ------------
# Fish

library(rfishbase)
library(tidyverse)
library(taxize)

#fish taxon info needed
# fish_taxa_all <- data_fish %>% filter(is.na(fish_family)) %>% distinct(type_of_fish) %>% 
#   mutate(type_fishmatch = str_replace(type_of_fish, "_", " ")) 

fish_taxa_all <- fish_still_needed %>% distinct(type_of_fish) %>% 
  mutate(type_fishmatch = str_replace(type_of_fish, "_", " ")) 

# #use fishbase to find synonyms
# fish_syn <- rfishbase::synonyms(fish_taxa_all$type_fishmatch, server = "fishbase")
# #load fishbase taxon info for all fish
# fb_taxa <- load_taxa("FishBase")

# #add taxon info
# fish_taxa_toadd <- fish_syn %>% 
#   filter(Status == "synonym" | Status == "accepted name") %>% 
#   select(synonym, Species) %>% 
#   rename(type_fishmatch = synonym) %>% 
#   right_join(fish_taxa_all) %>% 
#   mutate(fish_species = Species) %>%
#   tibble() %>% 
#   left_join(fb_taxa) %>%
#   rename(fish_family =  Family,
#          fish_order = Order) %>% 
#   rename(fish_class = Class,
#          fish_superclass = SuperClass) %>% 
#   select(contains("fish")) %>% 
#   tibble() %>% 
#   mutate(type_lower = tolower(type_of_fish))
# 
# #taxa still needed
# fish_taxa_needed <- fish_taxa_toadd %>% filter(is.na(fish_species)) 
# 
# #taxa still needed - unique names to taxize
# fish_taxa_needed_unique <- fish_taxa_needed  %>% 
#   select(type_lower) %>% 
#   ungroup() %>% 
#   distinct(type_lower)

#taxize
fish_classify <- classification(fish_taxa_all$type_of_fish, db = "ncbi", 
                                return_id = T)

#reshape to merge with original
fish_taxized <- fish_classify %>% rbind() %>% as_tibble() %>% 
  filter(rank != "no rank") %>%
  select(-id) %>% 
  distinct(rank, query, .keep_all = T) %>% 
  pivot_wider(names_from = rank,
              values_from = name) %>% 
  select(query,class, order, family, species, superclass) %>% 
  setNames(paste0('taxized_', names(.))) %>% 
  rename(type_of_fish = taxized_query)


fish_taxized2 <- new_data_latlon_fishized_preyized %>% left_join(fish_taxized) %>% 
  mutate(fish_order = case_when(is.na(fish_order) ~ taxized_order, TRUE ~ fish_order),
         fish_class = case_when(is.na(fish_class) ~ taxized_class, TRUE ~ fish_class),
         fish_species = case_when(is.na(fish_species) ~ taxized_species, TRUE ~ fish_species),
         fish_family = case_when(is.na(fish_family) ~ taxized_family, TRUE ~ fish_family),
         fish_superclass = case_when(is.na(fish_superclass) ~ taxized_superclass, TRUE ~ fish_superclass)) %>% 
  select(!contains("taxized"))


#still needed
# add_by_hand <- fish_taxized2 %>% filter(is.na(fish_order)) %>% distinct(type_of_fish)
# write.csv(add_by_hand, file = "add_by_hand.csv", row.names = F)
added_by_hand <- read_csv(file = "add_by_hand.csv")

fish_taxized3 <- fish_taxized2 %>% left_join(added_by_hand) %>% 
  mutate(fish_order = case_when(is.na(fish_order) ~ taxized_order, TRUE ~ fish_order),
         fish_class = case_when(is.na(fish_class) ~ taxized_class, TRUE ~ fish_class),
         fish_species = case_when(is.na(fish_species) ~ taxized_species, TRUE ~ fish_species),
         fish_family = case_when(is.na(fish_family) ~ taxized_family, TRUE ~ fish_family),
         fish_superclass = case_when(is.na(fish_superclass) ~ taxized_superclass, TRUE ~ fish_superclass)) %>% 
  select(!contains("taxized"))


new_data_latlon_fishized_preyized <- fish_taxized3

saveRDS(new_data_latlon_fishized_preyized, file = "database/new_data_latlon_fishized_preyized.rds")


# Check duplicates, percents to 1 or 100 ----------------------------------------------------------

library(tidyverse)
library(janitor)

new_data_latlon_fishized_preyized <- readRDS("database/new_data_latlon_fishized_preyized.rds")

# get duplicated fish ids
dup_fish_ids <- new_data_latlon_fishized_preyized %>% 
  group_by_at(vars(-fish_id)) %>% 
  filter(n() > 1) %>% ungroup() %>% select(fish_id) %>% 
  mutate(issue = "duplicate data")

# get ids for percents that don't add to ~ 100
perc_greaterthan_100_ids  <- new_data_latlon_fishized_preyized  %>% 
  group_by(fish_id, measurement_typeunits, fish_species, author, citation, type_of_fish) %>% 
  filter(grepl("percent", measurement_typeunits)) %>% 
  summarize(tot = sum(as.numeric(measurement))) %>% 
  filter(tot <=98 | tot >= 102) %>% ungroup() %>% select(fish_id) %>% 
  mutate(issue = "percent greather than 100")

#select them in the main dataset (use this to fix issues)
only_dups <- new_data_latlon_fishized_preyized %>% semi_join(dup_fish_ids) %>% mutate(issue = "duplicate data")
only_100s <- new_data_latlon_fishized_preyized %>% semi_join(perc_greaterthan_100_ids) %>% mutate(issue = "percent_not_100")

new_data_latlon_fishized_preyized_only_dups_only_percentgreater_100 <- bind_rows(only_dups, only_100s)

#combine those ids
dup_perc_tofix <- bind_rows(dup_fish_ids, perc_greaterthan_100_ids)

#remove them from the main dataset
new_data_latlon_fishized_preyized_no_dups_no_percentgreater_100 <- new_data_latlon_fishized_preyized %>% anti_join(dup_perc_tofix)

#check that it worked
#original number of unique ids
new_data_latlon_fishized_preyized %>% 
  distinct(fish_id) %>% nrow()

#number of ids to remove and number in the dataset to fix (should be the same number)
dup_perc_tofix %>% distinct(fish_id) %>% nrow()

new_data_latlon_fishized_preyized_only_dups_only_percentgreater_100 %>% 
  distinct(fish_id) %>% nrow()

#number ofremaining unique ids (should be the difference of the above two results)
new_data_latlon_fishized_preyized_no_dups_no_percentgreater_100 %>% 
  distinct(fish_id) %>% nrow()


#the clean data set
new_data_latlon_fishized_preyized_clean <- new_data_latlon_fishized_preyized_no_dups_no_percentgreater_100
saveRDS(new_data_latlon_fishized_preyized_clean, 
        file = "database/new_data_latlon_fishized_preyized_clean.rds")

#the dirty data set
saveRDS(new_data_latlon_fishized_preyized_only_dups_only_percentgreater_100, 
        file = "database/new_data_latlon_fishized_preyized_only_dups_only_percentgreater_100.rds")

write.csv(new_data_latlon_fishized_preyized_only_dups_only_percentgreater_100, 
          file = "database/new_data_latlon_fishized_preyized_only_dups_only_percentgreater_100.csv", row.names = F)



# NOW TRY TO FIX THEM
new_data_latlon_fishized_preyized_only_dups_only_percentgreater_100 <- readRDS("database/new_data_latlon_fishized_preyized_only_dups_only_percentgreater_100.rds")

only_dups <- new_data_latlon_fishized_preyized_only_dups_only_percentgreater_100 %>% filter(issue == "duplicate data")

# remove the duplicates by distinct sample_id 
removed_dups <- only_dups %>%
  group_by_at(vars(-fish_id)) %>% 
  distinct(sample_id, .keep_all = T) 

# check (should have zero rows)
removed_dups %>% 
  filter(n() > 1) %>% ungroup() %>% select(fish_id)

# check plot
only_dups %>% mutate(update = "dirty") %>% 
  bind_rows(removed_dups %>% mutate( update = "clean")) %>% 
  group_by(fish_id, measurement_typeunits, fish_species, update) %>% 
  filter(grepl("percent", measurement_typeunits)) %>% 
  summarize(tot = sum(as.numeric(measurement))) %>% 
  ggplot(aes(x = 1:nrow(.), y = tot, color = update)) + 
  geom_point() +
  ylim(50, 250)
            

# looks good. Add them back to the original dataset

new_data_latlon_fishized_preyized_fixed_dups <- new_data_latlon_fishized_preyized_clean %>% bind_rows(removed_dups)


# fixes below were found during prey taxizing
#after taxizing prey below, bring in added prey taxa

added_by_hand_prey <- read.csv("added_by_hand_prey.csv") %>%
  setNames(paste0('taxized_', names(.))) %>% 
  rename(prey_taxon = taxized_prey_taxon) %>% 
  mutate_if(is.factor, as.character)


new_data_latlon_fishized_preyized_fixed_dups_fixed_prey <- readRDS("database/new_data_latlon_fishized_preyized_fixed_dups.rds") %>% 
  glimpse() %>%
  filter(prey_taxon != "and eggs") %>% #should remove two instances
  filter(!grepl("cumulative", prey_taxon)) %>% 
  glimpse() %>% 
  filter(!grepl("Ofori-Danson", author)) %>% # removes it. It is from a saltwater lagoon, not freshwater
  glimpse() %>% 
  filter(!grepl("Walter and Austin", author)) %>% 
  glimpse() %>% 
  filter(prey_taxon != "x52") %>% 
  glimpse() %>% 
  filter(!grepl("Facade", author)) %>% 
  glimpse() %>% 
  left_join(added_by_hand_prey) %>% 
  glimpse() %>% 
  mutate(prey_phylum = case_when(is.na(prey_phylum) ~ taxized_prey_phylum, TRUE ~ prey_phylum),
         prey_stage = case_when(prey_stage == "unknwonw" ~ taxized_prey_stage, TRUE ~ prey_stage),
         prey_kingdom = case_when(is.na(prey_kingdom) ~ taxized_prey_kingdom, TRUE ~ prey_kingdom),
         prey_class = case_when(is.na(prey_class) ~ taxized_prey_class, TRUE ~ prey_class),
         prey_order = case_when(is.na(prey_order) ~ taxized_prey_order, TRUE ~ prey_order),
         prey_family = case_when(is.na(prey_family) ~ taxized_prey_family, TRUE ~ prey_family),
         prey_species = case_when(is.na(prey_species) ~ taxized_prey_species, TRUE ~ prey_species),
         prey_taxon_cleaned = case_when(is.na(prey_taxon_cleaned) ~ taxized_prey_taxon_cleaned, TRUE ~ prey_taxon_cleaned),
         prey_superclass = case_when(is.na(prey_superclass) ~ taxized_prey_superclass, TRUE ~ prey_superclass),
         prey_subclass = case_when(is.na(prey_subclass) ~ taxized_prey_subclass, TRUE ~ prey_subclass),
         prey_subphylum = case_when(is.na(prey_subphylum) ~ taxized_prey_subphylum, TRUE ~ prey_subphylum),
         prey_type = case_when(is.na(prey_type) ~ taxized_prey_type, TRUE ~ prey_type),
         prey_origin = case_when(is.na(prey_origin) ~ taxized_prey_origin, TRUE ~ prey_origin)) %>% 
  select(!contains("taxized")) %>% 
  glimpse()

saveRDS(new_data_latlon_fishized_preyized_fixed_dups_fixed_prey, file = "database/new_data_latlon_fishized_preyized_fixed_dups_fixed_prey.rds")








# Taxize Prey ------------
# Prey
Sys.setenv(ENTREZ_KEY = "d67218139a9e4ea75a44be3afb87378ff908")

library(rfishbase)
library(tidyverse)
library(taxize)
library(stringr)


prey_taxa_all <- new_data_latlon_fishized_preyized_fixed_dups %>% distinct(prey_taxon) %>% 
  separate(prey_taxon, c("first", "second", "third")) %>% 
  distinct(first) %>% 
  mutate(first = str_to_sentence(first))

#taxize
prey_classify <- classification(prey_taxa_all$first, db = "ncbi", 
                                return_id = T)

#reshape to merge with original
prey_taxized <- prey_classify %>% rbind() %>% as_tibble() %>% 
  filter(rank != "no rank") %>%
  select(-id) %>% 
  distinct(rank, query, .keep_all = T) %>% 
  pivot_wider(names_from = rank,
              values_from = name) %>% 
  select(query,kingdom, class, order, family, species, phylum, superclass, subclass, subphylum) %>% 
  setNames(paste0('taxized_', names(.))) %>% 
  rename(prey_taxon = taxized_query)


prey_taxized2 <- new_data_latlon_fishized_preyized_clean %>% left_join(prey_taxized) %>% 
  mutate(prey_order = case_when(is.na(prey_order) ~ taxized_order, TRUE ~ prey_order),
         prey_class = case_when(is.na(prey_class) ~ taxized_class, TRUE ~ prey_class),
         prey_species = case_when(is.na(prey_species) ~ taxized_species, TRUE ~ prey_species),
         prey_family = case_when(is.na(prey_family) ~ taxized_family, TRUE ~ prey_family),
         prey_superclass = case_when(is.na(prey_superclass) ~ taxized_superclass, TRUE ~ prey_superclass),
         prey_kingdom = case_when(is.na(prey_kingdom) ~ taxized_kingdom, TRUE ~ prey_kingdom),
         prey_phylum = case_when(is.na(prey_phylum) ~ taxized_phylum, TRUE ~ prey_phylum),
         prey_subphylum = case_when(is.na(prey_subphylum) ~ taxized_subphylum, TRUE ~ prey_subphylum),
         prey_subclass = case_when(is.na(prey_subclass) ~ taxized_subclass, TRUE ~ prey_subclass)) %>% 
  select(!contains("taxized"))


#still needed
# add_by_hand_prey <- prey_taxized2 %>% filter(is.na(prey_kingdom)) %>% distinct(prey_taxon)
# write.csv(add_by_hand_prey, file = "add_by_hand_prey.csv", row.names = F)
added_by_hand_prey <- read_csv(file = "add_by_hand_prey.csv")
prey_taxa_have <- data_fish %>% select(contains("prey")) %>% distinct(prey_taxon, .keep_all = T) %>% 
  write.csv(file = "prey_taxa_have.csv")

#after entering missing taxa info for prey by hand, this is what is left to interpret.
prey_still_dont_have_ridgway <- read.csv("database/prey_still_dont_have_ridgway.csv")

#match prey_still_dont_have_ridgway with their citations. Then check those and enter in the csv.

prey_still_dont_have_ridgway_withcitation <- new_data_latlon_fishized_preyized_fixed_dups %>% semi_join(prey_still_dont_have_ridgway) %>% 
  distinct(prey_taxon, citation, author, table_figure) %>% 
  distinct(prey_taxon, .keep_all = T)

write.csv(prey_still_dont_have_ridgway_withcitation, file = "prey_still_dont_have_ridgway_withcitation.csv", row.names = F)




fish_taxized3 <- fish_taxized2 %>% left_join(added_by_hand) %>% 
  mutate(fish_order = case_when(is.na(fish_order) ~ taxized_order, TRUE ~ fish_order),
         fish_class = case_when(is.na(fish_class) ~ taxized_class, TRUE ~ fish_class),
         fish_species = case_when(is.na(fish_species) ~ taxized_species, TRUE ~ fish_species),
         fish_family = case_when(is.na(fish_family) ~ taxized_family, TRUE ~ fish_family),
         fish_superclass = case_when(is.na(fish_superclass) ~ taxized_superclass, TRUE ~ fish_superclass)) %>% 
  select(!contains("taxized"))


new_data_latlon_fishized_preyized <- fish_taxized3

saveRDS(new_data_latlon_fishized_preyized, file = "database/new_data_latlon_fishized_preyized.rds")


# Add fixed dups to full dataset ------------------------------------------


# plot differences
new_data_latlon_fishized_preyized_fixed_dups_fixed_prey %>% 
  mutate(update = "clean") %>% 
  bind_rows(new_data_latlon_fishized_preyized_fixed_dups %>% mutate(update = "dirty")) %>% 
  group_by(fish_id, measurement_typeunits, fish_species, update) %>% 
  filter(grepl("percent", measurement_typeunits)) %>% 
  summarize(tot = sum(as.numeric(measurement))) %>% 
  ggplot(aes(x = 1:nrow(.), y = tot)) + 
  geom_point() +
  facet_wrap(~update)

#plot stage and aqterr data
fish_by_insectstage <- data_fish %>% 
  mutate(measure_numeric = as.numeric(measure_numeric)) %>% 
  filter(grepl("percent", measurement_typeunits),
         prey_class == "Insecta",
         prey_order == "Diptera",
         prey_origin == "aquatic") %>% 
  group_by(fish_species, prey_stage, fish_family) %>% 
  summarize(mean = mean(measure_numeric, na.rm = T),
            sd = sd(measure_numeric, na.rm = T))

fish_by_insectstage_prop <- fish_by_insectstage %>% 
  group_by(fish_species, fish_family) %>% 
  mutate(total = sum(mean),
         prop = mean/total)

fish_by_insectstage_prop %>% 
  ggplot(aes(x = reorder(prey_stage, -prop), y = prop, color = fish_family)) +
  geom_point(position = position_dodge(width = 0.1)) +
  geom_boxplot(aes(group = interaction(prey_stage, fish_family))) +
  guides(color = F)

fish_by_aqterr <- new_data_latlon_fishized_preyized_clean %>% 
  mutate(measure_numeric = as.numeric(measure_numeric)) %>% 
  # filter(grepl("percent", measurement_typeunits)) %>% 
  group_by(fish_species, prey_origin, measurement_typeunits) %>% 
  summarize(mean = mean(measure_numeric, na.rm = T),
            sd = sd(measure_numeric, na.rm = T))

fish_by_aqterr %>% 
  ggplot(aes(x = prey_origin, y = mean)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_boxplot(aes(group = prey_origin), outlier.shape = NA) +
  facet_wrap(~measurement_typeunits, scales = "free")



