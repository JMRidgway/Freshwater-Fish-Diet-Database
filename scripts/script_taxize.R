
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
library(taxize)

#full database
data_fish_temp <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
  mutate_all(funs('as.character')) %>% 
  remove_empty("rows")

#taxa we have
prey_taxa_all <- read.csv(text = getURL("https://raw.githubusercontent.com/JMRidgway/Freshwater-Fish-Diet-Database/master/prey_taxa_all.csv"))
fish_taxa_all <- read.csv(text = getURL("https://raw.githubusercontent.com/JMRidgway/Freshwater-Fish-Diet-Database/master/fish_taxa_all.csv"))

#taxa names distinct
prey_taxa_needed <- data_fish_temp %>% 
  filter(is.na(prey_kingdom)) %>% 
  group_by(prey_taxon) %>% 
  tally() %>% 
  drop_na() %>% 
  left_join(prey_taxa_to_clean) %>% 
  distinct(search_term) %>% 
  drop_na()

prey_taxa_needed <- data_fish_temp %>% 
  filter(is.na(prey_kingdom)) %>% 
  distinct(prey_taxon)
write.csv(prey_taxa_needed, file = "prey_taxa_needed.csv")


fish_taxa_needed <- data_fish_temp %>% filter(is.na(fish_family)) %>% 
  distinct(type_of_fish, .keep_all = T) 
 
prey_taxa_gnr <- gnr_resolve(names = prey_taxa_needed$search_term, best_match_only = T)

prey_taxa_gnr_distinct <- distinct(prey_taxa_gnr, matched_name)
prey_taxa_added <- classification(prey_taxa_needed$search_term, db = "eol", 
                                  return_id = T)

prey_taxa_to_add <- rbind(prey_taxa_added) %>% 
  filter(rank != "no rank") %>% 
  select(-id) %>% 
  pivot_wider(names_from = rank,
              values_from = name) %>% 
  select(query, kingdom, class, order, family, genus, species) %>% 
  rename(prey_taxon = query,
         prey_kingdom_add = kingdom,
         prey_class_add = class,
         prey_order_add = order,
         prey_family_add = family,
         prey_genus_add = genus,
         prey_species_add = species) %>% 
  # right_join(prey_taxa_gnr, by = "matched_name") %>% 
  # rename(prey_taxon = user_supplied_name) %>% 
  select(prey_taxon, prey_kingdom_add, prey_class_add, 
         prey_order_add, prey_family_add, prey_species_add, prey_genus_add) 
  
# 
# 
# duplicates <- prey_taxa_to_add %>% 
#   group_by(prey_taxon) %>%
#   filter(n()>1) %>% arrange(prey_taxon) %>% 
#   ungroup() %>% 
#   mutate(delete_row = delete) %>% 
#   unite(delete_id, c("prey_taxon", "prey_kingdom_add","prey_class_add","prey_order_add"),
#         remove = F)
# # write.csv(duplicates, file = "duplicates.csv")
# 
# delete <- c(NA,
#             "delete",
#             NA,
#             "delete",
#             NA,
#             "delete",
#             NA,
#             "delete",
#             NA,
#             "delete",
#             NA,
#             "delete")
# 
# prey_taxa_to_add_full <- prey_taxa_to_add %>% 
#   unite(delete_id, c("prey_taxon", "prey_kingdom_add","prey_class_add","prey_order_add"),
#       remove = F) %>% 
#   left_join(duplicates) %>% 
#   filter(delete_row != "delete" | is.na(delete_row)) %>% 
#   select(-delete_id) %>% 
#   distinct()
# 
# prey_taxa_to_add_full %>%
#   group_by(prey_taxon) %>%
#   filter(n()>1) %>% arrange(prey_taxon) %>% 
#   distinct() %>% View()
#   

prey_taxa_to_add <- read_csv("prey_taxa_needed.csv")

test_fish <- data_fish_temp %>% 
  left_join(prey_taxa_to_add) %>%
  mutate(prey_kingdom = case_when(is.na(prey_kingdom) ~ prey_kingdom_add,
                                  TRUE ~ prey_kingdom),
         prey_phylum = case_when(is.na(prey_phylum) ~ prey_phylum_add,
                                 TRUE ~ prey_phylum),
         prey_class = case_when(is.na(prey_class) ~ prey_class_add,
                                TRUE ~ prey_class),
         prey_order = case_when(is.na(prey_order) ~ prey_order_add,
                                TRUE ~ prey_order),
         prey_family = case_when(is.na(prey_family) ~ prey_family_add,
                                 TRUE ~ prey_family),
         prey_genus = case_when(is.na(prey_genus) ~ prey_genus_add,
                                 TRUE ~ prey_genus),
         prey_species = case_when(is.na(prey_species) ~ prey_species_add,
                                  TRUE ~ prey_species)) %>% 
  select(-contains("_add"))


data_fish %>% group_by(prey_class) %>% tally() %>% arrange(-n)
test_fish %>% group_by(prey_class) %>% tally() %>% arrange(-n)

# 
data_fish <- test_fish
saveRDS(data_fish, file = "database/data_fish.rds")



# Taxize fish -------------------------------------------------------------

#full database
data_fish_temp <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
  mutate_all(funs('as.character')) %>% 
  remove_empty("rows")

#taxa we have
prey_taxa_all <- read.csv(text = getURL("https://raw.githubusercontent.com/JMRidgway/Freshwater-Fish-Diet-Database/master/prey_taxa_all.csv"))
fish_taxa_all <- read.csv(text = getURL("https://raw.githubusercontent.com/JMRidgway/Freshwater-Fish-Diet-Database/master/fish_taxa_all.csv"))

#taxa names distinct
fish_taxa_needed <- data_fish_temp %>% filter(is.na(fish_family)) %>% 
  distinct(type_of_fish, .keep_all = T) 

fish_taxa_gnr <- gnr_resolve(names = fish_taxa_needed$type_of_fish, best_match_only = T)


fish_taxa_gnr_distinct <- distinct(fish_taxa_gnr, matched_name)


fish_taxa_needed <- read_csv("fish_taxa_needed.csv")
fish_taxa_added <- classification(fish_taxa_needed$search_fish, db = "gbif", 
                                  return_id = T)

fish_taxa_to_add <- rbind(fish_taxa_added) %>% 
  filter(rank != "no rank") %>% 
  select(-id) %>% 
  pivot_wider(names_from = rank,
              values_from = name) %>% 
  select(query, order, family, species) %>% 
  rename(matched_name = query,
         fish_order_add = order,
         fish_family_add = family, 
         fish_species_add = species) %>% 
  right_join(fish_taxa_gnr, by = "matched_name") %>% 
  rename(type_of_fish = user_supplied_name) %>% 
  select(type_of_fish, fish_order_add, fish_family_add, fish_species_add) 

#check for duplicates
fish_taxa_to_add %>% 
  group_by(type_of_fish) %>%
  filter(n()>1) %>% arrange(type_of_fish) 

#export and add missing info by hand
write.csv(fish_taxa_to_add, file = "fish_taxa_to_add.csv", row.names = F)

#bring it back in
fish_taxa_to_add <- read_csv("fish_taxa_to_add.csv")

test_fish <- data_fish_temp %>% 
  left_join(fish_taxa_to_add) %>% 
  mutate(fish_order = case_when(is.na(fish_order) ~ fish_order_add,
                                TRUE ~ fish_order),
         fish_family = case_when(is.na(fish_family) ~ fish_family_add,
                                 TRUE ~ fish_family),
         fish_species = case_when(is.na(fish_species) ~ fish_species_add,
                                  TRUE ~ fish_species)) %>% 
  select(-contains("_add"))


data_fish %>% group_by(fish_order) %>% tally() %>% arrange(-n)
test_fish %>% group_by(fish_order) %>% tally() %>% arrange(-n)


data_fish <- test_fish
saveRDS(data_fish, file = "database/data_fish.rds")


# # 
# fish_taxa_to_add <- data_fish %>% filter(is.na(fish_order)) %>%
#   distinct(type_of_fish)
# write.csv(fish_taxa_to_add, file = "fish_taxa_to_add.csv", row.names = F)


