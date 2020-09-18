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
data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
  mutate_all(funs('as.character')) %>% 
  remove_empty("rows")

prey_taxa_needed <- data_fish %>%
  filter(is.na(prey_kingdom) & is.na(prey_type)) %>% 
  group_by(prey_taxon, citation) %>% 
  tally() %>% 
  arrange(prey_taxon) %>% 
  filter(!grepl("Fragilaria", prey_taxon))

# write.csv(prey_taxa_needed, file = "prey_taxa_needed.csv", row.names = F)
  
prey_names_needed <- prey_taxa_needed %>% mutate(search_name = str_to_sentence(prey_taxon))

prey_taxa_classified <- classification(prey_names_needed$search_name[401:575], db = "gbif", return_id = T)

classified <- rbind(prey_taxa_classified) %>% distinct(name, rank, query) %>% 
  as_tibble() 


prey_taxa_to_add <- classified %>% 
  pivot_wider(names_from = rank,
              values_from = name) %>% 
  select(query, kingdom, phylum, class, order, family, species) %>% 
  rename(search_name = query,
         prey_kingdom_add = kingdom,
         prey_phylum_add = phylum,
         prey_class_add = class,
         prey_order_add = order,
         prey_family_add = family,
         prey_species_add = species) %>% 
  left_join(prey_names_needed) %>% 
  select(prey_kingdom_add, prey_class_add, 
         prey_order_add, prey_family_add, prey_phylum_add,
         prey_species_add, prey_taxon) %>% 
  glimpse()
 

prey_taxa_to_add <- read_csv("prey_taxa_to_add.csv") %>% select(-citation, -n) %>% distinct(prey_taxon, .keep_all = T)

test_fish <- data_fish %>% 
  left_join(prey_taxa_to_add) %>% 
  mutate(prey_kingdom = case_when(is.na(prey_kingdom) ~ prey_kingdom_add,
                                  TRUE ~ prey_kingdom),
         prey_class = case_when(is.na(prey_class) ~ prey_class_add,
                                TRUE ~ prey_class),
         prey_order = case_when(is.na(prey_order) ~ prey_order_add,
                                TRUE ~ prey_order),
         prey_family = case_when(is.na(prey_family) ~ prey_family_add,
                                 TRUE ~ prey_family),
         prey_phylum = case_when(is.na(prey_phylum) ~ prey_phylum_add,
                                 TRUE ~ prey_phylum),
         prey_species = case_when(is.na(prey_species) ~ prey_species_add,
                                   TRUE ~ prey_species),
         prey_subphylum = case_when(is.na(prey_subphylum) ~ prey_subphylum_add,
                                  TRUE ~ prey_subphylum),
         prey_superclass = case_when(is.na(prey_superclass) ~ prey_superclass_add,
                                  TRUE ~ prey_superclass),
         prey_subclass = case_when(is.na(prey_subclass) ~ prey_subclass_add,
                                     TRUE ~ prey_subclass),
         prey_type = case_when(is.na(prey_type) ~ prey_type_add,
                               TRUE ~ prey_type)) %>% 
  select(-contains("_add"))

data_fish %>% group_by(prey_kingdom) %>% tally() %>% arrange(-n)
test_fish %>% group_by(prey_kingdom) %>% tally() %>% arrange(-n)

# 
data_fish <- test_fish
saveRDS(data_fish, file = "database/data_fish.rds")



# find



