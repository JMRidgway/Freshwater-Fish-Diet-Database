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

data_fish %>% group_by(prey_class) %>% tally() %>% arrange(-n)
test %>% group_by(prey_class) %>% tally() %>% arrange(-n)

# 
data_fish <- test
saveRDS(data_fish, file = "database/data_fish.rds")

test <- data_fish
# fill in missing taxonomic info for prey.
test %>% 
  filter(is.na(prey_class)) %>% select(contains("prey_")) %>% View()


need <- test %>% 
  select(contains("prey_"), author, year, table_figure) %>%
  filter(is.na(prey_family) & !is.na(prey_order)) %>%
  distinct(prey_order)

get_fam <- classification(need$prey_order, db = "gbif", return_id = T)
got_fam <- rbind(get_fam) %>% distinct(name, rank, query) %>% 
  as_tibble() %>% filter(rank == "order") %>% 
  rename(prey_order_add = name,
         prey_order = query) %>% 
  mutate(prey_order_match = prey_order) %>% 
  select(-rank)




test <- data_fish %>% 
  mutate(prey_phylum = str_replace(prey_phylum, "Byrozoa", "Bryozoa"),
         prey_phylum = str_replace(prey_phylum, "Charaphyta", "Charophyta"),
         prey_subphylum = str_replace(prey_subphylum, "Crustaceae", "Crustacea"),
         prey_class = case_when(prey_subphylum == "Actinopterygii" ~ "Actinopterygii", TRUE ~ prey_class),
         prey_order = case_when(prey_class == "Psocoptera" ~ "Psocoptera", TRUE ~ prey_order),
         prey_class = case_when(prey_order == "Psocoptera" ~ "Insecta", TRUE ~ prey_class),
         prey_subphylum = str_replace(prey_subphylum, "Actinopterygii", NA_character_),
         prey_class = str_replace(prey_class, "unknown", NA_character_),
         prey_kingdom = str_replace(prey_kingdom, "unknown", NA_character_),
         prey_phylum = str_replace(prey_phylum, "unknown", NA_character_),
         prey_order = str_replace(prey_order, "unknown", NA_character_),
         prey_family = str_replace(prey_family, "unknown", NA_character_),
         prey_species = str_replace(prey_species, "unknown", NA_character_),
         prey_superclass = str_replace(prey_superclass, "unknown", NA_character_),
         prey_subclass = str_replace(prey_subclass, "unknown", NA_character_)) %>% 
  left_join(got_fam) %>% 
  mutate(prey_order = case_when(prey_order == prey_order_match ~ prey_order_add,
                                 TRUE ~ prey_order)) %>% 
  select(-prey_order_add, -prey_order_match) %>% 
  mutate(prey_species = case_when(prey_order == "beraeoptera rorta" ~ "Beraeoptera roria", 
                                  prey_order == "brycinus lateralis" ~ "Brycinus lateralis",
                                  TRUE ~ prey_species),
         prey_family = case_when(prey_order == "beraeoptera rorta" ~ "Conoesucidae", 
                                 prey_order == "brycinus lateralis" ~ "Alestidae",
                                 TRUE ~ prey_family),
         prey_order = case_when(grepl("coleoptera", prey_order) ~ "Coleoptera",
                                grepl("diptera", prey_order) ~ "Diptera",
                                grepl("Dipter", prey_order) ~ "Diptera",
                                prey_order == "dintera" ~ "Diptera",
                                grepl("hoinoptera", prey_order) ~ "Homoptera",
                                grepl("homoptera", prey_order) ~ "Homoptera",
                                grepl("thhysamoptera", prey_order) ~ "Thysanoptera",
                                grepl("thysanopter", prey_order) ~ "Thysanoptera",
                                grepl("trichopter", prey_order) ~ "Trichoptera",
                                grepl("tricopter", prey_order) ~ "Trichoptera",
                                grepl("Coleoptera", prey_order) ~ "Coleoptera",
                                grepl("hymenopter", prey_order) ~ "Hymenoptera",
                                grepl("orthopter", prey_order) ~ "Orthoptera",
                                grepl("lepidopter", prey_order) ~ "Lepidoptera",
                                grepl("anisoptera", prey_order) ~ "Odonata",
                                grepl("zygoptera", prey_order) ~ "Odonata",
                                grepl("zigoptera", prey_order) ~ "Odonata",
                                grepl("hemiptera", prey_order) ~ "Hemiptera",
                                grepl("beraeoptera", prey_order) ~ "Trichoptera",
                                grepl("brycinus lateralis", prey_order) ~ "Characiformes",
                                grepl("plecoptera", prey_order) ~ "Plecoptera",
                                grepl("megaloptera", prey_order) ~ "Megaloptera",
                                grepl("ttichoptera", prey_order) ~ "Trichoptera",
                                grepl("liemiptera", prey_order) ~ "Hemiptera",
                                grepl("epemeropter", prey_order) ~ "Ephemeroptera",
                                grepl("ephemeroptera", prey_order) ~ "Ephemeroptera",
                                grepl("Cyclopodia", prey_order) ~ "Cyclopoida",
                                grepl("Triehoptera", prey_order) ~ "Trichoptera",
                                TRUE ~ prey_order),
         prey_order = str_to_sentence(prey_order))
  

test %>% distinct(prey_order, prey_family) %>% View()

add_phylum <- test %>%
  left_join(got_fam) %>% 
  mutate(prey_phylum = case_when(is.na(prey_phylum) ~ prey_phylum_add,
                                 TRUE ~ prey_phylum)) %>% 
  select(-prey_phylum_add)


test <- add_phylum




data_fish %>% distinct(prey_subphylum) %>% View()

