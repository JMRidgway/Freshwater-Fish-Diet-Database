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


data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
  mutate_all(funs('as.character')) %>% 
  remove_empty("rows") 
  # mutate(prey_taxon_cleaned = case_when(grepl("dipter", prey_taxon) ~ "Diptera",
  #                                       grepl("Dipter", prey_taxon) ~ "Diptera",
  #                                       grepl("richopter", prey_taxon) ~ "Trichoptera",
  #                                       grepl("insect", prey_taxon) ~ "Insecta",
  #                                       grepl("Insect", prey_taxon) ~ "Insecta",
  #                                       grepl("algae", prey_taxon) ~ "Plantae",
  #                                       grepl("Algae", prey_taxon) ~ "Plantae",
  #                                       grepl("Invertebrate", prey_taxon) ~ "Arthropods",
  #                                       grepl("invertebrate", prey_taxon) ~ "Arthropods",
  #                                       grepl("seed", prey_taxon) ~ "Plantae",
  #                                       grepl("Seed", prey_taxon) ~ "Plantae",
  #                                       grepl("rustacea", prey_taxon) ~ "Crustacean",
  #                                       grepl("hironomid", prey_taxon) ~ "Chironomidae",
  #                                       grepl("plant ", prey_taxon) ~ "Plantae",
  #                                       grepl("Plant ", prey_taxon) ~ "Plantae",
  #                                       grepl("plant_", prey_taxon) ~ "Plantae",
  #                                       grepl("hironomid", prey_taxon) ~ "Chironomidae",
  #                                       grepl("fish", prey_taxon) ~ "Actinopterygii",
  #                                       grepl("Fish", prey_taxon) ~ "Actinopterygii"))
  # 

data_fish %>% group_by(author, year) %>% tally() %>% arrange(-n)

#plot fish families
fish_families <- data_fish %>% 
  unite(author_year, c("author", "year"), remove = F) %>% 
  group_by(author_year) %>% 
  distinct(fish_family) %>% 
  group_by(fish_family) %>% 
  tally() %>% arrange(-n)

ggplot(fish_families, aes(x = reorder(fish_family,n), y = n)) +
  geom_point() + 
  coord_flip()


prey_taxa_to_clean <- data_fish %>% filter(is.na(prey_kingdom)) %>% 
  distinct(prey_taxon, .keep_all = T) %>% 
  select(prey_taxon, prey_taxon_cleaned) %>% 
  filter(is.na(prey_taxon_cleaned))

write.csv(prey_taxa_to_clean, file = "prey_taxa_to_clean.csv", row.names = F)

prey_list <- prey_taxa_to_clean %>% 
  distinct(prey_to_clean)


prey_list_taxize <- classification(prey_list$prey_to_clean, db = "ncbi",
                                   return_id = T)

prey_list_temp <- rbind(prey_list_taxize) %>% 
  filter(rank != "no rank") %>% 
  select(-id) %>% 
  pivot_wider(names_from = rank,
              values_from = name) %>% 
  select(query, kingdom, class, order, family, species) %>% 
  rename(prey_taxon = query,
         prey_kingdom_add = kingdom,
         prey_class_add = class,
         prey_order_add = order,
         prey_family_add = family, 
         prey_species_add = species)

add <- data_fish %>% left_join(prey_list_temp) %>% 
  mutate(prey_kingdom = case_when(is.na(prey_kingdom) ~ prey_kingdom_add,
                                  TRUE ~ prey_kingdom),
         prey_class = case_when(is.na(prey_class) ~ prey_class_add,
                                TRUE ~ prey_class),
         prey_order = case_when(is.na(prey_order) ~ prey_order_add,
                                TRUE ~ prey_order),
         prey_family = case_when(is.na(prey_family) ~ prey_family_add,
                                 TRUE ~ prey_family),
         prey_species = case_when(is.na(prey_species) ~ prey_species_add,
                                  TRUE ~ prey_species)) %>% 
  select(-contains("_add"))

data_fish %>% filter(is.na(prey_kingdom)) %>% tally()
add %>% filter(is.na(prey_kingdom)) %>% tally()

saveRDS(data_fish, file = "database/data_fish.rds")

prey_taxa_left_refined <- read_csv("prey_taxa_left_refined.csv")

prey_taxa_left_refined %>% filter(grepl("insect", prey_taxon)) %>% View()



classification("cladocera", db = "ncbi")





prey_taxa_all <- data_fish %>% filter(!is.na(prey_kingdom)) %>% 
  select(prey_taxon, prey_kingdom, prey_family, prey_order, prey_class, prey_species) %>% 
  distinct(prey_taxon, .keep_all = T)

prey_taxa_to_clean <- read_csv("prey_taxa_to_clean.csv")

prey_taxa_added <- prey_taxa_to_clean %>% select(search_term) %>% 
  rename(prey_taxon = search_term) %>% 
  left_join(prey_taxa_all) %>% 
  filter(!is.na(prey_kingdom)) %>%
  mutate(ending = str_sub(prey_taxon, -3)) %>% 
  mutate(prey_family = case_when(ending == "dae" ~ prey_taxon,
                                 TRUE ~ prey_family),
         prey_order = case_when(ending == "era" ~ prey_taxon,
                                TRUE ~ prey_order)) %>%
  select(-ending) %>% 
  rename(search_term = prey_taxon)

prey_add <- prey_taxa_to_clean %>% select(prey_taxon, search_term) %>% 
  left_join(prey_taxa_added) %>% distinct(prey_taxon, .keep_all = T) %>% 
  rename(prey_kingdom_add = prey_kingdom,
prey_class_add = prey_class,
prey_order_add = prey_order,
prey_family_add = prey_family, 
prey_species_add = prey_species)

data_fish <- data_fish %>% left_join(prey_add) %>%
  mutate(prey_kingdom = case_when(is.na(prey_kingdom) ~ prey_kingdom_add,
                                  TRUE ~ prey_kingdom),
         prey_class = case_when(is.na(prey_class) ~ prey_class_add,
                                TRUE ~ prey_class),
         prey_order = case_when(is.na(prey_order) ~ prey_order_add,
                                TRUE ~ prey_order),
         prey_family = case_when(is.na(prey_family) ~ prey_family_add,
                                 TRUE ~ prey_family),
         prey_species = case_when(is.na(prey_species) ~ prey_species_add,
                                  TRUE ~ prey_species)) %>%
  select(-prey_kingdom_add, -prey_class_add,
         -prey_order_add, -prey_family_add, -prey_species_add)




saveRDS(data_fish, file = "database/data_fish.rds")
