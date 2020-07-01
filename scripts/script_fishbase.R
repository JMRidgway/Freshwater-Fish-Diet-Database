library(rfishbase)
library(tidyverse)
library(taxize)

#fish taxon names in database
fish_taxa_all <- data_fish %>% select(type_of_fish, fish_order, fish_family, fish_species) %>% 
  distinct(type_of_fish, .keep_all = TRUE) %>%
  mutate(type_fishmatch = str_replace(type_of_fish, "_", " ")) 

#use fishbase to find synonyms
fish_syn <- synonyms(fish_taxa_all$type_fishmatch)
#load fishbase taxon info for all fish
fb_taxa <- load_taxa("FishBase")

#add taxon info
fish_taxa_toadd <- fish_syn %>% 
  filter(Status == "synonym" | Status == "accepted name") %>% 
  select(synonym, Species) %>% 
  rename(type_fishmatch = synonym) %>% 
  right_join(fish_taxa_all) %>% 
  mutate(fish_species = case_when(is.na(fish_species) ~ Species, TRUE ~ fish_species)) %>%
  tibble() %>% 
  left_join(fb_taxa) %>%
  mutate(fish_family = case_when(is.na(fish_family) ~ Family, TRUE ~ fish_family),
         fish_order = case_when(is.na(fish_order) ~ Order, TRUE ~ fish_order)) %>% 
    rename(fish_class = Class,
         fish_superclass = SuperClass) %>% 
  select(contains("fish")) %>% 
  tibble() %>% 
  mutate(type_lower = tolower(type_of_fish))


# Taxize remaining missing names ------------------------------------------

#taxa still needed
fish_taxa_needed <- fish_taxa_toadd %>% filter(is.na(fish_species)) 

#taxa still needed - unique names to taxize
fish_taxa_needed_unique <- fish_taxa_needed  %>% 
  select(type_lower) %>% 
  ungroup() %>% 
  distinct(type_lower)

#taxize
fish_classify <- classification(fish_taxa_needed_unique$type_lower, db = "ncbi", 
               return_id = T)

#reshape to merge with original
fish_taxized <- fish_classify %>% rbind() %>% 
  filter(rank != "no rank") %>%
  tibble() %>% 
  select(-id) %>% 
  pivot_wider(names_from = rank,
              values_from = name) %>% 
  select(query,class, order, family, species) %>% 
  setNames(paste0('taxized_', names(.))) %>% 
  rename(type_lower = taxized_query)



# Add taxized fish to full list -------------------------------------------

fish_taxized2 <- fish_taxa_toadd %>% left_join(fish_taxized) %>% 
  mutate(fish_order = case_when(is.na(fish_order) ~ taxized_order, TRUE ~ fish_order),
         fish_class = case_when(is.na(fish_class) ~ taxized_class, TRUE ~ fish_class),
         fish_species = case_when(is.na(fish_species) ~ taxized_species, TRUE ~ fish_species),
         fish_family = case_when(is.na(fish_family) ~ taxized_species, TRUE ~ fish_family)) %>% 
  select(contains("fish"))


#still needed 
add_by_hand <- fish_taxized2 %>% filter(is.na(fish_species))
write.csv(add_by_hand, file = "add_by_hand.csv", row.names = F)
added_by_hand <- read_csv(file = "add_by_hand.csv")

fish_done <- fish_taxized2 %>% 
  filter(!is.na(fish_species)) %>% 
  bind_rows(added_by_hand)


#add to full data base

# data_fish <- data_fish %>% select(-fish_family, -fish_species, -fish_genus_species, -fish_order) %>% 
#   left_join(fish_done, by = "type_of_fish")
# 
# 
# test %>% select(type_of_fish, fish_order, fish_family, fish_species) %>% 
#   distinct(type_of_fish, .keep_all = TRUE) %>%
#   mutate(type_fishmatch = str_replace(type_of_fish, "_", " ")) %>% View()


