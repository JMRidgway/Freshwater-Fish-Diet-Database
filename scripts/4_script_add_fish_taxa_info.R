library(rfishbase)
library(tidyverse)
library(taxize)

#fish taxon info needed
fish_taxa_all <- data_fish %>% filter(is.na(fish_family)) %>% distinct(type_of_fish) %>% 
  mutate(type_fishmatch = str_replace(type_of_fish, "_", " ")) 

#use fishbase to find synonyms
fish_syn <- rfishbase::synonyms(fish_taxa_all$type_fishmatch, server = "fishbase")
#load fishbase taxon info for all fish
fb_taxa <- load_taxa("FishBase")

#add taxon info
fish_taxa_toadd <- fish_syn %>% 
  filter(Status == "synonym" | Status == "accepted name") %>% 
  select(synonym, Species) %>% 
  rename(type_fishmatch = synonym) %>% 
  right_join(fish_taxa_all) %>% 
  mutate(fish_species = Species) %>%
  tibble() %>% 
  left_join(fb_taxa) %>%
  rename(fish_family =  Family,
         fish_order = Order) %>% 
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
# added_by_hand <- read_csv(file = "add_by_hand.csv")

fish_done <- fish_taxized2 %>% 
  filter(!is.na(fish_species)) %>% 
  bind_rows(added_by_hand) %>% 
  rename(fish_family_add = fish_family,
         fish_superclass_add = fish_superclass,
         fish_class_add = fish_class,
         fish_order_add = fish_order,
         fish_species_add = fish_species)


#add to full data base

test <- data_fish %>% left_join(fish_done) %>% 
  mutate(fish_class = case_when(is.na(fish_class) ~ fish_class_add, TRUE ~ fish_class),
         fish_family = case_when(is.na(fish_family) ~ fish_family_add, TRUE ~ fish_family),
         fish_order = case_when(is.na(fish_order) ~ fish_order_add, TRUE ~ fish_order),
         fish_superclass = case_when(is.na(fish_superclass) ~ fish_superclass_add, TRUE ~ fish_superclass),
         fish_species = case_when(is.na(fish_species) ~ fish_species_add, TRUE ~ fish_species)) %>% 
  select(-fish_class_add, -fish_species_add, -fish_family_add, -fish_superclass_add, -fish_order_add, -type_fishmatch)

# data_fish <- test
