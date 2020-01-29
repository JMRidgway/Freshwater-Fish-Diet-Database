
# Update and add prey taxa ------------------------------------------------

# load temp data_fish
data_fish_temp <- data_fish
fish_we_have <- read.csv(text = getURL("https://raw.githubusercontent.com/JMRidgway/Freshwater-Fish-Diet-Database/master/fish_taxa_all.csv"))



#lists of prey taxa as lower and sentence case
prey_lowercase <- prey_taxa_all %>% as_tibble()
prey_sentencecase <- prey_taxa_all  %>% 
  mutate(prey_taxon = str_to_sentence(prey_taxon)) 

#merge lower and sentence case information for prey taxa
prey_taxa_all <- prey_lowercase %>% 
  bind_rows(prey_sentencecase) %>% 
  as_tibble() %>% 
  filter(!is.na(prey_kingdom)) %>% 
  rename_all(paste0, "add") %>% 
  rename(prey_taxon = prey_taxonadd) %>% 
  distinct(prey_taxon, .keep_all = T) %>% 
  mutate_all(funs('as.character'))




fish_lowercase <- fish_we_have %>% 
  mutate(type_of_fish = str_to_lower(type_of_fish)) %>% as_tibble()
fish_sentencecase <- fish_we_have  %>% 
  mutate(type_of_fish = str_to_sentence(type_of_fish)) 

#merge lower and sentence case information for fish taxa
fish_taxa_all <- fish_lowercase %>% 
  bind_rows(fish_sentencecase) %>% 
  bind_rows(fish_we_have) %>% 
  as_tibble() %>% 
  filter(!is.na(fish_order)) %>% 
  rename_all(paste0, "add") %>% 
  rename(type_of_fish = type_of_fishadd) %>% 
  distinct(type_of_fish, .keep_all = T) %>% 
  mutate_all(funs('as.character'))


#add prey taxon data and replace nas with updated list, otherwise
#keep all else the sam
data_fish <- data_fish_temp %>% 
  left_join(prey_taxa_all) %>% 
  left_join(fish_taxa_all) %>% 
  mutate(prey_kingdom = case_when(is.na(prey_kingdom) ~ prey_kingdomadd,
                                  TRUE ~ prey_kingdom),
         prey_class = case_when(is.na(prey_class) ~ prey_classadd,
                                TRUE ~ prey_class),
         prey_order = case_when(is.na(prey_order) ~ prey_orderadd,
                                TRUE ~ prey_order),
         prey_family = case_when(is.na(prey_family) ~ prey_familyadd,
                                 TRUE ~ prey_family),
         prey_species = case_when(is.na(prey_species) ~ prey_speciesadd,
                                  TRUE ~ prey_species),
         fish_order = case_when(is.na(fish_order) ~ fish_orderadd,
                                TRUE ~ fish_order),
         fish_family = case_when(is.na(fish_family) ~ fish_familyadd,
                                 TRUE ~ fish_family),
         fish_species = case_when(is.na(fish_species) ~ fish_speciesadd,
                                  TRUE ~ fish_species)) %>% 
  select(-prey_kingdomadd, -prey_classadd, -prey_orderadd,
         -prey_familyadd, -prey_speciesadd,
         -fish_orderadd, -fish_speciesadd, -fish_familyadd)


#compare before and after
data_fish_temp %>% group_by(fish_order) %>% tally() %>% arrange(-n)
data_fish %>% group_by(fish_order) %>% tally() %>% arrange(-n)


data_fish_temp %>% group_by(prey_kingdom) %>% tally() %>% arrange(-n)
data_fish %>% group_by(prey_kingdom) %>% tally() %>% arrange(-n)

saveRDS(data_fish, file = "data_fish.rds")
write.csv(fish_taxa_all, file = "fish_taxa_all.csv")
write.csv(prey_taxa_all, file = "prey_taxa_all.csv")
