
data_fish_prey_taxa

prey_taxon <- data_fish_refined %>% distinct(prey_taxon)

prey_taxon_gnr <- gnr_resolve(names = prey_taxon$prey_taxon, best_match_only = T)

prey_taxon_gnr_alreadyhave <- prey_taxon_gnr %>% 
  rename(prey_taxon = user_supplied_name) %>% 
  left_join(data_fish_prey_taxa) %>% 
  select(prey_taxon, kingdom_add, family_add,
                                            class_add, order_add, species_add)


prey_taxa_names <- prey_taxon_gnr_alreadyhave %>%
  filter(is.na(kingdom_add)) %>%
  select(prey_taxon) %>%
  rename(user_supplied_name = prey_taxon) %>%
  left_join(prey_taxon_gnr) 


prey_taxa_names_distinct <- prey_taxa_names %>% 
  distinct(matched_name)


first_1_49 <- prey_taxa_names_distinct %>% slice(1:49)

prey_taxa_taxized_1 <- classification(first_1_49$matched_name,
                                    db = "ncbi", return_id = T)

first_50_99 <- prey_taxa_names_distinct %>% slice(50:99)

prey_taxa_taxized_2 <- classification(first_50_99$matched_name,
                                      db = "ncbi", return_id = T)


first_100_149 <- prey_taxa_names_distinct %>% slice(100:149)
prey_taxa_taxized_4 <- classification(first_100_149$matched_name,
                                      db = "ncbi", return_id = T)

first_150_198 <- prey_taxa_names_distinct %>% slice(150:198)
prey_taxa_taxized_5 <- classification(first_150_198$matched_name,
                                      db = "ncbi", return_id = T)


first_199_235 <- prey_taxa_names_distinct %>% slice(199:235)
prey_taxa_taxized_6 <- classification(first_199_235$matched_name,
                                      db = "ncbi", return_id = T)

first_236_272 <- prey_taxa_names_distinct %>% slice(236:272)
prey_taxa_taxized_7 <- classification(first_236_272$matched_name,
                                      db = "ncbi", return_id = T)


prey_taxon_add <- rbind(prey_taxa_taxized_1,
              prey_taxa_taxized_2,
              prey_taxa_taxized_3,
              prey_taxa_taxized_4,
              prey_taxa_taxized_5,
              prey_taxa_taxized_6,
              prey_taxa_taxized_7) %>% 
  as_tibble() %>% 
  distinct() %>% 
  filter(rank != "no rank") %>% 
  select(-id) %>% 
  pivot_wider(names_from = rank,
              values_from = name) %>% 
  select(query, kingdom, class, order, family, species) %>% 
  rename(matched_name = query,
         kingdom_add = kingdom,
         class_add = class,
         order_add = order,
         family_add = family, 
         species_add = species) %>% 
  right_join(prey_taxa_names) %>% 
  rename(prey_taxon = user_supplied_name) %>% 
  select(prey_taxon, kingdom_add, class_add, order_add, family_add, species_add)


prey_taxa_all <- bind_rows(prey_taxon_add, prey_taxon_gnr_alreadyhave) %>% 
  rename(prey_kingdom = kingdom_add,
         prey_class = class_add,
         prey_order = order_add,
         prey_family = family_add,
         prey_species = species_add) %>% 
  distinct(prey_taxon, .keep_all = T)
  
write.csv(prey_taxa_all, file = "prey_taxa_all.csv")

data_fish_refined_taxizedprey <- data_fish_refined %>% select(-prey_kingdom,
                                                              -prey_class,
                                                              -prey_family,
                                                              -prey_species) %>% 
  left_join(prey_taxa_all)



# Taxize fish -------------------------------------------------------------

fish_taxa <- data_fish_refined_taxizedprey %>% 
  mutate(type_of_fish = case_when(type_of_fish == "NA" ~ fish_genus_species,
                                  TRUE ~ type_of_fish)) %>% 
    distinct(type_of_fish)


fish_taxa_gnr <- gnr_resolve(names = fish_taxa$type_of_fish, best_match_only = T)

fish_taxa_names_distinct <- fish_taxa_gnr %>% distinct(matched_name)



first_fish__1_40 <- fish_taxa_names_distinct %>% slice(1:40)

fish_taxa_taxized_1 <- classification(first_fish__1_40$matched_name,
                                      db = "gbif", return_id = T)

first_fish__41_99 <- fish_taxa_names_distinct %>% slice(41:99)

fish_taxa_taxized_2 <- classification(first_fish__41_99$matched_name,
                                      db = "gbif", return_id = T)


first_fish__100_149 <- fish_taxa_names_distinct %>% slice(100:149)
fish_taxa_taxized_4 <- classification(first_fish__100_149$matched_name,
                                      db = "gbif", return_id = T)

first_fish__150_176 <- fish_taxa_names_distinct %>% slice(150:176)
fish_taxa_taxized_5 <- classification(first_fish__150_176$matched_name,
                                      db = "gbif", return_id = T)

fish_taxon_add <- rbind(fish_taxa_taxized_1,
                        fish_taxa_taxized_2,
                        fish_taxa_taxized_4,
                        fish_taxa_taxized_5) %>% 
  as_tibble() %>% 
  distinct() %>% 
  filter(rank != "no rank") %>% 
  select(-id) %>% 
  pivot_wider(names_from = rank,
              values_from = name) %>% 
  select(query, order, family, species) %>% 
  rename(matched_name = query,
         fish_order = order,
         fish_family = family, 
         fish_species = species) %>% 
  right_join(fish_taxa_gnr) %>% 
  rename(type_of_fish = user_supplied_name) %>% 
  select(type_of_fish, fish_order, fish_family, fish_species) %>% 
  mutate(fish_order = case_when(type_of_fish == "black crappie" ~ "Perciformes",
                                type_of_fish == "blue catfish" ~ "Siluriformes",
                                TRUE ~ fish_order),
         fish_family = case_when(type_of_fish == "black crappie" ~ "Centrachidae",
                                 type_of_fish == "blue catfish" ~ "Ictaluridae",
                                 TRUE ~ fish_family),
         fish_species = case_when(type_of_fish == "black crappie" ~ "Pomoxis nigromaculatus",
                                  type_of_fish == "blue catfish" ~ "Ictalurus furcatus",
                                  is.na(fish_species) ~ type_of_fish,
                                  TRUE ~ fish_species))
write.csv(fish_taxon_add, file = "fish_taxon_add.csv")


data_fish_refined_taxized <- data_fish_refined_taxizedprey %>% 
  select(-fish_family, -fish_genus, -fish_order) %>% 
  left_join(fish_taxon_add) %>% 
  mutate(new_fish_id = as.numeric(as.factor(paste(predator_min_length, predator_max_length,sample_size, start_date, end_date,
                               author, year, journal, table_figure,
                               fish_species, fish_genus_species, type_of_fish))))

saveRDS(data_fish_refined_taxized, file  = "database/data_fish_refined_taxized.rds")
