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

# saveRDS(data_fish_updated2, file = "database/data_fish.rds")

# data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
#   mutate_all(funs('as.character')) %>% 
#   remove_empty("rows") 

#check if percent measures do not sum to 100
data_fish %>% 
  distinct() %>% 
  unite(author_year_tbl, c("author", "year", "table_figure"),sep = "_", remove = F) %>% 
  distinct() %>% 
  filter(grepl("proportion", measurement_typeunits)) %>% 
  mutate(fish_id_temp = as.numeric(as.factor(fish_id_temp))) %>% 
  group_by(fish_id_temp,  citation, author_year_tbl, sample_size, start_date) %>% 
  summarize(total = sum(as.numeric(measure_numeric))) %>% 
  filter(total != 100) %>% 
  ungroup() %>% 
  distinct(fish_id_temp,  total, citation, author_year_tbl, sample_size, start_date) %>% 
  arrange(-total) %>% View()

# temp <-data_fish %>%  mutate(start_date = case_when(author == "Idodo_Umeh" ~ "3/1/1982",
#                                       TRUE ~ start_date),
#                end_date = case_when(author == "Idodo_Umeh" ~ "2/1/1984",
#                                     TRUE ~ start_date),
#                fish_id_temp = paste0(author,"_", year,"_",table_figure,"_", sample_size, "_",measurement_units, measurement_type,
#                                      "_", site_name,"_",
#                                      predator_min_length, "_", predator_max_length, "_", sample_id,
#                                      "_", start_date, "_", end_date, habitat, 
#                                      microhabitat),
#                fish_id_old = fish_id,
#                fish_id = as.numeric(as.factor(fish_id_temp)),
#                fish_id = case_when(fish_id == 886 ~ 887,
#                                    author == "Elakhame" & sample_size == "25" ~ 465,
#                                    TRUE ~ fish_id))

data_fish_updated <- data_fish %>% 
  filter(sample_size != "0", 
         author != "Yalcin",
         author != "Kliemann et al", #need to re-extract
         author != "Goodson", #need to re-extract
         author != "Edds et al.", #delete and do not use. Numbers do not add to 100%
         author != "Moffet&Hunt", #delete and do not use. Numbers do not add to 100%
         author != "Huish", #need to re-extract
         author != "Cable", #need to re-extract
         author_year_tbl != "Darnell_1958_tbl3", #duplicate
         !grepl("George_1979", author_year_tbl), #need to re-extract
         author_year_tbl != "Mathur_1977_tbl3", #need to re-extract
         author_year_tbl != "Adite_1997_table_3", #need to re-extract
         author_year_tbl != "Adite_1997_table_4", #need to re-extract
         author_year_tbl != "Sanchez-Hernandez_2012_tbl2", #need to re-extract
         author_year_tbl != "Keast_1978_tbl1", #need to re-extract. Numbers below 100, but later odd as well.
         author_year_tbl != "Rice_1942_tbl1", #need to re-extract
         author_year_tbl != "Miller_2015_NA", #need to re-extract
         author_year_tbl != "Aranha_1998_tbl3", #delete and re-enter. more than 100
         author_year_tbl != "Adalsteinsson_1979_table 2", #delete and do not use. Numbers do not add to 100% in original
         !grepl("Greger&Deacon_1980", author_year_tbl), #delete and re-extract. more than 100
         !grepl("Rom<e1>n-V", author), # numbers not reported for all prey taxa - do not use
         !grepl("Whitaker Jr, J. O.", citation)) %>% #need to re-extract
  bind_rows(add_other_to_100) %>%
  filter(fish_id != 782 | author != "Crawford", #totals did not add to 100 in original
         fish_id != 809 | author != "Crawford", #totals did not add to 100 in original
         fish_id != 286 | author != "Dorgeloh", #totals did not add to 100 in original
         fish_id != 284 | author != "Dorgeloh", #totals did not add to 100 in original
         author_year_tbl != "Alexander_2013_tbl1", #totals did not add to 100 in original
         author_year_tbl != "Dorgeloh_1994_NA", #totals did not add to 100 in original
         author_year_tbl != "Arawomo_1976_tbl2") #totals did not add to 100 in original 







#add Other prey category for papers that included only "major" prey items so that total == 100
#first create a column with total percentages for each fish_id
totals <- data_fish_updated %>% 
  distinct() %>% 
  filter(grepl("percent", measurement_typeunits)) %>% 
  group_by(fish_id_temp, table_figure, citation, author, year, sample_size, start_date) %>% 
  summarize(total = sum(as.numeric(measure_numeric))) %>% 
  filter(total < 100) %>% 
  ungroup() %>% 
  distinct(citation, author, year, table_figure, total, fish_id_temp, sample_size, start_date) %>% 
  arrange(total) %>% 
  select(fish_id_temp, total)


add_other_to_100 <- data_fish_updated %>% 
  select(-total) %>% 
  unite(author_year_table, c("author", "year", "table_figure"),sep = "_", remove = F) %>%
  filter(grepl("percent", measurement_typeunits)) %>% 
  filter(grepl("Angermeier_1985", author_year_tbl)) %>% #table caption says all "blanK" cells were <1% of totals. Added Other column, though technically some of the zeros (i.e. blanks) could be non-zero.
           # author_year_table == "Keast_1978_tbl1" |
  #          author_year_table == "Sibbing & Nabelkirk_2000_tbl 5"|) %>% 
           # fish_id == 586| #Surber 1941
           # fish_id == 587| #Surber 1941
           # fish_id == 589| #Surber 1941
           # fish_id == 590| #Surber 1941
           # fish_id == 591| #Surber 1941
           # fish_id == 593| #Surber 1941
           # fish_id == 592)  %>% #Surber 1941
  left_join(totals, by = "fish_id_temp") %>% 
  distinct(fish_id, .keep_all = T) %>% 
  mutate(prey_taxon = "Other",
         measurement = as.character(100-as.numeric(total)),
         measure_numeric = as.character(100-as.numeric(total)),
         prey_kingdom = NA,
         prey_class = NA,
         prey_family = NA,
         prey_species = NA,
         prey_stage = NA)


to_delete <- data_fish %>% filter(author == "Iyabo")

data_fish_culled <- data_fish %>% 
  anti_join(to_delete) %>% 
  mutate_all(as.character)

to_add <- data_fish %>% filter(author == "Iyabo") %>% 
  filter()

data_fish_updated <- bind_rows(data_fish_culled, to_add)


#check that proportions sum to 1 and percentages sum to 100
# data_fish_culled_updated2 %>% 
#   filter(grepl("percent", measurement_typeunits)) %>% 
#   group_by(fish_id, table_figure, citation) %>% 
#   summarize(total = sum(as.numeric(measure_numeric))) %>% 
#   filter(total >= 150) %>% 
#   ungroup() %>% 
#   distinct(citation, table_figure) %>% 
#   print(n = 50)



write.csv(more_than_100_percent, file = "more_than_100_percent.csv", row.names = F)


test <- data_fish_updated2 %>% distinct

test %>% 
  filter(grepl("percent", measurement_typeunits)) %>% 
  group_by(fish_id, table_figure, citation) %>% 
  summarize(total = sum(as.numeric(measure_numeric))) %>% 
  filter(total >= 150) %>% 
  ungroup() %>% 
  distinct(citation, table_figure) %>% 
  print(n = 50) 

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
# 
# data_fish <- data_fish %>% left_join(prey_add) %>%
#   mutate(prey_kingdom = case_when(is.na(prey_kingdom) ~ prey_kingdom_add,
#                                   TRUE ~ prey_kingdom),
#          prey_class = case_when(is.na(prey_class) ~ prey_class_add,
#                                 TRUE ~ prey_class),
#          prey_order = case_when(is.na(prey_order) ~ prey_order_add,
#                                 TRUE ~ prey_order),
#          prey_family = case_when(is.na(prey_family) ~ prey_family_add,
#                                  TRUE ~ prey_family),
#          prey_species = case_when(is.na(prey_species) ~ prey_species_add,
#                                   TRUE ~ prey_species)) %>%
#   select(-prey_kingdom_add, -prey_class_add,
#          -prey_order_add, -prey_family_add, -prey_species_add)
# 
# data_fish %>% filter(is.na(prey_kingdom)) %>% nrow()
# data_fish_test %>% filter(is.na(prey_kingdom)) %>% nrow()
# 
# 
# saveRDS(data_fish, file = "database/data_fish.rds")
