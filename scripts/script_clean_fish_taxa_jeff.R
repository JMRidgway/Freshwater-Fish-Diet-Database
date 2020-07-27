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
library(ggrepel)

# Fixed error with missing fish by re-binding  --------
# separated data_fish into data with and without

#load master data_frame
data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
  mutate_all(funs('as.character')) %>% 
  remove_empty("rows")


#save a backup
write.csv(data_fish,file = paste0("database/data_backups/data_fish", Sys.Date(),".csv"),row.names = F)

#make a list of citations that had no data for type_of_fish
missing_fish_citations <- data_fish %>% filter(is.na(type_of_fish)) %>% distinct(citation) %>% mutate(citation_to_remove = citation)

#add new column identifying rows with missing type_of_fish
data_fish_test <- data_fish %>% full_join(missing_fish_citations)

#make dataframe that includes only rows that have type_of_fish
data_fish_have <- data_fish_test %>% mutate_all(funs('as.character')) %>% 
  filter(is.na(citation_to_remove))

family_diversity <- read_csv("families.csv")

#make dataframe that has number of records per fish family - including NA when family is missing
fams <- data_fish %>% 
  distinct(fish_species, fish_family) %>% 
  group_by(fish_family) %>% 
  tally() %>% 
  arrange(-n) %>% 
  left_join(family_diversity)


fams %>% 
  ggplot(aes(x = reorder(fish_family, n), y = n)) +
  geom_bar(stat = "identity") + 
  coord_flip()


fams %>% filter(!is.na(fish_family)) %>% 
  mutate(x1 = species_no/max(species_no, na.rm = T),
         y1 = n/max(n)) %>% 
  ggplot(aes(x = species_no, y = n)) +
  geom_point() +
  # scale_y_log10() +
  # scale_x_log10() +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label = fish_family)) +
  # geom_abline() +
  NULL


type_div <- data_fish_have %>% 
  distinct(fish_family, type_of_fish) %>% 
  group_by(fish_family) %>% 
  tally() %>% 
  arrange(-n) %>% 
  left_join(family_diversity)


# families <- fams %>% select(fish_family) %>% distinct()
# write.csv(families, file = "families.csv", row.names = F)



#load all diversity of fishes
fishbase_div <- read_csv("https://raw.githubusercontent.com/jswesner/fish_summaries/master/family_diversity.csv") %>% 
  mutate(prop_species_fb = no_fish_species/sum(no_fish_species),
         data = "fishbase") %>% 
  rename(no_fish_species_fb = no_fish_species)

fishbase_fresh <- read_csv("database/Fresh_Fish_Abundance.csv") %>% select(-X1) %>% 
  rename(no_fish_species_fb = species_number,
         prop_species_fb = relative_species_number,
         Family = family) %>% 
  mutate(data = "fishbase",
         prop_species_fb = no_fish_species_fb/sum(no_fish_species_fb))

fams_in_db <- data_fish %>% 
  distinct(fish_species, fish_family) %>% 
  group_by(fish_family) %>% 
  tally() %>% 
  arrange(-n) %>% 
  mutate(prop_species = n/sum(n),
         data = "in database") %>% 
  rename(no_fish_species_db = n,
         prop_species_db = prop_species,
         Family = fish_family)



all <- fishbase_fresh  %>% select(Family, no_fish_species_fb, prop_species_fb) %>% 
  # filter(no_fish_species >= 100) %>% 
  left_join(fams_in_db %>% select(Family, no_fish_species_db, prop_species_db))  %>% 
  filter(!is.na(Family)) %>% 
  gather(group, prop_species, c(prop_species_fb, prop_species_db)) %>% 
  gather(group_no, no_fish_species, c(no_fish_species_fb, no_fish_species_db)) %>% 
  mutate(database = case_when(grepl("fb", group) ~ "FishBase",TRUE ~ "Our database"),
         sort = case_when(grepl("fb", group) ~ prop_species,TRUE ~ 0),
         split = case_when(sort >= 0.001 ~ "common",
                           sort >= 0.0001 ~ "rare",
                           sort >= 0.00001 ~ "rarest"))

all

family_diversity_plot <- all %>% 
  # filter(prop_species >=0.001) %>% 
  ggplot(aes(x = reorder(Family,sort), y = prop_species, group = database, 
             color = database,
             fill = database,
             alpha = database,
             size = database)) +
  # geom_point() +
  geom_bar(stat = "identity", position = position_dodge(width = 0)) +
  coord_flip() +
  scale_size_manual(values = c(.4, 0.1)) +
  scale_alpha_manual(values = c(1, 0.4)) +
  # facet_wrap(~split) +
  theme(axis.text.y = element_text(size = 2.5)) +
  labs(x = "Fish Family",
       y = "Proportion of species in the family (freshwater families only)")

ggsave(family_diversity_plot, file = "plots/family_diversity_plots.pdf", dpi = 500, height = 20, width = 10)


# 
# setdiff(fams_in_db$Family, fishbase_div$Family)
# 
# fishbase_div %>% filter(Family == "Percichthyidae") %>% View()

# data_fish <- data_fish %>%
#   mutate(fish_family = case_when(fish_family == "Perichthyidae" ~ "Percichthyidae",
#                                  TRUE ~ fish_family))

