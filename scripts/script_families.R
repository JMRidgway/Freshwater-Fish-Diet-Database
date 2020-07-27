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

#load master data_frame
data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
  mutate_all(funs('as.character')) %>% 
  remove_empty("rows")


#load all diversity of fishes
# fishbase_div <- read_csv("https://raw.githubusercontent.com/jswesner/fish_summaries/master/family_diversity.csv") %>% 
#   mutate(prop_species_fb = no_fish_species/sum(no_fish_species),
#          data = "fishbase") %>% 
#   rename(no_fish_species_fb = no_fish_species)

fishbase_fresh <- read_csv("database/Fresh_Fish_Abundance.csv") %>% select(-X1) %>% 
  rename(no_fish_species_fb = species_number,
         prop_species_fb = relative_species_number,
         Family = family) %>% 
  mutate(data = "fishbase",
         prop_species_fb = no_fish_species_fb/sum(no_fish_species_fb))


#make list of families in the main data_fish
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


#combine our database and fishbase
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


#make the plot
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

#save the plot
ggsave(family_diversity_plot, file = "plots/family_diversity_plots.pdf", dpi = 500, height = 20, width = 10)