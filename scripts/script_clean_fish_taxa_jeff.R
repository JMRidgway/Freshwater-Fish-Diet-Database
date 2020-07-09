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

type_div %>%
  filter(!is.na(fish_family)) %>% 
  ggplot(aes(x = species_no, y = n)) +
  geom_point(intercept =)


# families <- fams %>% select(fish_family) %>% distinct()
# write.csv(families, file = "families.csv", row.names = F)
