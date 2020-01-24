library(tidyverse)


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

#load summer_data_analyze_ad
summer_data_analyze_ad <- read_csv("summer_2019_edited_csv/summer_data_analyze_ad.csv") %>% 
  mutate_all(funs('as.character'))



data_fish <- bind_rows("jan_2020_method" = data_fish_have, 
                  "summer_data_analyze.csv" = summer_data_analyze_ad, .id = "data_source") %>% 
  select(-X1)

saveRDS(data_fish, file = "database/data_fish.rds")
