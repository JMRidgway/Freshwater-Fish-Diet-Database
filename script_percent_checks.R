library(tidyverse)
library(janitor)

#full database
data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
  mutate_all(funs('as.character')) %>% 
  remove_empty("rows")

data_fish %>% 
  filter(grepl("percent", measurement_typeunits)) %>% 
  ggplot(aes(x = 1:nrow(.),y = as.numeric(measure_numeric))) +
  geom_point() +
  ylim(0,1)
  
fish_by_insectstage <- data_fish %>% 
  mutate(measure_numeric = as.numeric(measure_numeric)) %>% 
  filter(grepl("percent", measurement_typeunits),
         prey_class == "Insecta",
         prey_order == "Diptera",
         prey_origin == "aquatic") %>% 
  group_by(fish_species, prey_stage, fish_family) %>% 
  summarize(mean = mean(measure_numeric, na.rm = T),
            sd = sd(measure_numeric, na.rm = T))


fish_by_insectstage_prop <- fish_by_insectstage %>% 
  group_by(fish_species, fish_family) %>% 
  mutate(total = sum(mean),
         prop = mean/total)


fish_by_insectstage_prop %>% 
  ggplot(aes(x = reorder(prey_stage, -prop), y = prop, color = fish_family)) +
  geom_point(position = position_dodge(width = 0.1)) +
  geom_boxplot(aes(group = interaction(prey_stage, fish_family))) +
  guides(color = F)


fish_by_aqterr <- data_fish %>% 
  mutate(measure_numeric = as.numeric(measure_numeric)) %>% 
  filter(grepl("percent", measurement_typeunits)) %>% 
  group_by(fish_species, prey_origin) %>% 
  summarize(mean = mean(measure_numeric, na.rm = T),
            sd = sd(measure_numeric, na.rm = T))


fish_by_aqterr %>% 
  ggplot(aes(x = prey_origin, y = mean)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_boxplot(aes(group = prey_origin), outlier.shape = NA)



data_fish %>% 
  filter(grepl("percent", measurement_typeunits)) %>% 
  group_by(fish_id, fish_species, citation, author, year, table_figure) %>% 
  summarize(sum = sum(as.numeric(measure_numeric, na.rm = T))) %>% 
  group_by(sum, fish_id, fish_species, citation, author, year, table_figure) %>% 
  tally() %>% 
  filter(sum > 103) %>% 
  View()


percent_dups <- data_fish %>% 
  filter(grepl("percent", measurement_typeunits)) %>% 
  group_by(fish_id, fish_species, citation, author, year, table_figure) %>% 
  summarize(sum = sum(as.numeric(measure_numeric, na.rm = T))) %>% 
  group_by(sum, fish_id, fish_species, citation, author, year, table_figure) %>% 
  tally()

percent_dups_id <- percent_dups %>% ungroup() %>% select(fish_id)

percent_dups_id


data_fish %>% 
  group_by(across(c(-fish_id))) %>% 
  slice(1)







