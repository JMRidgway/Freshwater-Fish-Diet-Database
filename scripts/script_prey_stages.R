library(tidyverse)
library(janitor)

#full database
data_fish <- readRDS(url("https://github.com/JMRidgway/Freshwater-Fish-Diet-Database/blob/master/database/data_fish.rds?raw=true")) %>% 
  mutate_all(funs('as.character')) %>% 
  remove_empty("rows")

#Some stage information was entered during data extraction or through 1_script_load_data.R. If not, then the following terms were used to infer prey life stage. 
#Terms (like "and_pupa") were chosen after first scanning the list of prey_taxon names and then filtering based on those terms
#All strings of terms were checked before inferring stage to ensure that they referred to the same thing. 

temp <- data_fish %>% 
  mutate(prey_stage = case_when(is.na(prey_stage) ~ "unknown",
                                prey_stage == "na" ~ "unknown",
                                TRUE ~ prey_stage)) %>% 
  mutate(prey_stage = case_when(grepl("and_pupa", prey_taxon) ~ "larvae/pupae",
                                grepl("and_larv", prey_taxon) ~ "larvae/pupae",
                                grepl("pae_and_adult", prey_taxon) ~ "adults/pupae",
                                grepl("vae_and_adult", prey_taxon) ~ "larvae/adults",
                                grepl("imag", prey_taxon) ~ "adults",
                                grepl("sub_imag", prey_taxon) ~ "subadults",
                                grepl("emerg", prey_taxon) ~ "adults",
                                grepl("aerial", prey_taxon) ~ "adults",
                                grepl("eggs_and", prey_taxon) ~ "unknown",
                                grepl("ova", prey_taxon) ~ "eggs",
                                grepl("egg", prey_taxon) ~ "eggs",
                                grepl("newt", prey_taxon) ~ "larvae",
                                grepl("tadp", prey_taxon) ~ "juvenile",
                                grepl("fry", prey_taxon) ~ "juvenile",
                                grepl("uvenile", prey_taxon) ~ "juvenile",
                                TRUE ~ prey_stage))

temp %>% 
  group_by(prey_stage) %>% 
  tally()

data_fish <- temp
