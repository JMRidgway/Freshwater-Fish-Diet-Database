library(brms)
library(tidyverse)
library(ggridges)
library(lubridate)
library(httr)
library(cowplot)
library(janitor)
library(repmis)
library(readxl)
library(rlist)
library(ggmap)

data_fish_refined_taxized_latlon %>% distinct(new_fish_id, .keep_all = T)

na_string = c("na", "NA", "n/a", "NA")

d <- data_fish_refined_taxized %>% 
  mutate_at(vars(start_month, start_year, start_day, 
                 end_month, end_year, end_day), na_if, "na") %>% 
  mutate_at(vars(start_month, start_year, start_day, 
                 end_month, end_year, end_day), na_if, "n/a") %>% 
  mutate_at(vars(start_month, start_year, start_day, 
                  end_month, end_year, end_day), na_if, "NA") %>% 
  mutate_at(vars(start_month, start_year, start_day, 
                 end_month, end_year, end_day), na_if, "N/A") %>% 
  mutate(start_day = case_when(is.na(start_day) ~ "1",
                               TRUE ~ start_day),
         end_day = case_when(is.na(end_day) ~ "1",
                             TRUE ~ end_day),
         start_date_add = ymd(paste(start_year, "_", start_month,"_", start_day)),
         end_date_add = ymd(paste(end_year, "_", end_month,"_", end_day)),
         start_date = ymd(as.Date(start_date, origin = "1899-12-30")),
         end_date = ymd(as.Date(end_date, origin = "1899-12-30")),
         start_date = case_when(is.na(start_date) ~ start_date_add,
                                TRUE ~ start_date),
         end_date = case_when(is.na(end_date)~end_date_add,
                              TRUE ~ end_date),
         temp_startyear = year(start_date),
         temp_endyear = year(end_date),
         temp_startmonth = month(start_date),
         temp_endmonth = month(end_date),
         temp_startday = day(start_date),
         temp_endday = day(end_date),
         temp_start_year = case_when(temp_startyear >= 2019 ~ year-3,
                                     TRUE ~ temp_startyear),
         temp_end_year = case_when(temp_endyear >= 2019 ~ year-3,
                                   TRUE ~ temp_endyear),
         start_date_good = ymd(str_replace(paste(temp_start_year,"-",temp_startmonth,"-", temp_startday)," ", "")),
         end_date_good = ymd(str_replace(paste(temp_end_year,"-",temp_endmonth,"-", temp_endday)," ", "")),
         start_date_old = start_date,
         end_date_old = end_date,
         start_date = start_date_good,
         end_date = end_date_good) %>% 
  select(-temp_startyear,
         -temp_endyear,
         -temp_startmonth,
         -temp_startday,
         -temp_endday,
         -temp_start_year,
         -temp_end_year,
         -start_date_add,
         -end_date_add,
         -temp_endmonth,
         -start_date_good,
         -end_date_good,
         -long)




# analyze for poster ------------------------------------------------------
#non-numeric measurements

d2 <- d2_refined %>% 
  mutate(measurement = as.numeric(case_when(grepl("<",measurement) ~ "0.5",
                         TRUE ~ measurement)),
         prey_stage = case_when(grepl("nonadult", prey_taxon) ~ "larvae/pupae",
                                TRUE ~ prey_stage))

d2_refined %>% filter(is.na(type_of_fish))

#dataframe contains only chiros and only samples that include stage info
d2_chiro <- d2 %>% 
  filter(measurement_type_temp == "number - percent" | 
           measurement_type_temp == "volume - percent" | 
           measurement_type_temp == "weight - percent",
         measurement <= 100) %>% 
  group_by(new_fish_id) %>% 
  filter(any(prey_stage == "pupae" | prey_stage == "adults"),
         prey_family == "Chironomidae") %>% 
  group_by(new_fish_id, prey_stage, start_date, type_of_fish,fish_order, fish_family, fish_species, author, year) %>% 
  summarize(mean = mean(measurement))
  

d %>% filter(type_of_fish == "NA")
d2 %>% group_by(type_of_fish) %>% 
  tally() %>% 
  arrange(-n)

d2_chiro %>%
  filter(prey_family == "Chironomidae",
         prey_stage == "adults"|prey_stage == "larvae" | prey_stage == "pupae" | prey_stage == "eggs") %>% 
  ggplot(aes(y = measurement_type_temp, x = measurement, fill = prey_stage)) +
  geom_density_ridges()
