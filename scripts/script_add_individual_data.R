library(brms)
library(tidyverse)
library(ggridges)
library(lubridate)
library(RCurl)
library(cowplot)
library(janitor)




# Load most recent data ---------------------------------------------------

data_fish <- read_csv("database/data_fish.csv") %>% clean_names() %>% 
  mutate(data_sorted_by = NA,
         notes = NA)

saveRDS(data_fish_test, file = "database/data_fish.rds")
write.csv(data_fish_test, file = "database/data_fish.csv")
#get column names
cols <- names(data_fish)

#function to add columns to data_sheet if not already added
fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
}

centrarchidae_harris_2 <- fncols(read_csv("data_editedRaw/2020-01-14/centrarchidae_harris_2.csv") %>% clean_names(), cols) %>% 
  mutate(predator_min_length = as.character(predator_min_length),
         predator_max_length = as.character(predator_max_length),
         start_day = as.character(start_day),
         end_day = as.character(end_day))

data_fish_test <- data_fish %>% bind_rows(centrarchidae_harris_2)

centr_cols <- names(centrarchidae_harris_2)

setdiff(centr_cols, cols)

