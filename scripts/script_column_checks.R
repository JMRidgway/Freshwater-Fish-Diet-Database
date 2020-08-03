str(data_fish)


unique(data_fish$sample_size) #good, but need to clean up (sent email to Jacob on 8/1/2020 - what to do with "under")
unique(data_fish$start_date) #good
unique(data_fish$end_date) #good
unique(sort(data_fish$measurement_type)) #good
unique(sort(data_fish$measurement_typeunits)) #good
unique(sort(data_fish$measurement_units)) #good
# unique(sort(data_fish$measurement_units_total_percent_other)) #removed column
# unique(sort(data_fish$measurement_units_total_percent_other)) #removed column
unique(sort(data_fish$year)) #good
unique(sort(data_fish$habitat)) #good (confirmed that all are freshwater habitats - 08/03/2020)
unique(sort(data_fish$habitat_general)) #good
unique(sort(data_fish$table_figure)) #good
unique(sort(data_fish$measurement)) #check
unique(sort(data_fish$author)) #good, but need to clean up
unique(sort(data_fish$prey_stage)) #good, but need to clean up
unique(sort(data_fish$sampling_interval)) #good, but need to clean up
unique(sort(data_fish$lat)) #good
unique(sort(data_fish$lon)) #good


unique(sort(test$measurement)) #check
test2 <- data_fish %>% filter(is.na(measure_numeric))

fix_measurement <- test2 %>% distinct(measurement, author, citation) %>% mutate(measurement = tolower(measurement)) %>% arrange(measurement) %>%
  filter(measurement != "trace") %>% 
  filter(!grepl("<", measurement)) %>% 
  filter(!grepl(">", measurement)) %>%
  mutate(how_to_fix = "") %>%
  select(measurement, how_to_fix, everything())

write.csv(fix_measurement, file = "database/data_to_add/re_do/fix_measurement.csv", row.names = F)
