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


data_fish <- data_fish %>% mutate(table_figure = str_replace_all(table_figure, "tbl1...8", "tbl1"))

data_fish %>% filter(str_detect(table_figure, "tbl1...8")) %>% View()
data_fish %>% filter(grepl("Dutton", author)) %>% distinct(table_figure, .keep_all = T) %>% View()                                                                                   
