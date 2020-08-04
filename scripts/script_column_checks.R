str(data_fish)


unique(data_fish$sample_size) #good, but need to clean up (sent email to Jacob on 8/1/2020 - what to do with "under")
unique(data_fish$start_date) #good - need common format
unique(data_fish$end_date) #good- need common format
unique(sort(data_fish$measurement_type)) #good
unique(sort(data_fish$measurement_typeunits)) #good
unique(sort(data_fish$measurement_units)) #good
# unique(sort(data_fish$measurement_units_total_percent_other)) #removed column
# unique(sort(data_fish$measurement_units_total_percent_other)) #removed column
unique(sort(data_fish$year)) #good
unique(sort(data_fish$habitat)) # good
unique(sort(data_fish$habitat_general)) #good
unique(sort(data_fish$table_figure)) #good
unique(sort(data_fish$measurement)) #check (sent email to Jacob on 8/3/2020 to fix typo's)
unique(sort(data_fish$author)) #good
unique(sort(data_fish$citation)) #good, but need to clean up
unique(sort(data_fish$prey_stage)) #good
unique(sort(data_fish$sampling_interval)) #good
unique(sort(data_fish$lat)) #good
unique(sort(data_fish$lon)) #good
unique(sort(data_fish$journal))

test <- data_fish
test$journal <- str_to_lower(test$journal)
test$journal <- str_to_title(test$journal)
test$journal <- str_replace(test$journal, " Of The ", " of the ")
test$journal <- str_replace(test$journal, " Of ", " of ")
test$journal <- str_replace(test$journal, "Tafs", "Transactions of the American Fisheries Society")
test$journal <- str_replace(test$journal, "Transactionsoftheamericanfisheriessociety" , "Transactions of the American Fisheries Society")
test$journal <- str_replace(test$journal, "Scientificcontributionsofthenewyorkzoologicalsociety"  , "Scientific Contributions of the New York Zoological Society")
test$journal <- str_replace(test$journal, "Revista De Biología Tropical"  , "Revista De Biologia Tropical")
test$journal <- str_replace(test$journal, "Arch. Hydrobiol."   , "Archiv Fuer Hydrobiologie")
unique(sort(test$journal))

