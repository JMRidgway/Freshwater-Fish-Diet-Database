str(data_fish)


unique(data_fish$sample_size) #good, but need to clean up (sent email to Jacob on 8/1/2020 - what to do with "under")
unique(data_fish$start_date) #good - need common format
unique(data_fish$end_date) #good- need common format
unique(sort(data_fish$measurement_type)) #good
unique(sort(data_fish$measurement_typeunits)) #good - this is made in script 2
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
unique(sort(data_fish$journal)) #good
unique(sort(data_fish$site_name)) #good (some state abbreviations are not capitalized, though)
unique(sort(data_fish$predator_stage)) #good
unique(sort(data_fish$length_measure)) #good
unique(sort(data_fish$length_units)) #good, but units are in cm, mm, in, etc
unique(sort(data_fish$predator_min_length)) #good. Some are zero, but that's just means that length is <= X
unique(sort(data_fish$predator_max_length)) #good
unique(sort(data_fish$type_of_fish)) #good
unique(sort(data_fish$microhabitat)) #good
unique(sort(data_fish$author_year_tbl)) #good - CHECK NA's
unique(sort(data_fish$prey_taxon)) #OK - as entered
unique(sort(data_fish$predator_average_length)) #good
unique(sort(data_fish$fish_id)) #good, NO NA's
unique(sort(data_fish$data_sorted_by)) #good
unique(sort(data_fish$dateadded)) #good
unique(sort(data_fish$prey_kingdom)) #good, but 23,504 NAs
unique(sort(data_fish$prey_class)) #good, but 26035 NAs 
unique(sort(data_fish$prey_family)) #good, but 37158 NAs 
unique(sort(data_fish$prey_order)) #good, but 33842 NAs
unique(sort(data_fish$prey_species)) #good, but 43152 NAs
unique(sort(data_fish$unique_character)) #good, but not sure what it is. Might be a duplicate of fish_id_add 
unique(sort(data_fish$fish_id_new)) #deleted
unique(sort(data_fish$fish_genus)) #good, but why is it here?
unique(sort(data_fish$fish_species)) # 9076 NAs
unique(sort(data_fish$fish_family)) # 7154 NAs
unique(sort(data_fish$fish_class)) #7154 NAs
unique(sort(data_fish$fish_order)) #7154 NAs
unique(sort(data_fish$fish_superclass)) #22019 NAs
unique(sort(data_fish$correction_mult)) #deleted - (8/5/2020)
unique(sort(data_fish$new_min)) #deleted
unique(sort(data_fish$new_max)) #deleted
unique(sort(data_fish$new_average)) #deleted
unique(sort(data_fish$sample_id)) #what is this for?
unique(sort(data_fish$citation_to_remove)) #deleted - (8/5/2020)
unique(sort(data_fish$delete_row)) #deleted - (8/5/2020)
unique(sort(data_fish$search_term)) #good, but what is this?
unique(sort(data_fish$type_temp)) #good - this is made in script 2
unique(sort(data_fish$units_temp)) #good - this is made in script 2
unique(sort(data_fish$measure_numeric)) #good - this is made in script 2
unique(sort(data_fish$lat.x)) #deleted - (8/5/2020)
unique(sort(data_fish$long)) #deleted - (8/5/2020)
unique(sort(data_fish$lat.y)) #deleted - (8/5/2020)
unique(sort(data_fish$delete)) #deleted - (8/5/2020)
unique(sort(data_fish$fish_id_temp)) #good - this is an old version of fish id. keep for recovery
unique(sort(data_fish$fish_id_old)) #good - this is an old version of fish id. keep for recovery
unique(sort(data_fish$end_date_ymd)) #deleted - (8/5/2020) - fix dates after all data are in
unique(sort(data_fish$author_table)) #deleted - (8/5/2020) - redundant
unique(sort(data_fish$measurement_type_old)) #deleted - (8/5/2020) - redundant
unique(sort(data_fish$measurement_units_old)) #deleted - (8/5/2020) - redundant


# saveRDS(data_fish, file = "database/data_fish.rds")





