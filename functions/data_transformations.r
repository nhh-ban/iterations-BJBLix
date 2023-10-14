
#Converting metadata into a tibble
transform_metadata_to_df <- function(metadata){
  metadata$trafficRegistrationPoints %>% 
    bind_rows() %>%
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>%  
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
    unnest_wider(location) %>% 
    unnest_wider(latLon)
}

# This function changes datetime to ISO8601 with an optional offset 
# This function requires lubridate and anytime library
to_iso8601 <- function(date_time, days_offset){
  adjusted_date_time <- date_time + days(days_offset)
  iso_8601_adjusted_time <- paste0(iso8601(adjusted_date_time), "Z")
  return(iso_8601_adjusted_time)
}

#testing the function
to_iso8601(as_datetime("2016-09-01 10:11:12"),-4)


transform_volumes <- function(data){
  data <- toJSON(data) # Transform to a JSON object
  data_df <- as.data.frame(fromJSON(data))$node %>% #Transform the JSON object to a dataframe
    mutate(volume = total$volumeNumber$volume, #Extract the volume from the nested elem
           to = ymd_hms(to), # Convert to datetime
           from = ymd_hms(from), # Convert to datetime
           volume = as.numeric(volume)) %>% # Convert to numerical
    select(c("from", "to", "volume")) # Select the columns I need
  return(data_df) # Return the dataframe
}

