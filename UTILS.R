library(tidyverse)
library(jsonlite)
library(lubridate)

import_exposure_keys <- function(file_exported_from = "iPhone", data_dir = "json_exposure_files"){
  files <- file.path(data_dir, dir(data_dir)) 
  for(file in files){
    if(file == files[1]){df <- tibble()}
    file_data <- fromJSON(file)
    
    if(toupper(file_exported_from) == "IPHONE"){
      for(i in 1:length(file_data$ExposureChecks$Timestamp)){
        df <- df %>% 
          bind_rows(tibble(file_data$ExposureChecks$Files[[i]]["Hash"] ,
                           file_data$ExposureChecks$Files[[i]]["Timestamp"],
                           file_data$ExposureChecks$Files[[i]]["MatchCount"],
                           file_data$ExposureChecks$Files[[i]]["KeyCount"]) %>%
                      rename(hash = Hash, timestamp = Timestamp, match_count = MatchCount, key_count = KeyCount) %>% 
                      select(hash, timestamp, match_count, key_count) %>% 
                      mutate(timestamp = as_datetime(timestamp)) )
      }
    }
    
    if(toupper(file_exported_from) == "ANDROID"){
      df <- df %>%
        bind_rows(as_tibble(file_data) %>%
                    rename(key_count = keyCount, match_count = matchesCount) %>%
                    select(hash, timestamp, match_count, key_count) %>%
                    mutate(timestamp = dmy_hms(timestamp)) )
    }
    rm(file_data)
  }
  
  df %>% 
    arrange(desc(timestamp)) %>%
    mutate(date = date(timestamp)) %>%
    distinct()
}