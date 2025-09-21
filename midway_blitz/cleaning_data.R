### I. Load libraries and read data
library(tidyverse)
library(stringr)
library(lubridate)
library(DT)

### II. Clean Data 
data_midway <- read_csv('midway_blitz/operation_midway_blitz.csv',
                        # add appropriate column names
                        col_names = c('location', 'date_time_reported',
                                      'unique_id', 'date_time_raid',
                                      'day', 'time_category', 
                                      'raid_calls', 'notes',
                                      'street_address', 'type_report',
                                      'detention_centers', 'tactics_reported',
                                      'rapid_response_team', 'people_detained',
                                      'verified_by_rrt', 'OCAD_operator_uploaded',
                                      'business_worksite', 'license_plate')) 



# Format the date and time correctly + make certain columns strings 
data_midway$date_time_reported <-  mdy_hm(data_midway$date_time_reported)
data_midway$date_time_raid <-  mdy_hm(data_midway$date_time_raid)


# Apply iconv to remove invalid UTF-8 characters for the entire dataframe
df_clean <- as.data.frame(lapply(data_midway, function(x) {
  if (is.character(x)) {
    iconv(x, from = "UTF-8", to = "UTF-8", sub = "")  # Remove invalid UTF-8 characters
  } else {
    x  # Leave non-character columns unchanged
  }
}))

# Select all dates after 9/9 
data_filtered <- df_clean %>%
              filter(date_time_raid > ymd('2025-09-08')) %>%
              filter(verified_by_rrt == 'checked')  %>%
              filter(!str_detect(type_report, 'False|FALSE|Rumors')) 


# convert to readable table
datatable(data_filtered)

### 3. Edits for Analysis

# People Detained 
analysis_ready <- data_filtered %>%
              mutate(people_detained = case_when(unique_id == "09-15 Chicago - Little Village-2359 S Albany" ~ 1, TRUE ~ people_detained))

# Tactics 
analysis_ready <- data_filtered %>%
  mutate(tactics_reported = case_when(unique_id == "09-15 West Chicago-813 Main St, West Chicago, IL 60185" ~ "Multiple Cars - 3", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-15 Dixmoor-14700 Robey Ave, Dixmoor, IL 60426" ~ "Multiple Cars - 3, Sensitive Location - Outside School", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-11 Geneva-Kane Co. Courthouse (100 S 3rd St, Geneva, IL 60134)" ~ "Senistive Location - Courthouse", TRUE ~ tactics_reported))



  
              
