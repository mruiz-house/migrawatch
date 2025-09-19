# Load libraries and read data
library(tidyverse)
library(stringr)
library(lubridate)
data_midway <- read_csv('midway_blitz/operation_midway_blitz.csv',
                        # add appropriate column names
                        col_names = c('location', 'date_time_reported',
                                      'address', 'date_time_raid',
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

# Select all dates after 9/9 
data_filtered <- data_midway %>%
              filter(date_time_raid > ymd('2025-09-08')) %>%
              filter(verified_by_rrt == 'checked')


  
              
