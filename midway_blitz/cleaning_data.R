# Load libraries and read data
library(tidyverse)
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


# Clean the data 


  
              
