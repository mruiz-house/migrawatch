### I. Load libraries and read data
library(tidyverse)
library(stringr)
library(lubridate)
library(DT)

### II. Clean Data 
data_midway <- read_csv('midway_blitz/9.21.25_data.csv',
                        # add appropriate column names
                        col_names = c('location', 'date_time_reported',
                                      'unique_id', 'date_time_raid',
                                      'day', 'time_category', 
                                      'raid_calls', 'notes',
                                      'street_address', 'type_report',
                                      'detention_centers', 'tactics_reported',
                                      'rapid_response_team', 'people_detained',
                                      'verified_by_rrt', 'OCAD_operator_uploaded',
                                      'business_worksite', 'license_plate', 'source')) 

# Select all rows, delete columns 20 and 21 
data_midway <-  data_midway[, 1:19]


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
data_filtered <- data_filtered %>%
              mutate(people_detained = case_when(unique_id == "09-15 Chicago - Little Village-2359 S Albany" ~ 1, TRUE ~ people_detained))

# Tactics 
data_filtered <- data_filtered %>%
  mutate(tactics_reported = case_when(unique_id == "09-15 West Chicago-813 Main St, West Chicago, IL 60185" ~ "Multiple Cars - 3", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-15 Dixmoor-14700 Robey Ave, Dixmoor, IL 60426" ~ "Multiple Cars - 3, Sensitive Location - Outside School", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-11 Geneva-Kane Co. Courthouse (100 S 3rd St, Geneva, IL 60134)" ~ "Sensitive Location - Courthouse", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-18 Chicago - North Lawndale - 2513 W Cullerton St, Chicago, IL 60608." ~ "Staging Or Circling", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-20 Elgin-Hastings and Liberty/Route 25, Elgin" ~ "Pulling Up Cars, Reckless Engagerment, Intimidating RR Tea,", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-17 Brighton Park/McKinley Park-Archer and Western" ~ "Multiple Agents - 2", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "9-19 Chicago - Little Village- Rockwell between 19th and 21st, 60608" ~ "Intimidating RR Team", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-17 Cicero-79th and Cicero" ~ "Multiple Cars - 3, Foot Chase", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-19 Chicago - Hermosa-Dickens and Kilbourn - 60639" ~ "Multiple Agents - 5, Multiple Cars - 3", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-19 Chicago - Belmont Craign-Diversey and mango" ~ "Foot Chase, Multiple Agents - 5", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-17 Chicago - Belmont Cragin -2930 n central 60634" ~ "Violent Towards Detainee", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-17 Cicero -19th & 47th Ave Alley (The first alley east of Cicero on 19th Street)" ~ "Intimidating RR Team", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-09 Chicago - Little Village-S California Blvd & W 26th St, Chicago, IL 60623" ~ "Sensitive Location - Courthouse", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-09 Chicago - Loop-Richard J. Daley Center, 50 W Washington St, Chicago, IL 60602" ~ "Sensistive Location - Courthouse", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-09 Rolling Meadows-Cook County Circuit Court - Rolling Meadows, 2121 Euclid Ave, Rolling Meadows, IL 60008" ~ "Senistive Location - Courthouse", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-08 Chicago - West Lawn-61st and Kildare" ~ "Stake Out, Door Knocking", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-12 Elgin-Melrose & Carr" ~ "Reckless Endangerment", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-11 Elgin-Rec Centre of Elgin" ~ "Multiple Agents - 3", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-11 Cicero-Circling the block between 14th and 50th Ct" ~ "Staging or Circling", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-12 Franklin Park-Grand and Elder by a library, 60131" ~ "Car Chase", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-12 Chicago - La Villita-27 entre Homan y Trumbul" ~ "Pulling Up Cars (Traffic Stop)", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-08 Chicago - Little Village -26th Street Criminal Courthouse" ~ "Sensitive Locatioin - Courthouse", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-14 Chicago - Little Village-S Harding Ave & W 30th St." ~ "Multiple Agents (4), Face Coverings", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-13 Chicago - Pilsen-37th and Ashland" ~ "Staging Or Stake Out", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-16 Blue Island-Vermont St & Irving Ave, Blue Island, IL 60406" ~ "Pulling Up Cars", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-15 Maywood-South 1st Ave and Maybrook Drive" ~ "Sensitive Location - Courthouse", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-16 Chicago - Dan Ryan Woods-Chicago, IL 60620" ~ "Staging or Stake Out", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-16 Chicago - Pilsen-S Prairie Ave & E 16th St., Chicago, IL 60616" ~ "Federal Agency Collab, Multiple Agents - 8", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-18 Chicago - SWS-42nd Street & Ashland Ave., 60609" ~ "Multiple Vehicles - 8", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-18 Chicago - NWS-Grant at Lamon" ~ "Multiple Agents - 2, Face Covering", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-18 Chicago - SWS - Hermosa-Walmart, N Cicero Ave & W North Ave, 60639" ~ "Multiple Agents - 12", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-18 Chicago - NWS - Hermosa-Cook Brothers/Burlington @ 1740 N Kostner Ave, Chicago, IL 60639" ~ "Face Covering, Multiple Agents - 4", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-18 Chicago - SWS-71st & Damen" ~ "Pulling Up Cars", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-19 Chicago - Little Village- Rockwell between 19th and 21st, 60608" ~ "Intimidating RR Team", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-18 Elgin-Elgin Community College- 1700 Spartan Dr, Elgin, IL 60123" ~ "Sensitive Location - University", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-09 Chicago - Little Village-4457 W 26th St, Chicago, IL 60623" ~ "Face Covering, Search of Personal Belongings", TRUE ~ tactics_reported),
         tactics_reported = case_when(unique_id == "09-09 Cicero-4749 West 26th Street, 2601 S Cicero Ave, Cicero, IL 60804" ~ "Pulling Up Cars", TRUE ~ tactics_reported))
         
# Business or Worksite? 
data_filtered <- data_filtered  %>%
  mutate(business_worksite = case_when (unique_id = "09-09 Chicago - Little Village-4457 W 26th St, Chicago, IL 60623" ~ "Hardware Store", TRUE ~ business_worksite),
         business_worksite = case_when (unique_id = "09-12 Chicago - Little Village-2646 W Cermak, 60608" ~ "Hardware Store", TRUE ~ business_worksite ),
         business_worksite = case_when (unique_id = "09-18 Chicago - SWS - BP/BOTY-Home Depot, 4555 S Western Blvd, Chicago, IL 60609" ~ "Hardware Store", TRUE ~ business_worksite),
         business_worksite = case_when (unique_id = "09-18 Chicago - SWS-71st & Damen" ~ "Hardware Store", TRUE ~ business_worksite),
         business_worksite = case_when (unique_id = "09-17 Brighton Park-47th and Western" ~ "Hardware Store", TRUE ~ business_worksite),
         business_worksite = case_when (unique_id = "09-17 Naperville - Menards. 715 Fort Hill Dr Naperville, IL 60540" ~ "Hardware Store", TRUE ~ business_worksite),
         business_worksite = case_when (unique_id = "09-18 Chicago - NWS - Belmont-Fullerton & Menard" ~ "Hardware Store", TRUE ~ business_worksite),
         business_worksite = case_when (unique_id = "09-14 Chicago - Pilsen-2333 S Cicero Ave, Cicero, IL 60804" ~ "Hardware Store", TRUE ~ business_worksite))
         
  
              
