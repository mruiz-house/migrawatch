library(tidyverse)
library(stringr)
# Make sure to quote paths!
addresses <- read_csv("dashboard_scripts/Updated_street_addresses.csv")

# Transfer over cities and states to new column
clean_addresses <- addresses %>%
  # separate the city names 
  mutate(City = case_when(str_detect(`Street Address`, "48th and Rockwell|1352 32nd St|Chicago") ~ "Chicago", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Skokie") ~ "Skokie", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Evanston") ~ "Evanston", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Broadview") ~ "Broadview", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Calumet Park") ~ "Calumet Park", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Blue Island") ~ "Blue Island", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Rolling Meadows") ~ "Rolling Meadows", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "East Dundee") ~ "East Dundee", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Oak Park") ~ "Oak Park", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Aurora") ~ "Aurora", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Elgin") ~ "Elgin", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Cicero") ~ "Cicero", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Joliet") ~ "Joliet", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Hoffman Estates") ~ "Hoffman Estates", TRUE ~ City),
  # separate the state 
         State = case_when(str_detect(`Street Address`, "48th and Rockwell|1352 32nd St|Chicago|Elgin|Cicero") ~ "IL", TRUE ~ State),
         State = case_when(str_detect(`Street Address`, "IL") ~ "IL", TRUE ~ State)) %>%
  # delete zip codes, city, and state from the street addres 
  mutate(`Street Address` = str_remove_all(`Street Address`, "\\d{5}"),
         `Street Address` = str_remove_all(`Street Address`, "Hoffman Estates|Joliet|Chicago|Broadview|Evanston|Skokie|Cicero|Elgin|Calumet Park|Aurora|Oak Park|Rolling Meadows|East Dundee"),
         `Street Address` = str_remove_all(`Street Address`, "IL|Il"),
         `Street Address` = str_remove_all(`Street Address`, "[:punct:]"),
         `Street Address` = str_remove_all(`Street Address`, "USA|United States"))

# Some additional manual cleaning: 
clean_addresses <- clean_addresses %>% 
  mutate(`Street Address` = 
           case_when(`Street Address` == "2219 S St Louis  " ~ "2219 South Saint Louis Street", TRUE ~ `Street Address`),
           case_when(`Street Address` == "Esperanza Health centers on 21st and California " ~ "2001 S California Ave #100", TRUE ~ `Street Address`)) 

	

# String Squish! 
clean_addresses$`Street Address` <- str_squish(clean_addresses$`Street Address`) 

#write our beautiful csv 
write_csv(clean_addresses, "cleaned_addresses.csv")

