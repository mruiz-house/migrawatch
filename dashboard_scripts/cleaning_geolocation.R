library(tidyverse)
library(stringr)
# Make sure to quote paths!
addresses <- read_csv("dashboard_scripts/street_addresses.csv")

# Transfer over cities and states to new column
clean_addresses <- addresses %>%
  # separate the city names 
  mutate(City = case_when(str_detect(`Street Address`, "Chicago") ~ "Chicago", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Skokie") ~ "Skokie", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Evanston") ~ "Evanston", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Broadview") ~ "Broadview", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Calumet Park") ~ "Calumet Park", TRUE ~ City),
         City = case_when(str_detect(`Street Address`, "Blue Island") ~ "Blue Island", TRUE ~ City),
  # separate the state 
         State = case_when(str_detect(`Street Address`, "Chicago") ~ "IL", TRUE ~ State),
         State = case_when(str_detect(`Street Address`, "IL") ~ "IL", TRUE ~ State)) %>%
  # delete zip codes, city, and state from the street addres 
  mutate(`Street Address` = str_remove_all(`Street Address`, "\\d{5}"),
         `Street Address` = str_remove_all(`Street Address`, "Chicago|Broadview|Evanston|Skokie"),
         `Street Address` = str_remove_all(`Street Address`, "IL|Il"),
         `Street Address` = str_remove_all(`Street Address`, "[:punct:]"))

# String Squish! 
clean_addresses$`Street Address` <- 

