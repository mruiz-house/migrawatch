library(tidyverse)
library(stringr)
# Make sure to quote paths!
addresses <- read_csv("dashboard_scripts/Updated_street_addresses.csv") %>%
  filter(`Number of People Detained` >=1 ) %>%
  rename(street = `Street Address or Intersection`) %>%
  mutate(City = "",
         State = "")

# Transfer over cities and states to new column
clean_addresses <- addresses %>%
  # separate the city names 
  mutate(City = case_when(str_detect(street, "Chicago") ~ "Chicago", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "Chicago") ~ "Chicago", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "Skokie") ~ "Skokie", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "Evanston") ~ "Evanston", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "Broadview") ~ "Broadview", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "Calumet Park") ~ "Calumet Park", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "Blue Island") ~ "Blue Island", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "Rolling Meadows") ~ "Rolling Meadows", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "East Dundee") ~ "East Dundee", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "Oak Park") ~ "Oak Park", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "Aurora") ~ "Aurora", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "Elgin") ~ "Elgin", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "Cicero") ~ "Cicero", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "Joliet") ~ "Joliet", TRUE ~ City),
         City = case_when(str_detect(`Neighborhood or City`, "Hoffman Estates") ~ "Hoffman Estates", TRUE ~ City),
  # separate the state 
         State = case_when(str_detect(`Neighborhood or City`, "Chicago|Elgin|Cicero") ~ "IL", TRUE ~ State),
         State = case_when(str_detect(street, "Chicago|Elgin|Cicero") ~ "IL", TRUE ~ State)) %>%
  # delete zip codes, city, and state from the street address
  mutate(street = str_remove_all(street, "\\d{5}"),
         street = str_remove_all(street, "Hoffman Estates|Joliet|Chicago|Broadview|Evanston|Skokie|Cicero|Elgin|Calumet Park|Aurora|Oak Park|Rolling Meadows|East Dundee"),
         street = str_remove_all(street, "IL|Il"),
         street = str_remove_all(street, "[:punct:]"),
         street = str_remove_all(street, "USA|United States"))
	

# String Squish! 
clean_addresses$street <- str_squish(clean_addresses$street) 

#write our beautiful csv 
write_csv(clean_addresses, "cleaned_addresses.csv")

