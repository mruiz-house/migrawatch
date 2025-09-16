library(tidyverse)
library(ggplot2)
library(sf)

# Set your GeoJSON file path
geojson_file <- "chicago_community_boundaries.json"

# Read the GeoJSON file + rapid_response.csv
chicago_communities <- st_read(geojson_file)

# Add columns for rapid response
chicago_communities <- chicago_communities %>%
  mutate(rapid_response = case_when(community %in%
                                      c('KENWOOD', 'WOODLAWN', 'HYDE PARK',
                                        'ROGERS PARK', 'ALBANY PARK', 'PORTAGE PARK', 
                                        'IRVING PARK', 'BELMONT CRAGIN', 'HERMOSA', 	
                                        'AVONDALE', 'LOGAN SQUARE', 'HUMBOLDT PARK','UPTOWN', 'LOWER WEST SIDE',
                                        'SOUTH CHICAGO', 'PULLMAN', 'EAST SIDE', 'HEGEWISCH',
                                        'BRIDGEPORT', 'EDGEWATER', 'SOUTH SHORE',
                                        'ROSELAND', 'CALUMET HEIGHTS','AVALON PARK',
                                        'GAGE PARK', 'BRIGHTON PARK', 'CHICAGO LAWN',
                                        'NEW CITY', 'LINCOLN PARK', 'LAKE VIEW', 'SOUTH LAWNDALE', 
                                        'MCKINLEY PARK', 'ARCHER HEIGHTS', 'ARMOUR SQUARE') 
                                    ~ "Yes",
                                    TRUE ~ "No"))

# add in teams 
# PUÑO, 
chicago_communities <- chicago_communities %>%
  mutate(team_name = case_when(community %in% c('LOWER WEST SIDE')
                                  ~ "PUÑO",
                               TRUE ~  NA_character_))

# Hyde Park 
chicago_communities <- chicago_communities %>%
  mutate(team_name = case_when(community %in%
                                 c('KENWOOD', 'WOODLAWN', 'HYDE PARK') 
                               ~ "Hyde Park",
                               TRUE ~ team_name))
# Northwest Defense 
chicago_communities <- chicago_communities %>%
  mutate(team_name = case_when(community %in%
                                 c('PORTAGE PARK', 
                                   'IRVING PARK', 
                                   'BELMONT CRAGIN',
                                   'AVONDALE', 
                                   'LOGAN SQUARE', 
                                   'HUMBOLDT PARK',
                                   'HERMOSA') 
                               ~ "NW Defense",
                               TRUE ~ team_name))

# Far South
chicago_communities <- chicago_communities %>%
  mutate(team_name = case_when(community %in%
                                 c('PULLMAN', 'SOUTH SHORE',
                                   'ROSELAND', 'CALUMET HEIGHTS','AVALON PARK', 'SOUTH CHICAGO') ~ 'Far South',
                               TRUE ~ team_name))

# Southwest 
chicago_communities <- chicago_communities %>%
  mutate(team_name = case_when(community %in%
                                 c('GAGE PARK', 'BRIGHTON PARK', 'CHICAGO LAWN',
                                   'NEW CITY','MCKINLEY PARK', 'ARCHER HEIGHTS') ~ 'SW Side',
                               TRUE ~ team_name))
# Uptown 
chicago_communities <- chicago_communities %>%
  mutate(team_name = case_when(community %in%
                                 c('EDGEWATER', 'UPTOWN', 'LINCOLN PARK', 'LAKE VIEW') ~ 'Uptown',
                               TRUE ~ team_name))
# Rogers Park
chicago_communities <- chicago_communities %>%
  mutate(team_name = case_when(community == 'ROGERS PARK' ~ 'Protect RP',
         TRUE ~ team_name))

# Chinatown 
chicago_communities <- chicago_communities %>%
  mutate(team_name = case_when(community == 'BRIDGEPORT' ~ 'Chinatown',
                               TRUE ~ team_name))

# La Villita Se Defiende 
chicago_communities <- chicago_communities %>%
  mutate(team_name = case_when(community == 'SOUTH LAWNDALE'
                                  ~ 'LVSD',
                               TRUE ~ team_name))

# East Chicago
chicago_communities <- chicago_communities %>%
  mutate(team_name = case_when(community %in% c('HEGEWISCH', 'EAST SIDE')
                               ~ 'East Chi',
                               TRUE ~ team_name))
# APDN
chicago_communities <- chicago_communities %>%
  mutate(team_name = case_when(community == 'ALBANY PARK'
                               ~ 'APDN',
                               TRUE ~ team_name))


# Plot the community areas
ggplot(chicago_communities) + 
  geom_sf(aes(fill = rapid_response), color = "black", alpha = 0.5) +
  geom_sf_text(data = chicago_communities, aes_string(label = "team_name"), size = 1.25) +
  labs(title = "Rapid Response Teams in Chicago",
       subtitle = "Official ICIRR x OCAD Affiliates") +
  theme_minimal()
