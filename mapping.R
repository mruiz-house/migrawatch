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
      c("KENWOOD", "WOODLAWN", 'HYDE PARK'
        'ROGERS PARK', 'ALBANY PARK', 'PORTAGE PARK', 
        'IRVING PARK', 'BELMONT CRAGIN', 'HERMOSA', 	
        'AVONDALE', 'LOGAN SQUARE', ) 
      ~ "Yes",
      TRUE ~ rapid_response))

# add in teams 

# Plot the community areas
ggplot() +
  geom_sf(data = chicago_communities, fill = "lightblue", color = "black", alpha = 0.5) +
  geom_sf_text(data = chicago_communities, aes_string(label = "community"), size = 1) +
  labs(title = "Rapid Response Teams in Chicago",
       subtitle = "Official ICIRR Affiliates") +
  theme_minimal()