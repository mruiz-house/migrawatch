# I. Read libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(stringr)


# II. Load and transform my data
      # remember data files must be read in ""
df_abductions <- read_csv("raids_all_data3.3.csv") 

# Filter for people detained 
df_abductions <- df_abductions %>%
                  filter(`Number of People Detained` > 0) 

# Mutate with if statements by pulling out addresses 

# pull out address with regular expressions 
pattern_extract <- paste0(
  "^\\s*(?:",
  # (1) numbered street
  "(?:(\\d+\\s+[^,]+))",
  "|",
  # (2+3) intersection: optional dir prefix + street1  & optional dir prefix + street2
  "(?:\\s*((?:[NSEW]|NE|NW|SE|SW|North|South|East|West|N\\.|S\\.|E\\.|W\\.)?\\s*[^,&]+?)\\s*(?:&|and)\\s*((?:[NSEW]|NE|NW|SE|SW|North|South|East|West|N\\.|S\\.|E\\.|W\\.)?\\s*[^,]+?))",
  "),\\s*",
  "([A-Za-z .'-]+),\\s*",             # (4) city
  "([A-Z]{2})",                       # (5) state
  "(?:\\s+(\\d{5}(?:-\\d{4})?))?\\s*$" # (6) optional zip
)

df_abductions <- df_abductions %>%
  mutate(
    oneline = if_else(
      # condition to be met
      str_detect(`Street Address or Intersection`, address_pattern),
      # what occurs if true 
      str_extract(`Street Address or Intersection`, address_pattern),
      # what occurs if false
      paste0(`Street Address or Intersection`, ", ", `Neighborhood or City`, ", ", `ZIP Code`)
    )
  )
         


# III. Write and Iterate through loop
base_url <- "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress"

# Create output columns
df_abductions$census_address <- NA_character_
df_abductions$lat <- NA
df_abductions$lon <- NA

for (i in seq_len(nrow(df_abductions))) {
  resp <- try(
    GET(base_url, query = list(
      address   = df_abductions$oneline[i],
      benchmark = "Public_AR_Current",
      format    = "json"
    )),
    silent = TRUE
  )
  
  # network error or try-error -> skip
  if (inherits(resp, "try-error")) { Sys.sleep(0.2); next }
  
  # require HTTP 200
  if (status_code(resp) != 200) { Sys.sleep(0.2); next }
  
  # parse JSON safely
  parsed <- tryCatch(
    fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed)) { Sys.sleep(0.2); next }
  
  matches <- parsed$result$addressMatches
  if (is.null(matches) || length(matches) == 0) { Sys.sleep(0.2); next }
  
  m0 <- matches[[1]]
  
  # safe extraction with fallback to NA
  df_abductions$census_address[i] <- if (!is.null(m0$matchedAddress) && nzchar(m0$matchedAddress)) as.character(m0$matchedAddress) else NA_character_
  df_abductions$lon[i]     <- if (!is.null(m0$coordinates$x)) as.numeric(m0$coordinates$x) else NA_real_
  df_abductions$lat[i]     <- if (!is.null(m0$coordinates$y)) as.numeric(m0$coordinates$y) else NA_real_
  
  Sys.sleep(0.2)  # polite pause
}

write_csv(df_abductions, "D:/Datos/Monica/Downloads/geocoded_abductions_3.5.csv")