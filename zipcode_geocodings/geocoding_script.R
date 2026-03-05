# 1. Read libraries
library(tidyverse)
library(httr)
library(jsonlite)

# 2. Load and transform my data
              # remember datafiles must be read in ""
df <- read_csv("zipcode_geocodings/chicago_polling_optimized_coordinates.csv",
               col_names = c("ID", "Address", "City", "State", "Rm"))

# Drop end column
df <- df[, 1:4]

# Aggregate address
df$oneline <- paste(df$Address, df$City, df$State, sep = ", ")


# 3. Write and Iterate through loop
base_url <- "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress"

# Create output columns
df$zipcode <- NA
df$lat <- NA
df$lon <- NA

for (i in seq_len(nrow(df))) {
  resp <- try(
    GET(base_url, query = list(
      address   = df$oneline[i],
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
  df$zipcode[i] <- if (!is.null(m0$addressComponents$zip)) as.character(m0$addressComponents$zip) else NA_character_
  df$lon[i]     <- if (!is.null(m0$coordinates$x)) as.numeric(m0$coordinates$x) else NA_real_
  df$lat[i]     <- if (!is.null(m0$coordinates$y)) as.numeric(m0$coordinates$y) else NA_real_
  
  Sys.sleep(0.2)  # polite pause
}

write_csv(df, "D:/Datos/Monica/Downloads/geocoded_output.csv")