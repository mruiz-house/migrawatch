library(tidyverse)
zip_codes <- read_csv("dashboard_scripts/zip_codes.csv") 


df_reports <- zip_codes %>%
  # drop literal double-quotes if present
  mutate(Raids = str_replace_all(Raids, '"', '')) %>%
  # split wherever a new "Report-" starts (positive lookahead)
  separate_rows(Raids, sep = "(?=Report-)", convert = FALSE) %>%
  # remove leading/trailing punctuation/commas/spaces
  mutate(Raids = str_trim(str_remove_all(Raids, "^,|,$"))) %>%
  # drop any empty fragments
  filter(Raids != "")

write_csv(df_reports, "cleaned_zip_codes")