# Load libraries and read data
library(tidyverse)
data_midway <- read_csv("midway_blitz/operation_midway_blitz.csv")

# Select all raids from 9/9 onward that are proven
data_midway <- data_midway %>%
              
