# Script: merge_raw_data.r
# Purpose: merge oecd_capb, macro, and episodes onto oecd_capb
# Input: Data/Clean Data/oecd_capb.csv
#        Data/Clean Data/macro.csv
#        Data/Clean Data/episodes.csv
# Output: Data/Clean Data/raw_full.csv

# 1 - Load Libraries
library(readr)
library(dplyr)

# 2 - Load Datasets
oecd_capb <- read_csv("Data/Clean Data/oecd_capb.csv", show_col_types = FALSE)
macro     <- read_csv("Data/Clean Data/macro.csv", show_col_types = FALSE)
episodes  <- read_csv("Data/Clean Data/episodes.csv", show_col_types = FALSE)

# 3 - Standardise Merge Keys
oecd_capb <- oecd_capb |>
  dplyr::rename(
    country_code = 1,
    year = 2
  ) |>
  dplyr::mutate(year = as.integer(year))

macro <- macro |>
  dplyr::rename(
    country_code = 1,
    year = 2
  ) |>
  dplyr::mutate(year = as.integer(year))

episodes <- episodes |>
  dplyr::rename(
    country_code = 1,
    year = 2
  ) |>
  dplyr::mutate(year = as.integer(year))

# 4 - Merge Everything onto OECD CAPB
raw_full <- oecd_capb |>
  dplyr::left_join(macro, by = c("country_code", "year")) |>
  dplyr::left_join(episodes, by = c("country_code", "year")) |>
  dplyr::arrange(country_code, year)

# 5 - Save Final Dataset
write_csv(raw_full, "Data/Clean Data/raw_full.csv")