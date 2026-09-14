# Script: clean_episodes_data.r
# Purpose: extract selected variables from GDP sheet
# Input: Data/Raw Data/NewComponents1978-2014_final.xlsx
# Output: Data/Clean Data/episodes.csv

# 1 - Load Libraries
library(readxl)
library(readr)

# 2 - Load Raw Data from GDP sheet without headers
raw_data <- read_excel(
  "Data/Raw Data/NewComponents1978-2014_final.xlsx",
  sheet = "GDP",
  col_names = FALSE
)

# 3 - Build column names from first two rows
header_row_1 <- as.character(unlist(raw_data[1, ]))
header_row_2 <- as.character(unlist(raw_data[2, ]))

col_names_resolved <- ifelse(
  !is.na(header_row_2) & header_row_2 != "",
  header_row_2,
  header_row_1
)

# 4 - Variables to keep
variables_keep <- c(
  "New Plan", "EB", "TB", "Direct", "Indirect", "Consumption", "Transfers"
)

# 5 - Identify columns to keep:
# first two columns + requested variables
keep_idx <- c(1, 2, which(col_names_resolved %in% variables_keep))
keep_idx <- unique(keep_idx)

# 6 - Subset data:
# remove first two header rows, keep selected columns
episodes_data <- raw_data[-c(1, 2), keep_idx]

# 7 - Assign unique column names
names(episodes_data) <- make.unique(col_names_resolved[keep_idx])

# 8 - Remove duplicate TB column created as TB.1
episodes_data <- episodes_data[, names(episodes_data) != "TB.1"]

# 9 - Save CSV
write_csv(episodes_data, "Data/Clean Data/episodes.csv")