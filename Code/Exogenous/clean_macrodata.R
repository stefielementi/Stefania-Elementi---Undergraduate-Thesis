# Script: clean_macrodata.r
# Purpose: extract selected variables from MacroData sheet
# Input: Data/Raw Data/NewComponents1978-2014_final.xlsx
# Output: Data/Clean Data/macro.csv

# 1 - Load Libraries
library(readxl)
library(dplyr)
library(readr)

# 2 - Load Raw Data from MacroData sheet
raw_data <- read_excel(
  "Data/Raw Data/NewComponents1978-2014_final.xlsx",
  sheet = "MacroData"
)

# 3 - Select Variables to Keep
variables_keep <- c(
  "gdptr", "gdp", "r_sh", "r_lo", "itv", "utr", "CPI", "CC", "IC",
  "SMC", "BC", "rexr", "debta", "netint", "deficit", "primary_def",
  "dis", "receipts", "currentdisb_gdp",
  "currentdisb", "igaa"
)

# 4 - Keep First Column (country code), Third Column (year), and Selected Variables
macro_data <- raw_data |>
  dplyr::select(1, 3, any_of(variables_keep))

# 5 - Save CSV
write_csv(macro_data, "Data/Clean Data/macro.csv")