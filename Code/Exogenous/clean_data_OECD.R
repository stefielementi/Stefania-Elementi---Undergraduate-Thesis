# Script: clean_data_OECD.r
# Purpose: clean OECD raw data, keep selected variables, create a full country-year panel
#          from 1978 to 2014, and merge OECD GDP projections
# Input: Data/Raw Data/OECD.ECO.MAD,DSD_EO@DF_EO,+..A.csv
#        Data/Raw Data/OECD.ECO.MAD,DSD_EO_LTB@DF_EO_LTB,+all.csv
# Output: Data/Clean Data/oecd_capb.csv

# 1 - Load Libraries
library(readr)
library(dplyr)
library(tidyr)

# 2 - Load Raw Data
raw_data <- read_csv(
  "Data/Raw Data/OECD.ECO.MAD,DSD_EO@DF_EO,+..A.csv",
  show_col_types = FALSE
)

# 3 - Select Countries
countries_keep <- c(
  "AUS", "AUT", "BEL", "CAN", "DEU", "DNK", "ESP", "FIN",
  "FRA", "GBR", "IRL", "ITA", "JPN", "PRT", "SWE", "USA"
)

# 4 - Select Variables
variables_keep <- c("NLGXA", "GAP", "NLGXQA", "IRS","IRL")

# 5 - Select Columns (from raw dataset)
columns_keep <- c("REF_AREA", "MEASURE", "TIME_PERIOD", "OBS_VALUE")

# 6 - Keep Variables and Columns within Time Period (1978-2014)
filtered_raw_data <- raw_data |>
  dplyr::select(any_of(columns_keep)) |>
  dplyr::filter(
    REF_AREA %in% countries_keep,
    MEASURE %in% variables_keep
  ) |>
  dplyr::mutate(
    TIME_PERIOD = as.integer(TIME_PERIOD),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) |>
  dplyr::filter(TIME_PERIOD >= 1978, TIME_PERIOD <= 2014) |>
  dplyr::distinct(REF_AREA, MEASURE, TIME_PERIOD, .keep_all = TRUE)

# 7 - Create full country-year panel, then pivot data onto it
full_panel <- tidyr::expand_grid(
  REF_AREA = countries_keep,
  TIME_PERIOD = 1978:2014
)

wide_data <- filtered_raw_data |>
  tidyr::pivot_wider(
    names_from = MEASURE,
    values_from = OBS_VALUE
  )

clean_data <- full_panel |>
  dplyr::left_join(wide_data, by = c("REF_AREA", "TIME_PERIOD")) |>
  dplyr::rename(CAPB = NLGXA) |>
  dplyr::arrange(REF_AREA, TIME_PERIOD)

# 8 - Add GDP Growth Projections (same merge structure)
projections_data <- read_csv(
  "Data/Raw Data/OECD.ECO.MAD,DSD_EO_LTB@DF_EO_LTB,+all.csv",
  show_col_types = FALSE
)

countries_in_clean <- unique(clean_data$REF_AREA)

# Extract GDP under BAU1
gdp_bau1 <- projections_data %>%
  dplyr::select(REF_AREA, MEASURE, SCENARIO, TIME_PERIOD, OBS_VALUE) %>%
  dplyr::mutate(TIME_PERIOD = as.integer(TIME_PERIOD)) %>%
  dplyr::filter(
    REF_AREA %in% countries_in_clean,
    MEASURE == "GDP",
    SCENARIO == "BAU1"
  ) %>%
  dplyr::transmute(
    REF_AREA,
    TIME_PERIOD,
    GDP_BAU1 = as.numeric(OBS_VALUE)
  ) %>%
  dplyr::distinct(REF_AREA, TIME_PERIOD, .keep_all = TRUE)

# Merge into clean_data
# (1) Merge GDP_BAU1 onto existing years
clean_data <- clean_data %>%
  dplyr::left_join(gdp_bau1, by = c("REF_AREA", "TIME_PERIOD"))

max_hist_year <- max(clean_data$TIME_PERIOD, na.rm = TRUE)

# (2) Future BAU1 years beyond sample
future_keys <- gdp_bau1 %>%
  dplyr::filter(TIME_PERIOD > max_hist_year) %>%
  dplyr::select(REF_AREA, TIME_PERIOD, GDP_BAU1) %>%
  dplyr::distinct()

# (3) Create future rows then inject GDP_BAU1
template <- clean_data[0, ]

future_rows <- template %>%
  dplyr::bind_rows(future_keys %>% dplyr::select(REF_AREA, TIME_PERIOD)) %>%
  dplyr::mutate(
    CAPB = NA_real_,
    GAP = NA_real_,
    NLGXQA = NA_real_,
    GDP_BAU1 = NA_real_
  ) %>%
  dplyr::left_join(
    future_keys,
    by = c("REF_AREA", "TIME_PERIOD"),
    suffix = c("", "_new")
  ) %>%
  dplyr::mutate(
    GDP_BAU1 = dplyr::coalesce(GDP_BAU1_new, GDP_BAU1)
  ) %>%
  dplyr::select(-dplyr::any_of("GDP_BAU1_new"))

# (4) Append + sort
clean_data <- dplyr::bind_rows(clean_data, future_rows) %>%
  dplyr::arrange(REF_AREA, TIME_PERIOD)

# 9 - Label Variables
attr(clean_data$CAPB, "label") <- "cyclically adjusted primary balance, OECD"
attr(clean_data$GAP, "label") <- "output gap as percentage of potential GDP, OECD"
attr(clean_data$NLGXQA, "label") <- "CAPB as percentage of potential GDP, OECD"
attr(clean_data$GDP_BAU1, "label") <- "OECD Nominal GDP Projections"

# 10 - Save dataset
write_csv(clean_data, "Data/Clean Data/oecd_capb.csv")