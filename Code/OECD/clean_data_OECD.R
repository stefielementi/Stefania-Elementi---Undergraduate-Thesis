#Script: clean_data_OECD.r
#Purpose: clean OECD raw data, reshape to one row country-year
#Input: Data/Raw Data/OECD.ECO.MAD,DSD_EO@DF_EO,+..A.csv
#Output: Data/Clean Data/clean_data_OECD.csv

#1 - Load Libraries
library(readr)
library(dplyr)
library(tidyr)
library(conflicted)

conflicts_prefer(
  dplyr::select,
  dplyr::filter,
  dplyr::mutate,
  dplyr::arrange,
  dplyr::transmute,
  dplyr::distinct,
  dplyr::left_join,
  dplyr::bind_rows,
  dplyr::coalesce
)

#2 - Load Raw Data 
raw_data <- read_csv("Data/Raw Data/OECD.ECO.MAD,DSD_EO@DF_EO,+..A.csv",show_col_types = FALSE)

#3 - Select Countries 
countries <- c("AUS","AUT","BEL","CAN","DEU","DNK","ESP","FIN","FRA","GBR","IRL","ITA","JPN","PRT","SWE","USA")

#4 - Select Variables 
variables_keep <- c("NLGXQA","GDPTR","GDP","YRGT","YPGT","SAVG","GNINTP","GGINTP","YRGTQ","YPGTQ","GNINTQ","PCORE","PCOREH","PCOREH_YTYPCT","GAP","IRCB","IRS","PCORE_YTYPCT","IRL","UNR","EXCH","GDPVTR","EXCHER","NLGXQ","YRGA","IGV","GGFLMQ","NLGXA")

#5 - Select Columns (from raw dataset)
columns_keep <- c("REF_AREA","MEASURE","TIME_PERIOD","OBS_VALUE")

#6 - Keep Variables and Columns within Time Period (1960-2024)
filtered_raw_data <- raw_data |>
  select(any_of(columns_keep)) |>
  filter(
    REF_AREA %in% countries,
    MEASURE %in% variables_keep,
  ) |>
  mutate(
    TIME_PERIOD = as.integer(TIME_PERIOD)
  ) |>
  filter(TIME_PERIOD >= 1960, TIME_PERIOD <= 2024)

#7 - Pivot to one row per country-year, one column per variable
clean_data <- filtered_raw_data |>
  tidyr::pivot_wider(
    names_from = MEASURE,
    values_from = OBS_VALUE
  ) |>
  arrange(REF_AREA, TIME_PERIOD)

#8 - Add GDP Growth Projections (Needed for NEED_ADJ)
projections_data <- read_csv(
  "Data/Raw Data/OECD.ECO.MAD,DSD_EO_LTB@DF_EO_LTB,+all.csv",
  show_col_types = FALSE
)
countries_in_clean <- unique(clean_data$REF_AREA)
# Extract GDP under BAU1
gdp_bau1 <- projections_data %>%
  select(REF_AREA, MEASURE, SCENARIO, TIME_PERIOD, OBS_VALUE) %>%
  mutate(TIME_PERIOD = as.integer(TIME_PERIOD)) %>%
  filter(
    REF_AREA %in% countries_in_clean,  
    MEASURE == "GDP",
    SCENARIO == "BAU1"
  ) %>%
  transmute(
    REF_AREA,
    TIME_PERIOD,
    GDP_BAU1 = as.numeric(OBS_VALUE)
  ) %>%
  distinct(REF_AREA, TIME_PERIOD, .keep_all = TRUE)
# Merge into clean_data 
# (1) Merge GDP_BAU1 onto existing years
clean_data <- clean_data %>%
  left_join(gdp_bau1, by = c("REF_AREA", "TIME_PERIOD"))
max_hist_year <- max(clean_data$TIME_PERIOD, na.rm = TRUE)
# (2) Future BAU1 years beyond sample
future_keys <- gdp_bau1 %>%
  filter(TIME_PERIOD > max_hist_year) %>%
  select(REF_AREA, TIME_PERIOD, GDP_BAU1) %>%
  distinct()
# (3) Create future rows then inject GDP_BAU1
template <- clean_data[0, ]  # 0-row tibble with correct columns

future_rows <- template %>%
  bind_rows(future_keys %>% select(REF_AREA, TIME_PERIOD)) %>%
  mutate(GDP_BAU1 = NA_real_) %>%
  
  left_join(future_keys, by = c("REF_AREA", "TIME_PERIOD"), suffix = c("", "_new")) %>%
  mutate(GDP_BAU1 = coalesce(GDP_BAU1_new, GDP_BAU1)) %>%
  select(-any_of("GDP_BAU1_new"))
# (4) Append + sort
clean_data <- bind_rows(clean_data, future_rows) %>%
  arrange(REF_AREA, TIME_PERIOD)

#9 - Save clean_data.csv
write_csv(clean_data,"Data/Clean Data/clean_data_OECD.csv")