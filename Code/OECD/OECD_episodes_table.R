# Script: OECD_episodes_table.R
# Purpose: create a LaTeX table of fiscal adjustment episodes
# Input: Data/Clean Data/full_OECD_dataset.csv
# Output: Output/OECD_episodes_table.tex

library(readr)
library(dplyr)

# 1 - Load data
df <- read_csv("Data/Clean Data/full_OECD_dataset.csv", show_col_types = FALSE)

# 2 - Country names
country_map <- c(
  AUS = "Australia",
  AUT = "Austria",
  BEL = "Belgium",
  CAN = "Canada",
  DNK = "Denmark",
  FIN = "Finland",
  FRA = "France",
  DEU = "Germany",
  IRL = "Ireland",
  ITA = "Italy",
  JPN = "Japan",
  PRT = "Portugal",
  ESP = "Spain",
  SWE = "Sweden",
  GBR = "United Kingdom",
  USA = "United States"
)

# 3 - Helper functions
fmt_yy <- function(y) sprintf("%02d", y %% 100)

fmt_range <- function(start_year, end_year) {
  if (start_year == end_year) {
    fmt_yy(start_year)
  } else {
    paste0(fmt_yy(start_year), "--", fmt_yy(end_year))
  }
}

# 4 - Keep only episode years
ep_df <- df %>%
  filter(!is.na(episode_id), episode_id != 0) %>%
  arrange(country_code, year)

# 5 - Build episode periods by country
periods_df <- ep_df %>%
  group_by(country_code, episode_id) %>%
  summarise(
    start_year = min(year, na.rm = TRUE),
    end_year   = max(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(period_piece = mapply(fmt_range, start_year, end_year)) %>%
  group_by(country_code) %>%
  summarise(
    Period = paste(period_piece, collapse = ", "),
    .groups = "drop"
  )

# 6 - Count episodes and years
counts_df <- ep_df %>%
  group_by(country_code) %>%
  summarise(
    no_episodes = n_distinct(episode_id),
    no_years    = n(),
    .groups = "drop"
  )

# 7 - Build final country table
table_df <- counts_df %>%
  left_join(periods_df, by = "country_code") %>%
  mutate(
    Country = recode(country_code, !!!country_map, .default = country_code)
  ) %>%
  select(Country, Period, no_episodes, no_years) %>%
  arrange(Country)

# 8 - Common sample period for header
sample_start <- min(df$year, na.rm = TRUE)
sample_end   <- max(df$year, na.rm = TRUE)
header_label <- paste0("Country (", sample_start, "--", sample_end, ")")

# 9 - Total row values
n_countries <- nrow(table_df)
total_episodes <- sum(table_df$no_episodes, na.rm = TRUE)
total_years    <- sum(table_df$no_years, na.rm = TRUE)

# 10 - Create LaTeX rows
body_rows <- apply(table_df, 1, function(x) {
  paste0(
    x["Country"], " & ",
    x["Period"], " & ",
    x["no_episodes"], " & ",
    x["no_years"], " \\\\"
  )
})

total_row <- paste0(
  "\\textit{", n_countries, " countries} & ",
  "\\textit{} & ",
  "\\textit{\\textbf{", total_episodes, "}} & ",
  "\\textit{\\textbf{", total_years, "}} \\\\"
)

# 11 - Write LaTeX table manually
latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Episodes of fiscal adjustment (OECD Identification)}",
  "\\small",
  "\\renewcommand{\\arraystretch}{1.15}",
  "\\setlength{\\tabcolsep}{6pt}",
  "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}} l l r r}",
  "\\toprule",
  paste0(header_label, " & Period & No. episodes & No. years \\\\"),
  "\\midrule",
  body_rows,
  "\\midrule",
  total_row,
  "\\bottomrule",
  "\\end{tabular*}",
  "\\end{table}"
)

dir.create("Output", showWarnings = FALSE)
writeLines(latex_lines, "Output/OECD_episodes_table.tex")