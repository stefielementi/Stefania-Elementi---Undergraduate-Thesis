# Script: OECD_fig4.R
# Purpose: replicate OECD Figure 4 using consolidation episodes in full_OECD_dataset.csv,
#          classifying episodes by the share of current expenditure cuts,
#          not by EB/TB dummies.
# Input:  Data/Clean Data/full_OECD_dataset.csv
# Output: Figures/OECD_fig4.png

# 1 - Load Libraries
library(tidyverse)
library(readr)
# Register Verdana on Windows
if (.Platform$OS.type == "windows") {
  windowsFonts(Verdana = windowsFont("Verdana"))
}

# 2 - File path
infile <- "Data/Clean Data/full_OECD_dataset.csv"

if (!file.exists(infile)) {
  stop("Input file not found: ", infile)
}

# 3 - Settings
start_sample <- 1978
end_sample   <- 2014

capb_var <- "NLGXQA"

# OECD Figure 4 excludes episodes undertaken in the absence of need for adjustment.
# This uses lagged NEED_ADJ at the start of the episode.
# If this removes all episodes in your dataset, the code automatically proceeds without it.
apply_need_filter <- TRUE

out_png <- "Figures/OECD_fig4.png"
out_csv <- "Figures/OECD_fig4.csv"

# 4 - Helper Functions
first_non_missing <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else x[1]
}

first_true_year <- function(flag, year) {
  yy <- year[which(flag)]
  if (length(yy) == 0) NA_integer_ else yy[1]
}

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else mean(x)
}

to_flag <- function(x) {
  if (is.logical(x)) return(replace_na(x, FALSE))
  if (is.numeric(x) || is.integer(x)) return(!is.na(x) & x != 0)
  
  x_chr <- trimws(tolower(as.character(x)))
  x_chr %in% c("1", "true", "t", "yes", "y")
}

assert_required_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }
}

value_at_year <- function(x, t, target_year) {
  if (is.na(target_year)) return(NA_real_)
  idx <- which(t == target_year & is.finite(x))
  if (length(idx) == 0) NA_real_ else x[idx[1]]
}

change_from_pre_start <- function(x, t, start_year, end_year) {
  if (is.na(start_year) || is.na(end_year)) return(NA_real_)
  
  x_pre <- value_at_year(x, t, start_year - 1L)
  x_end <- value_at_year(x, t, end_year)
  
  if (!is.finite(x_pre) || !is.finite(x_end)) return(NA_real_)
  
  x_end - x_pre
}

change_country <- function(data, country, var, start_year, end_year) {
  d <- data %>% filter(country_code == country)
  
  if (!(var %in% names(d))) return(NA_real_)
  
  change_from_pre_start(
    x = d[[var]],
    t = d$year,
    start_year = start_year,
    end_year = end_year
  )
}

classify_episode_oecd <- function(share_current_exp_cuts) {
  if (!is.finite(share_current_exp_cuts)) return(NA_character_)
  
  if (share_current_exp_cuts >= 0.5) {
    "Expenditure driven (1)"
  } else {
    "Revenue driven (1)"
  }
}

# 5 - Load Data
full_gen <- read_csv(infile, show_col_types = FALSE) %>%
  dplyr::select(-dplyr::any_of("...1"))

required_cols <- c(
  "country_code", "year", "episode_id",
  capb_var, "r_lo", "real_short_taylorgap",
  "start", "continue",
  "share_exp_cuts", "cum_currentexp_cut", "cum_dCAPB_episode",
  "NEED_ADJ"
)

assert_required_cols(full_gen, required_cols)

# 6 - Prepare Country-Year Dataset
df <- full_gen %>%
  mutate(
    country_code = as.character(country_code),
    year = as.integer(year),
    episode_id = as.integer(episode_id),
    
    CAPB_used = as.numeric(.data[[capb_var]]),
    r_lo = as.numeric(r_lo),
    real_short_taylorgap = as.numeric(real_short_taylorgap),
    
    share_exp_cuts = as.numeric(share_exp_cuts),
    cum_currentexp_cut = as.numeric(cum_currentexp_cut),
    cum_dCAPB_episode = as.numeric(cum_dCAPB_episode),
    NEED_ADJ = as.numeric(NEED_ADJ),
    
    EU_country = if ("EU_country" %in% names(.)) as.numeric(EU_country) else NA_real_,
    
    start_flag = to_flag(start),
    continue_flag = to_flag(continue),
    in_episode = start_flag | continue_flag
  ) %>%
  filter(year >= start_sample, year <= end_sample) %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    d_capb = CAPB_used - lag(CAPB_used),
    NEED_ADJ_lag = lag(NEED_ADJ)
  ) %>%
  ungroup()

# 7 - Build Benchmark Yields and Long-Term Spreads
benchmarks <- df %>%
  filter(country_code %in% c("DEU", "USA")) %>%
  select(year, country_code, r_lo) %>%
  mutate(
    bench = case_when(
      country_code == "DEU" ~ "bench_deu",
      country_code == "USA" ~ "bench_usa",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(bench)) %>%
  select(year, bench, r_lo) %>%
  pivot_wider(names_from = bench, values_from = r_lo)

european_countries <- c(
  "AUT", "BEL", "DNK", "ESP", "FIN", "FRA", "GBR",
  "DEU", "GRC", "IRL", "ITA", "LUX", "NLD", "PRT",
  "SWE", "CHE", "NOR", "ISL"
)

df <- df %>%
  left_join(benchmarks, by = "year") %>%
  mutate(
    is_europe = case_when(
      !is.na(EU_country) ~ EU_country == 1,
      country_code %in% european_countries ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # OECD Figure 4 uses long-term spreads:
    # European countries relative to Germany, non-European countries relative to the US.
    # US, Germany, Japan and UK episodes do not contribute to the long-term rate panel.
    spread_10y = case_when(
      country_code %in% c("USA", "DEU", "JPN", "GBR") ~ NA_real_,
      is_europe ~ r_lo - bench_deu,
      TRUE ~ r_lo - bench_usa
    )
  )

# 8 - Build Episode-Level Dataset
episode_rows <- df %>%
  filter(
    !is.na(episode_id),
    episode_id > 0,
    in_episode
  )

if (nrow(episode_rows) == 0) {
  stop("No consolidation episode rows found. Check episode_id, start and continue variables.")
}

episodes <- episode_rows %>%
  group_by(country_code, episode_id) %>%
  summarise(
    start_year = first_true_year(start_flag, year),
    end_year = max(year, na.rm = TRUE),
    
    d0 = value_at_year(d_capb, year, start_year),
    
    init_len = case_when(
      is.na(d0) ~ NA_integer_,
      d0 >= 1 ~ 1L,
      TRUE ~ 2L
    ),
    
    init_end_year = if_else(
      is.na(start_year) | is.na(init_len),
      NA_integer_,
      start_year + init_len - 1L
    ),
    
    need_initial = value_at_year(NEED_ADJ_lag, year, start_year),
    
    share_current_from_file = first_non_missing(share_exp_cuts),
    current_cut = first_non_missing(cum_currentexp_cut),
    cumulative_adjustment = first_non_missing(cum_dCAPB_episode),
    
    share_current_exp_cuts = case_when(
      is.finite(share_current_from_file) ~ share_current_from_file,
      is.finite(current_cut) &
        is.finite(cumulative_adjustment) &
        abs(cumulative_adjustment) > 1e-10 ~ current_cut / cumulative_adjustment,
      TRUE ~ NA_real_
    ),
    
    episode_type = classify_episode_oecd(share_current_exp_cuts),
    
    .groups = "drop"
  ) %>%
  filter(
    !is.na(start_year),
    !is.na(end_year),
    !is.na(init_end_year),
    !is.na(episode_type)
  )

if (nrow(episodes) == 0) {
  stop("No episodes could be classified using share_exp_cuts.")
}

# 9 - Apply OECD Need-for-Adjustment Filter
episodes_before_need_filter <- episodes

need_filter_used <- FALSE

if (apply_need_filter) {
  episodes_need <- episodes %>%
    filter(is.finite(need_initial), need_initial > 0)
  
  if (nrow(episodes_need) > 0) {
    episodes <- episodes_need
    need_filter_used <- TRUE
  } else {
    warning(
      "The positive lagged NEED_ADJ filter removed all episodes. ",
      "Proceeding without the NEED_ADJ filter so the figure can still be generated."
    )
    episodes <- episodes_before_need_filter
  }
}

# 10 - Compute Monetary Responses
episodes <- episodes %>%
  mutate(
    d_spread_initial = pmap_dbl(
      list(country_code, start_year, init_end_year),
      ~ change_country(df, ..1, "spread_10y", ..2, ..3)
    ),
    
    d_spread_whole = pmap_dbl(
      list(country_code, start_year, end_year),
      ~ change_country(df, ..1, "spread_10y", ..2, ..3)
    ),
    
    d_policy_initial = pmap_dbl(
      list(country_code, start_year, init_end_year),
      ~ change_country(df, ..1, "real_short_taylorgap", ..2, ..3)
    ),
    
    d_policy_whole = pmap_dbl(
      list(country_code, start_year, end_year),
      ~ change_country(df, ..1, "real_short_taylorgap", ..2, ..3)
    )
  )

# 11 - Build Plot Dataset
plot_df <- bind_rows(
  episodes %>%
    group_by(episode_type) %>%
    summarise(
      `Initial phase (4)` = safe_mean(d_spread_initial),
      `Whole episode (5)` = safe_mean(d_spread_whole),
      n_initial = sum(is.finite(d_spread_initial)),
      n_whole = sum(is.finite(d_spread_whole)),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = c(`Initial phase (4)`, `Whole episode (5)`),
      names_to = "phase",
      values_to = "mean_value"
    ) %>%
    mutate(panel = "Long-term interest rates (2)"),
  
  episodes %>%
    group_by(episode_type) %>%
    summarise(
      `Initial phase (4)` = safe_mean(d_policy_initial),
      `Whole episode (5)` = safe_mean(d_policy_whole),
      n_initial = sum(is.finite(d_policy_initial)),
      n_whole = sum(is.finite(d_policy_whole)),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = c(`Initial phase (4)`, `Whole episode (5)`),
      names_to = "phase",
      values_to = "mean_value"
    ) %>%
    mutate(panel = "Policy rates (3)")
) %>%
  filter(!is.na(mean_value)) %>%
  mutate(
    episode_type = factor(
      episode_type,
      levels = c("Expenditure driven (1)", "Revenue driven (1)")
    ),
    
    phase = factor(
      phase,
      levels = c("Initial phase (4)", "Whole episode (5)")
    ),
    
    panel = factor(
      panel,
      levels = c("Long-term interest rates (2)", "Policy rates (3)")
    ),
    
    x_pos = case_when(
      panel == "Long-term interest rates (2)" & phase == "Initial phase (4)" ~ 1,
      panel == "Long-term interest rates (2)" & phase == "Whole episode (5)" ~ 2,
      panel == "Policy rates (3)" & phase == "Initial phase (4)" ~ 4,
      panel == "Policy rates (3)" & phase == "Whole episode (5)" ~ 5,
      TRUE ~ NA_real_
    )
  )

if (nrow(plot_df) == 0) {
  stop("No usable values available to plot Figure 4.")
}

# 12 - Save Episode-Level Output for Diagnostics
dir.create("Figures", showWarnings = FALSE)

write_csv(
  episodes %>%
    select(
      country_code, episode_id, start_year, end_year, init_end_year,
      need_initial, share_current_exp_cuts, episode_type,
      d_spread_initial, d_spread_whole,
      d_policy_initial, d_policy_whole
    ),
  out_csv
)

# 13 - Caption Note
need_note <- if (need_filter_used) {
  "Episodes with non-positive lagged need for adjustment are excluded."
} else {
  "The lagged need-for-adjustment filter is not applied because it removed all episodes in this dataset."
}

# 14 - Plot
p <- ggplot(plot_df, aes(x = x_pos, y = mean_value, fill = episode_type)) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "grey35") +
  
  geom_col(
    position = position_dodge(width = 0.55),
    width = 0.28,
    color = "black",
    linewidth = 0.7
  ) +
  
  annotate(
    "text",
    x = 1.5,
    y = 1.8,
    label = "Long-term interest rates (2)",
    family = "Verdana",
    fontface = "bold",
    size = 12 / ggplot2::.pt
  ) +
  
  annotate(
    "text",
    x = 4.5,
    y = 1.8,
    label = "Policy rates (3)",
    family = "Verdana",
    fontface = "bold",
    size = 12 / ggplot2::.pt
  ) +
  
  scale_fill_manual(
    values = c(
      "Expenditure driven (1)" = "white",
      "Revenue driven (1)" = "grey45"
    ),
    breaks = c("Expenditure driven (1)", "Revenue driven (1)"),
    name = NULL
  ) +
  
  scale_x_continuous(
    limits = c(0.2, 5.8),
    breaks = c(1, 2, 4, 5),
    labels = c(
      "Initial\nphase (4)",
      "Whole\nepisode (5)",
      "Initial\nphase (4)",
      "Whole\nepisode (5)"
    )
  ) +
  
  scale_y_continuous(
    breaks = seq(-2.5, 2.0, by = 0.5),
    labels = function(x) sprintf("%.1f", x)
  ) +
  
  coord_cartesian(ylim = c(-2.5, 2.0)) +
  
  labs(
    title = "Response of long-term interest rates and policy rates during CAPB-based episodes of fiscal consolidation",
    x = NULL,
    y = NULL,
    caption = paste0(
      "1. Episodes are labelled as expenditure driven if cuts in current expenditure account for half of the overall adjustment or more; revenue driven otherwise. ",
      need_note, "\n",
      "2. Changes in long-term interest rates are measured by changes in the spread of the 10-year government bond yield relative to the US yield\n",
      "   for non-European countries and the German yield for European countries. US, Germany, Japan and UK episodes do not contribute to this panel.\n",
      "3. Policy rates are measured as changes in the real short-term Taylor-gap indicator.\n",
      "4. Initial phase = first 1 year if the start-year CAPB increase is at least 1% of GDP, otherwise first 2 years.\n",
      "5. Whole episode = cumulative change from the year before the episode starts to the final year of the episode.\n"
    )
  ) +
  
  theme_minimal(base_size = 12, base_family = "Verdana") +
  theme(
    text = element_text(family = "Verdana", size = 12),
    
    panel.background = element_rect(fill = "white", color = "grey55"),
    plot.background = element_rect(fill = "white", color = NA),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey86", linewidth = 0.5),
    
    axis.text.x = element_text(
      size = 12,
      face = "bold",
      color = "black",
      margin = margin(t = 14)
    ),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.ticks = element_line(color = "grey35"),
    
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(size = 12, face = "bold"),
    
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 20)
    ),
    
    plot.caption = element_text(
      size = 10,
      hjust = 0,
      lineheight = 1.15,
      margin = margin(t = 18)
    ),
    plot.caption.position = "plot",
    
    plot.margin = margin(t = 20, r = 24, b = 28, l = 24)
  )

ggsave(
  filename = out_png,
  plot = p,
  width = 16,
  height = 11,
  dpi = 300
)

print(p)

cat("Wrote: ", out_png, "\n", sep = "")
cat("Wrote: ", out_csv, "\n", sep = "")