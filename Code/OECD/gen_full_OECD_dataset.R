# Script: gen_full_OECD_dataset.r
# Purpose: generate full OECD dataset using data from exogenous dataset, just merging episode column
# Input: Data/Clean Data/episode_OECD.csv
# Output: Data/Clean Data/full_OECD_dataset.csv

# 1 - Load Libraries
library(tidyverse)
library(dplyr)
library(purrr)
library(readr)

# ----------------------------------------------------------------------------- #
# MERGE IDENTIFIED EPISODE FROM OECD METHOD TO ALL OTHER VARIABLES FROM EXOGENOUS DATASET)#
# A - Load data
raw_full <- read_csv("Data/Clean Data/raw_full.csv", show_col_types = FALSE)
episode_OECD <- read_csv("Data/Clean Data/episode_OECD.csv", show_col_types = FALSE)

# B - Keep only the needed OECD variable and harmonise key names
episode_start <- episode_OECD %>%
  transmute(
    country_code = REF_AREA,
    year         = TIME_PERIOD,
    start_OECD   = START_raw
  ) %>%
  filter(year >= 1978, year <= 2024) %>%
  distinct(country_code, year, .keep_all = TRUE)

# C - Merge into raw_full
raw_full_merged <- raw_full %>%
  left_join(episode_start, by = c("country_code", "year"))

# D - Save result
write_csv(raw_full_merged, "Data/Clean Data/intermediate_OECD_merged.csv")
# ----------------------------------------------------------------------------- #


# 2 - Load Data
raw_full <- read.csv(
  "Data/Clean Data/intermediate_OECD_merged.csv",
  check.names = FALSE
) %>%
  arrange(country_code, year)

# 3 - Construct NEED_ADJ
# ---- (a) Solver: constant cyclically adjusted overall balance (decimal)
#      that drives debt to ~0 in 30 years
target_CAOB <- function(b0, g_vec) {
  debt_end <- function(ob) {
    b <- b0
    for (k in seq_along(g_vec)) {
      b <- (b - ob) / (1 + g_vec[k])
    }
    b
  }
  
  tryCatch(
    uniroot(debt_end, lower = -2, upper = 2)$root,
    error = function(e) NA_real_
  )
}

raw_full_MOD <- raw_full %>%
  arrange(country_code, year) %>%
  mutate(
    CAPB     = as.numeric(CAPB),      # nominal CAPB level (OECD NLGXA)
    gdp      = as.numeric(gdp),       # nominal GDP, market prices
    GDP_BAU1 = as.numeric(GDP_BAU1),  # projected nominal GDP
    debta    = as.numeric(debta),     # debt-to-GDP ratio
    netint   = as.numeric(netint)     # net interest payments, % of GDP
  ) %>%
  group_by(country_code) %>%
  mutate(
    # ---- (b) Current cyclically adjusted primary balance in % of GDP
    CAPB_CurrentGDP = 100 * (CAPB / gdp),
    
    # ---- (c) Cyclically adjusted overall balance in % of GDP
    # netint is already expressed as % of GDP
    CAOB = CAPB_CurrentGDP - netint,
    
    # ---- (d) Nominal GDP path: realized GDP, extended with BAU1 projections
    GDP_path = coalesce(gdp, GDP_BAU1),
    g_nom_path = (GDP_path / lag(GDP_path)) - 1,
    
    # ---- (e) Initial debt ratio in decimals
    b0 = debta / 100
  ) %>%
  mutate(
    # ---- (f) Target CAOB (% of GDP) using next 30 growth rates
    TARGET_CAOB = map_dbl(row_number(), function(i) {
      g30 <- g_nom_path[(i + 1):(i + 30)]
      if (length(g30) < 30 || any(is.na(c(b0[i], g30)))) return(NA_real_)
      100 * target_CAOB(b0[i], g30)
    }),
    
    # ---- (g) Need for adjustment (% of GDP)
    NEED_ADJ = TARGET_CAOB - CAOB
  ) %>%
  ungroup() %>%
  filter(year <= 2014)

# 4 - Construct Episode Dummies
raw_full_MOD <- raw_full_MOD %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    # AFG plan indicator
    new_plan = tidyr::replace_na(as.numeric(`start_OECD`), 0),
    
    # CAPB as % of potential GDP 
    CAPB_pot  = as.numeric(NLGXQA),
    dCAPB_pot = CAPB_pot - lag(CAPB_pot, 1),
    
    # Start year = first year of a new AFG plan
    start = as.integer(new_plan == 1 & lag(new_plan, 1, default = 0) == 0)
  ) %>%
  mutate(
    # Episode stays active after START as long as CAPB keeps increasing
    episode = {
      in_ep <- rep(FALSE, n())
      for (i in seq_along(in_ep)) {
        if (i == 1) {
          if (isTRUE(start[i] == 1)) in_ep[i] <- TRUE
        } else {
          if (!in_ep[i - 1] && isTRUE(start[i] == 1)) {
            in_ep[i] <- TRUE
          } else if (in_ep[i - 1] && !is.na(dCAPB_pot[i]) && dCAPB_pot[i] > 0) {
            in_ep[i] <- TRUE
          }
        }
      }
      in_ep
    },
    
    # Continue = 1 only after the START year, for as long as CAPB keeps rising
    continue = as.integer(episode & lag(episode, 1, default = FALSE) & start == 0),
    
    # Serious = 1 if, in addition to an initial 1 pp CAPB increase
    # (either in one year or over two consecutive years with at least 0.5 in the first),
    # there is an additional adjustment of at least 1 pp
    serious = as.integer(
      start == 1 & (
        # Case A: initial 1 pp increase occurs in the start year
        (
          !is.na(dCAPB_pot) &
            dCAPB_pot >= 1 &
            !is.na(lead(dCAPB_pot, 1)) &
            !is.na(lead(dCAPB_pot, 2)) &
            (lead(dCAPB_pot, 1) + lead(dCAPB_pot, 2)) >= 1
        ) |
          # Case B: initial 1 pp increase occurs over start year + next year
          (
            !is.na(dCAPB_pot) &
              !is.na(lead(dCAPB_pot, 1)) &
              dCAPB_pot >= 0.5 &
              lead(dCAPB_pot, 1) >= 0.5 &
              (dCAPB_pot + lead(dCAPB_pot, 1)) >= 1 &
              !is.na(lead(dCAPB_pot, 2)) &
              !is.na(lead(dCAPB_pot, 3)) &
              (lead(dCAPB_pot, 2) + lead(dCAPB_pot, 3)) >= 1
          )
      )
    )
  ) %>%
  ungroup() %>%
  select(-new_plan, -CAPB_pot, -dCAPB_pot, -episode)

# 5 - Construct Episode-Level Outcome Variables and Monetary Condition Variable
pi_target <- 2

raw_full_MOD <- raw_full_MOD %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    # CAPB as % of potential GDP (same scale as old OECD-style episode code)
    CAPB_pot = as.numeric(NLGXQA),
    
    # Reconstruct full episode indicator from start/continue
    episode = (start == 1 | continue == 1),
    
    # Episode id and end of episode
    episode_id = if_else(episode, cumsum(start), NA_integer_),
    END = as.integer(episode & !lead(episode, 1, default = FALSE))
  ) %>%
  ungroup()

# (1) Cumulative change in CAPB over whole adjustment episode
raw_full_MOD <- raw_full_MOD %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    # CAPB in the year before the START year
    CAPB_pre_start_at_startyear = if_else(start == 1, lag(CAPB_pot, 1), NA_real_)
  ) %>%
  group_by(country_code, episode_id) %>%
  mutate(
    # Episode-specific pre-start CAPB
    CAPB_pre_start_ep = if_else(
      !is.na(episode_id),
      dplyr::first(na.omit(CAPB_pre_start_at_startyear), default = NA_real_),
      NA_real_
    ),
    
    # CAPB at end of episode
    CAPB_end_ep = if_else(
      !is.na(episode_id),
      dplyr::last(CAPB_pot, default = NA_real_),
      NA_real_
    ),
    
    # Cumulative adjustment achieved during the episode
    cum_dCAPB_episode = if_else(
      !is.na(episode_id),
      CAPB_end_ep - CAPB_pre_start_ep,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-CAPB_pre_start_at_startyear)

# (2) Deceleration in the pace of debt accumulation from consolidation episode
raw_full_MOD <- raw_full_MOD %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    debt = as.numeric(debta),
    dDebt = debt - lag(debt, 1),
    
    # 2-year pre-start average change in debt
    pre_avg_at_start = if_else(
      start == 1,
      (lag(dDebt, 1) + lag(dDebt, 2)) / 2,
      NA_real_
    ),
    
    # 2-year post-end average change in debt
    post_avg_at_end = if_else(
      END == 1,
      (lead(dDebt, 1) + lead(dDebt, 2)) / 2,
      NA_real_
    )
  ) %>%
  group_by(country_code, episode_id) %>%
  mutate(
    pre_avg_2y = if_else(
      !is.na(episode_id),
      dplyr::first(na.omit(pre_avg_at_start), default = NA_real_),
      NA_real_
    ),
    post_avg_2y = if_else(
      !is.na(episode_id),
      dplyr::first(na.omit(post_avg_at_end), default = NA_real_),
      NA_real_
    ),
    
    # Positive value = debt accumulation slowed after the episode
    decel_debt = if_else(
      !is.na(episode_id),
      pre_avg_2y - post_avg_2y,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-debt, -dDebt, -pre_avg_at_start, -post_avg_at_end)

# (3) Cumulative length of adjustment episode
raw_full_MOD <- raw_full_MOD %>%
  arrange(country_code, year) %>%
  group_by(country_code, episode_id) %>%
  mutate(
    episode_length = if_else(!is.na(episode_id), dplyr::n(), NA_integer_)
  ) %>%
  ungroup()

# (4) Cumulative change in CAPB in 2 years after consolidation episode
raw_full_MOD <- raw_full_MOD %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    post2_cum_at_end = if_else(
      END == 1,
      lead(CAPB_pot, 2) - CAPB_pot,
      NA_real_
    )
  ) %>%
  group_by(country_code, episode_id) %>%
  mutate(
    post2_cum_dCAPB = if_else(
      !is.na(episode_id),
      dplyr::first(na.omit(post2_cum_at_end), default = NA_real_),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-post2_cum_at_end)

# (5) Change in real short rate relative to Taylor rule
raw_full_MOD <- raw_full_MOD %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    # Inflation rate from CPI index (annual % change)
    INF = 100 * ((as.numeric(CPI) / lag(as.numeric(CPI), 1)) - 1),
    
    # Taylor-rule implied real short rate
    taylor_real = 0.5 * (INF - pi_target) + 0.5 * as.numeric(GAP),
    
    # Real short rate relative to Taylor rule
    real_short_taylorgap = as.numeric(r_sh) - taylor_real,
    
    # 1-year change
    d_real_short_taylorgap = real_short_taylorgap - lag(real_short_taylorgap, 1)
  ) %>%
  ungroup() %>%
  select(-CAPB_pot, -episode)

# 6 - Construct Share of Expenditure Cuts and Share of Capital Expenditure Cuts
raw_full_MOD <- raw_full_MOD %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    # Current / non-capital expenditure as % of potential GDP
    currentexp_pot = 100 * (as.numeric(currentdisb) / as.numeric(gdptr)),
    
    # Government capital expenditure as % of potential GDP
    capex_pot = 100 * (as.numeric(igaa) / as.numeric(gdptr)),
    
    # Pre-start levels tagged in the START year
    currentexp_pre_at_start = if_else(start == 1, lag(currentexp_pot, 1), NA_real_),
    capex_pre_at_start      = if_else(start == 1, lag(capex_pot, 1),      NA_real_)
  ) %>%
  group_by(country_code, episode_id) %>%
  mutate(
    # Episode-specific pre-start levels
    currentexp_pre_start = if_else(
      !is.na(episode_id),
      dplyr::first(na.omit(currentexp_pre_at_start), default = NA_real_),
      NA_real_
    ),
    capex_pre_start = if_else(
      !is.na(episode_id),
      dplyr::first(na.omit(capex_pre_at_start), default = NA_real_),
      NA_real_
    ),
    
    # Episode end levels
    currentexp_end = if_else(!is.na(episode_id), dplyr::last(currentexp_pot), NA_real_),
    capex_end      = if_else(!is.na(episode_id), dplyr::last(capex_pot),      NA_real_),
    
    # Cumulative cuts during the episode
    cum_currentexp_cut = if_else(
      !is.na(episode_id),
      currentexp_pre_start - currentexp_end,
      NA_real_
    ),
    cum_capex_cut = if_else(
      !is.na(episode_id),
      capex_pre_start - capex_end,
      NA_real_
    ),
    
    # Shares in total adjustment
    share_exp_cuts = if_else(
      !is.na(episode_id) & !is.na(cum_dCAPB_episode) & cum_dCAPB_episode > 0,
      cum_currentexp_cut / cum_dCAPB_episode,
      NA_real_
    ),
    share_capex_cuts = if_else(
      !is.na(episode_id) & !is.na(cum_dCAPB_episode) & cum_dCAPB_episode > 0,
      cum_capex_cut / cum_dCAPB_episode,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-currentexp_pre_at_start, -capex_pre_at_start)

# 7 - Additional Variables
raw_full_MOD <- raw_full_MOD %>%
  mutate(
    EU_country = as.integer(country_code %in% c(
      "AUT", "BEL", "DEU", "DNK", "ESP", "FIN",
      "FRA", "GBR", "IRL", "ITA", "PRT", "SWE"
    )),
    
    EA_country = as.integer(country_code %in% c(
      "AUT", "BEL", "DEU", "ESP", "FIN",
      "FRA", "IRL", "ITA", "PRT"
    )),
    
    EA_pre1992 = as.integer(
      country_code %in% c(
        "AUT", "BEL", "DEU", "ESP", "FIN",
        "FRA", "IRL", "ITA", "PRT"
      ) & year <= 1991
    ),
    
    EA_qual_9297 = as.integer(
      country_code %in% c(
        "AUT", "BEL", "DEU", "ESP", "FIN",
        "FRA", "IRL", "ITA", "PRT"
      ) & year >= 1992 & year <= 1997
    ),
    
    EA_1998on = as.integer(
      country_code %in% c(
        "AUT", "BEL", "DEU", "ESP", "FIN",
        "FRA", "IRL", "ITA", "PRT"
      ) & year >= 1998
    )
  )

# 8 - Save Output
write.csv(raw_full_MOD, "Data/Clean Data/full_OECD_dataset.csv", row.names = FALSE)