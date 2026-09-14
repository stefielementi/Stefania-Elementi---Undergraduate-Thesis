#Script: gen_episode_OECD.r
#Purpose: generate baseline OECD Episode Dataset to then Merge with Exogenous 
#Input: Data/Clean Data/clean_data_OECD.csv
#Output: Data/Clean Data/episode_OECD.csv

#1 - Load Libraries 
library(tidyverse)
library(dplyr)
library(purrr)

#2 - Load Clean Data
clean_data <- read.csv("Data/Clean Data/clean_data_OECD.csv") %>%
  arrange(REF_AREA, TIME_PERIOD)

# #3 - Construct Euro Area (11) Binary Indicator
# euro_area_11_countries <- c("AUT","BEL","FIN","FRA","DEU","IRL","ITA","LUX","NLD","PRT","ESP")
# clean_data_MOD <- clean_data %>%
#   mutate(euroarea_11 = as.integer(REF_AREA %in% euro_area_11_countries))

# #4 - Construct 10y Spread
# clean_data_MOD <- clean_data_MOD %>%
#   group_by(TIME_PERIOD)%>%
#   mutate(
#     spread_10y = {
#       ger <- IRL[REF_AREA == "DEU"][1]
#       dplyr::if_else(is.na(IRL) | is.na(ger), NA_real_, IRL - ger)
#     }
#   )%>%
#   ungroup()

# #5 - Construct NEED ADJ
# # ---- (a) Solver: constant overall balance (decimal) that drives debt to ~0 in 30y
# target_CAOB <- function(b0, g_vec){
#   debt_end <- function(ob){
#     b <- b0
#     for (k in seq_along(g_vec)) b <- (b - ob) / (1 + g_vec[k])
#     b
#   }
#   tryCatch(uniroot(debt_end, lower = -2, upper = 2)$root,
#            error = function(e) NA_real_)
# }
# 
# clean_data_MOD <- clean_data_MOD %>%
#   arrange(REF_AREA, TIME_PERIOD) %>%
#   group_by(REF_AREA) %>%
#   mutate(
#     # ---- (b) Current CAOB in pp of GDP (uses actual fiscal-year GDP)
#     CAPB_CurrentGDP = 100 * (NLGXA / GDP),
#     CAOB = CAPB_CurrentGDP - GNINTQ,
#     
#     # ---- (c) Nominal GDP path: realised GDP, extended with BAU1
#     GDP_path = coalesce(GDP, GDP_BAU1),
#     g_nom_path = (GDP_path / lag(GDP_path)) - 1,
#     
#     # ---- (d) Debt ratio in decimals
#     b0 = GGFLMQ / 100
#   ) %>%
#   mutate(
#     # ---- (e) Target CAOB (pp of GDP) using next 30 growth rates
#     TARGET_CAOB = map_dbl(row_number(), function(i){
#       g30 <- g_nom_path[(i + 1):(i + 30)]
#       if (length(g30) < 30 || any(is.na(c(b0[i], g30)))) return(NA_real_)
#       100 * target_CAOB(b0[i], g30)
#     }),
#     
#     # ---- (f) Need for adjustment (pp of GDP)
#     NEED_ADJ = TARGET_CAOB - CAOB
#   ) %>%
#   ungroup()

clean_data_MOD <- clean_data %>% filter(TIME_PERIOD <= 2024) #remove future rows (no longer needed)

# #6 - Construct Interest Rate Variables (and lagged inflation)
# clean_data_MOD <- clean_data_MOD %>%
#   arrange(REF_AREA, TIME_PERIOD) %>%
#   group_by(REF_AREA) %>%
#   mutate(
#     #Inflation series
#     INF = coalesce(PCOREH_YTYPCT, PCORE_YTYPCT),
#     #Nominal levels
#     IRS_nom = as.numeric(IRS),
#     IRL_nom = as.numeric(IRL),
#     #Real levels (ex-post approximation)
#     IRS_real = IRS_nom - INF,
#     IRL_real = IRL_nom - INF,
#     #1-year changes
#     dIRS      = IRS_nom  - lag(IRS_nom, 1),
#     dIRL      = IRL_nom  - lag(IRL_nom, 1),
#     dIRS_real = IRS_real - lag(IRS_real, 1),
#     dIRL_real = IRL_real - lag(IRL_real, 1),
#     #1-year lags levels
#     IRS_L1      = lag(IRS_nom, 1),
#     IRL_L1      = lag(IRL_nom, 1),
#     IRS_real_L1 = lag(IRS_real, 1),
#     IRL_real_L1 = lag(IRL_real, 1),
#     #1-year lags changes
#     dIRS_L1      = lag(dIRS, 1),
#     dIRL_L1      = lag(dIRL, 1),
#     dIRS_real_L1 = lag(dIRS_real, 1),
#     dIRL_real_L1 = lag(dIRL_real, 1),
#     #Lagged inflation 
#     INF_L1 = lag(INF, 1),
#     #Other Key Lags 
#     NEED_ADJ_L1 = lag(NEED_ADJ, 1),
#     GAP_L1      = lag(GAP, 1),
#     spread_10y_L1  = lag(spread_10y, 1)
#   ) %>%
#   ungroup()
# 
# #7 - Construct lagged-REER
# clean_data_MOD <- clean_data_MOD %>%
#   arrange(REF_AREA, TIME_PERIOD) %>%
#   group_by(REF_AREA) %>%
#   mutate(
#     #REER Level
#     REER = as.numeric(EXCHER),
#     
#     #REER 1-year change
#     dREER = REER - lag(REER, 1),
#     
#     #REER 1-year lags
#     REER_L1  = lag(REER, 1),
#     dREER_L1 = lag(dREER, 1)
#   ) %>%
#   ungroup()

#8 - Construct Episode Definitions and Other Dependent Variables
#(1) Start of Consolidation Episode
clean_data_MOD <- clean_data_MOD%>%
  arrange(REF_AREA, TIME_PERIOD) %>%
  group_by(REF_AREA) %>%
  mutate(
    CAPB  = as.numeric(NLGXQA), #CAPB stored as %potential GDP
    dCAPB = CAPB - lag(CAPB, 1),
    START_raw = as.integer(
      (dCAPB >= 1) |
        (dCAPB >= 0.5 & lead(dCAPB, 1) >= 0.5 & (dCAPB + lead(dCAPB, 1)) >= 1)
    )
  ) %>%
  ungroup()
# #(2) Continuation of Ongoing Episode
# clean_data_MOD <- clean_data_MOD %>%
#   arrange(REF_AREA, TIME_PERIOD) %>%
#   group_by(REF_AREA) %>%
#   mutate(
#     # "episode" state: starts when START==1, then continues while CAPB keeps increasing
#     episode = {
#       in_ep <- rep(FALSE, n())
#       for (i in seq_along(in_ep)) {
#         if (i == 1) {
#           if (isTRUE(START_raw[i] == 1)) in_ep[i] <- TRUE
#         } else {
#           if (!in_ep[i - 1] && isTRUE(START_raw[i] == 1)) {
#             in_ep[i] <- TRUE
#           } else if (in_ep[i - 1] && !is.na(dCAPB[i]) && dCAPB[i] > 0) {
#             in_ep[i] <- TRUE
#           }
#         }
#       }
#       in_ep
#     },
#     START = as.integer(episode & !lag(episode, 1, default = FALSE)),
#     # Continuation of an ongoing episode (this year is in episode AND last year was in episode)
#     CONTINUE = as.integer(episode == TRUE & lead(episode, 1) == TRUE)
#   ) %>%
#   ungroup() %>%
#   select(-START_raw)
# 
# clean_data_MOD <- clean_data_MOD %>%
#   arrange(REF_AREA, TIME_PERIOD) %>%
#   group_by(REF_AREA) %>%
#   mutate(
#     episode_id = if_else(episode, cumsum(START), NA_integer_),
#     END = as.integer(episode & !lead(episode, 1, default = FALSE))
#   ) %>%
#   ungroup()
# 
# #(3) Seriously Pursued 
# clean_data_MOD <- clean_data_MOD %>%
#   arrange(REF_AREA, TIME_PERIOD) %>%
#   group_by(REF_AREA) %>%
#   mutate(
#     serious_start = if_else(
#       START == 1,
#       as.integer(lead(CAPB, 2) - CAPB >= 1),
#       NA_integer_
#     )
#   ) %>%
#   ungroup()
# #(4) Change in CAPB
# #constructed as part of step (1)
# #(5) Cumulative change in CAPB over a whole adjustment episode
# clean_data_MOD <- clean_data_MOD %>%
#   arrange(REF_AREA, TIME_PERIOD) %>%
#   group_by(REF_AREA) %>%
#   mutate(
#     # CAPB in year before the START year (computed on full country series)
#     CAPB_pre_start_at_startyear = if_else(START == 1, lag(CAPB, 1), NA_real_)
#   ) %>%
#   group_by(REF_AREA, episode_id) %>%
#   mutate(
#     # pick the pre-start CAPB for this episode (same value for all episode years)
#     CAPB_pre_start_ep = if_else(
#       !is.na(episode_id),
#       first(CAPB_pre_start_at_startyear[!is.na(CAPB_pre_start_at_startyear)]),
#       NA_real_
#     ),
#     
#     # CAPB at end of episode
#     CAPB_end_ep = if_else(!is.na(episode_id), last(CAPB), NA_real_),
#     
#     # cumulative adjustment achieved during the episode
#     cum_dCAPB_episode = if_else(!is.na(episode_id), CAPB_end_ep - CAPB_pre_start_ep, NA_real_)
#   ) %>%
#   ungroup() %>%
#   select(-CAPB_pre_start_at_startyear)
# #(6) Deceleration in the pace of debt accumulation from consolidation episode
# clean_data_MOD <- clean_data_MOD %>%
#   arrange(REF_AREA, TIME_PERIOD) %>%
#   group_by(REF_AREA) %>%
#   mutate(
#     debt  = as.numeric(GGFLMQ),
#     dDebt = debt - lag(debt, 1),
#     # 2y pre-start avg change in debt (computed on START year)
#     pre_avg_at_start = if_else(
#       START == 1,
#       (lag(dDebt, 1) + lag(dDebt, 2)) / 2,
#       NA_real_
#     ),
#     # 2y post-end avg change in debt (computed on END year)
#     post_avg_at_end = if_else(
#       END == 1,
#       (lead(dDebt, 1) + lead(dDebt, 2)) / 2,
#       NA_real_
#     )
#   ) %>%
#   group_by(REF_AREA, episode_id) %>%
#   mutate(
#     # pull the episode-specific pre/post averages into every episode-year row
#     pre_avg_2y = if_else(
#       !is.na(episode_id),
#       first(pre_avg_at_start[!is.na(pre_avg_at_start)]),
#       NA_real_
#     ),
#     post_avg_2y = if_else(
#       !is.na(episode_id),
#       first(post_avg_at_end[!is.na(post_avg_at_end)]),
#       NA_real_
#     ),
#     # deceleration: positive means debt accumulation slowed after the episode
#     decel_debt = if_else(!is.na(episode_id), pre_avg_2y - post_avg_2y, NA_real_)
#   ) %>%
#   ungroup() %>%
#   select(-debt, -dDebt, -pre_avg_at_start, -post_avg_at_end)
# #(7) Cumulative length of adjustment episode 
# clean_data_MOD <- clean_data_MOD %>%
#   arrange(REF_AREA, TIME_PERIOD) %>%
#   group_by(REF_AREA, episode_id) %>%
#   mutate(
#     # episode length (years)
#     episode_length = if_else(!is.na(episode_id), n(), NA_integer_)
#   ) %>%
#   ungroup()
# #(8) Cumulative change in CAPB in 2 years after consolidation episode 
# clean_data_MOD <- clean_data_MOD %>%
#   arrange(REF_AREA, TIME_PERIOD) %>%
#   group_by(REF_AREA) %>%
#   mutate(
#     # compute the post-2y cumulative change at the END year (needs full REF_AREA grouping for lead())
#     post2_cum_at_end = if_else(
#       END == 1,
#       lead(CAPB, 2) - CAPB,
#       NA_real_
#     )
#   ) %>%
#   group_by(REF_AREA, episode_id) %>%
#   mutate(
#     # carry the episode-level value to all episode years
#     post2_cum_dCAPB = if_else(
#       !is.na(episode_id),
#       first(post2_cum_at_end[!is.na(post2_cum_at_end)]),
#       NA_real_
#     )
#   ) %>%
#   ungroup() %>%
#   select(-post2_cum_at_end)
# 
# #9 - Construct Remaining Control Variables
# #(a) Change in real short rate relative to taylor rule
# pi_target <- 2  
# clean_data_MOD <- clean_data_MOD %>%
#   arrange(REF_AREA, TIME_PERIOD) %>%
#   group_by(REF_AREA) %>%
#   mutate(
#     # Taylor-rule implied REAL short rate component (drops the π_t level)
#     taylor_real = 0.5 * (as.numeric(INF) - pi_target) + 0.5 * as.numeric(GAP),
#     # Real short rate relative to Taylor rule
#     real_short_taylorgap = IRS_real - taylor_real,
#     # Change (1y)
#     d_real_short_taylorgap = real_short_taylorgap - lag(real_short_taylorgap, 1),
#     # Lag 
#     d_real_short_taylorgap_L1 = lag(d_real_short_taylorgap, 1)
#   ) %>%
#   ungroup()
# #(b) Share of expenditure cuts & #(c) Share of capital expenditure cuts
# clean_data_MOD <- clean_data_MOD %>%
#   arrange(REF_AREA, TIME_PERIOD) %>%
#   group_by(REF_AREA) %>%
#   mutate(
#     # Build expenditure series
#     ca_prim_exp_nom = as.numeric(YRGA) - as.numeric(NLGXA),
#     # Express as % of potential GDP (nominal potential output GDPTR)
#     ca_prim_exp_pot = 100 * (ca_prim_exp_nom / as.numeric(GDPTR)),
#     # Capital expenditure proxy (volume) as % of potential output volume (GDPVTR)
#     capex_pot = 100 * (as.numeric(IGV) / as.numeric(GDPVTR)),
#     
#     # Pre-start levels tagged on START year (must be computed in REF_AREA group)
#     primexp_pre_at_start = if_else(START == 1, lag(ca_prim_exp_pot, 1), NA_real_),
#     capex_pre_at_start   = if_else(START == 1, lag(capex_pot, 1),       NA_real_)
#   ) %>%
#   group_by(REF_AREA, episode_id) %>%
#   mutate(
#     
#     #Episode-level endpoints
#     primexp_pre_start = if_else(
#       !is.na(episode_id),
#       first(primexp_pre_at_start[!is.na(primexp_pre_at_start)]),
#       NA_real_
#     ),
#     primexp_end = if_else(!is.na(episode_id), last(ca_prim_exp_pot), NA_real_),
#     capex_pre_start = if_else(
#       !is.na(episode_id),
#       first(capex_pre_at_start[!is.na(capex_pre_at_start)]),
#       NA_real_
#     ),
#     capex_end = if_else(!is.na(episode_id), last(capex_pot), NA_real_),
#     
#     #Cumulative cuts during episode
#     cum_primexp_cut = if_else(!is.na(episode_id), primexp_pre_start - primexp_end, NA_real_),
#     cum_capex_cut   = if_else(!is.na(episode_id), capex_pre_start   - capex_end,   NA_real_),
#     
#     # 5) Shares (divide by total CAPB adjustment)
#     share_exp_cuts = if_else(
#       !is.na(episode_id) & !is.na(cum_dCAPB_episode) & cum_dCAPB_episode > 0,
#       cum_primexp_cut / cum_dCAPB_episode,
#       NA_real_
#     ),
#     share_capex_cuts = if_else(
#       !is.na(episode_id) & !is.na(cum_dCAPB_episode) & cum_dCAPB_episode > 0,
#       cum_capex_cut / cum_dCAPB_episode,
#       NA_real_
#     )
#   ) %>%
#   ungroup() %>%
#   select(
#     -primexp_pre_at_start, -capex_pre_at_start
#   )

#10 - Save Final Full Dataset
write.csv(clean_data_MOD,"Data/Clean Data/episode_OECD.csv")