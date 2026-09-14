# Script: OECD_reg7.R
# Purpose: Regression 7 on OECD Episode Classification
# Input: Data/Clean Data/full_OECD_dataset.csv
# Output: Output/OECD_reg7.tex

# 1 - Load Libraries
library(readr)
library(dplyr)
library(purrr)
library(lmtest)
library(sandwich)
library(tibble)
library(truncreg)

# 2 - Helper Functions
first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[1]
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else mean(x)
}

safe_sum <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else sum(x)
}

# 3 - Load Dataset
df <- read_csv("Data/Clean Data/full_OECD_dataset.csv", show_col_types = FALSE) %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    # Ensure EA period dummies are numeric
    EA_pre1992   = as.numeric(EA_pre1992),
    EA_qual_9297 = as.numeric(EA_qual_9297),
    EA_1998on    = as.numeric(EA_1998on),
    
    # Monetary changes
    d_IRS  = as.numeric(IRS)  - lag(as.numeric(IRS)),
    d_r_sh = as.numeric(r_sh) - lag(as.numeric(r_sh)),
    d_IRL  = as.numeric(IRL)  - lag(as.numeric(IRL)),
    d_r_lo = as.numeric(r_lo) - lag(as.numeric(r_lo)),
    
    # RER relative to country-specific historic average
    rexr_hist_avg = mean(as.numeric(rexr), na.rm = TRUE),
    rexr_rel_hist = 100 * ((as.numeric(rexr) / rexr_hist_avg) - 1),
    
    # Controls in the year before the start of the episode
    NEED_ADJ_pre_at_start      = if_else(start == 1, lag(as.numeric(NEED_ADJ)), NA_real_),
    GAP_pre_at_start           = if_else(start == 1, lag(as.numeric(GAP)), NA_real_),
    rexr_rel_hist_pre_at_start = if_else(start == 1, lag(rexr_rel_hist), NA_real_),
    
    # EA dummies at the start of the episode
    EA_pre1992_at_start   = if_else(start == 1, EA_pre1992,   NA_real_),
    EA_qual_9297_at_start = if_else(start == 1, EA_qual_9297, NA_real_),
    EA_1998on_at_start    = if_else(start == 1, EA_1998on,    NA_real_)
  ) %>%
  ungroup() %>%
  filter(year >= 1978, year <= 2014)

# 4 - Build Episode-Level Dataset
# One observation per episode
episode_df <- df %>%
  filter(!is.na(episode_id)) %>%
  group_by(country_code, episode_id) %>%
  summarise(
    # Dependent variable: length of adjustment episode
    episode_length_ep = first_non_missing(episode_length),
    
    # Controls measured in the year before the start of the episode
    NEED_ADJ_pre      = first_non_missing(NEED_ADJ_pre_at_start),
    GAP_pre           = first_non_missing(GAP_pre_at_start),
    rexr_rel_hist_pre = first_non_missing(rexr_rel_hist_pre_at_start),
    
    # EA period dummies at the start of the episode
    EA_pre1992   = first_non_missing(EA_pre1992_at_start),
    EA_qual_9297 = first_non_missing(EA_qual_9297_at_start),
    EA_1998on    = first_non_missing(EA_1998on_at_start),
    
    # Monetary variables over the whole episode
    # OECD wording: effective average change during episode
    d_IRS_episode_avg  = safe_mean(d_IRS),
    d_r_sh_episode_avg = safe_mean(d_r_sh),
    d_IRL_episode_avg  = safe_mean(d_IRL),
    d_r_lo_episode_avg = safe_mean(d_r_lo),
    
    # OECD wording: cumulated change during episode
    d_taylor_episode_cum = safe_sum(as.numeric(d_real_short_taylorgap)),
    
    # Composition variables cumulated over the episode
    share_exp_episode   = first_non_missing(share_exp_cuts),
    share_capex_episode = first_non_missing(share_capex_cuts),
    
    .groups = "drop"
  ) %>%
  filter(
    !is.na(episode_length_ep),
    episode_length_ep >= 1
  )

# 5 - Specifications
# 1: need adj + output gap
# 2-6: need adj + output gap + RER + one monetary variable
# 7: need adj + output gap + RER + share of expenditure cuts
# 8: need adj + output gap + RER + share of capital-expenditure cuts
# 9-13: need adj + output gap + RER + share of expenditure cuts + one monetary variable
# All specifications also include the three EA-period dummies as controls
specs <- list(
  "1"  = list(include_rer = FALSE, money_var = NULL,                   comp_var = NULL),
  "2"  = list(include_rer = TRUE,  money_var = "d_IRS_episode_avg",    comp_var = NULL),
  "3"  = list(include_rer = TRUE,  money_var = "d_r_sh_episode_avg",   comp_var = NULL),
  "4"  = list(include_rer = TRUE,  money_var = "d_IRL_episode_avg",    comp_var = NULL),
  "5"  = list(include_rer = TRUE,  money_var = "d_r_lo_episode_avg",   comp_var = NULL),
  "6"  = list(include_rer = TRUE,  money_var = "d_taylor_episode_cum", comp_var = NULL),
  "7"  = list(include_rer = TRUE,  money_var = NULL,                   comp_var = "share_exp_episode"),
  "8"  = list(include_rer = TRUE,  money_var = NULL,                   comp_var = "share_capex_episode"),
  "9"  = list(include_rer = TRUE,  money_var = "d_IRS_episode_avg",    comp_var = "share_exp_episode"),
  "10" = list(include_rer = TRUE,  money_var = "d_r_sh_episode_avg",   comp_var = "share_exp_episode"),
  "11" = list(include_rer = TRUE,  money_var = "d_IRL_episode_avg",    comp_var = "share_exp_episode"),
  "12" = list(include_rer = TRUE,  money_var = "d_r_lo_episode_avg",   comp_var = "share_exp_episode"),
  "13" = list(include_rer = TRUE,  money_var = "d_taylor_episode_cum", comp_var = "share_exp_episode")
)

row_labels <- c(
  NEED_ADJ_pre         = "Need for adjustment (year before start of episode)",
  GAP_pre              = "Output gap (year before start of episode)",
  rexr_rel_hist_pre    = "RER relative to historic average (year before start of episode)",
  EA_pre1992           = "Euro-area countries 1978-1991",
  EA_qual_9297         = "Euro-area countries 1992-1997",
  EA_1998on            = "Euro-area countries 1998 onward",
  d_IRS_episode_avg    = "Nominal short rate (effective average change during episode)",
  d_r_sh_episode_avg   = "Real short rate (effective average change during episode)",
  d_IRL_episode_avg    = "Nominal long rate (effective average change during episode)",
  d_r_lo_episode_avg   = "Real long rate (effective average change during episode)",
  d_taylor_episode_cum = "Short rate relative to Taylor rule (cumulated change during episode)",
  share_exp_episode    = "Share of expenditure cuts in total adjustment (cumulated over episode)",
  share_capex_episode  = "Share of capital-expenditure cuts in total adj. (cumulated over episode)"
)

row_order <- names(row_labels)

# 6 - Function to Run One Truncated Regression
run_length_trunc <- function(data, include_rer = FALSE, money_var = NULL, comp_var = NULL) {
  
  vars_keep <- c(
    "country_code", "episode_length_ep", "NEED_ADJ_pre", "GAP_pre",
    "EA_pre1992", "EA_qual_9297", "EA_1998on"
  )
  
  if (include_rer) {
    vars_keep <- c(vars_keep, "rexr_rel_hist_pre")
  }
  
  if (!is.null(money_var)) {
    vars_keep <- c(vars_keep, money_var)
  }
  
  if (!is.null(comp_var)) {
    vars_keep <- c(vars_keep, comp_var)
  }
  
  reg_df <- data %>%
    select(all_of(vars_keep)) %>%
    filter(complete.cases(.))
  
  if (!is.null(money_var)) {
    reg_df <- reg_df %>%
      rename(monetary_var = all_of(money_var))
  }
  
  if (!is.null(comp_var)) {
    reg_df <- reg_df %>%
      rename(composition_var = all_of(comp_var))
  }
  
  rhs_vars <- c(
    "NEED_ADJ_pre", "GAP_pre",
    "EA_pre1992", "EA_qual_9297", "EA_1998on"
  )
  
  if (include_rer) {
    rhs_vars <- c(rhs_vars, "rexr_rel_hist_pre")
  }
  
  if (!is.null(money_var)) {
    rhs_vars <- c(rhs_vars, "monetary_var")
  }
  
  if (!is.null(comp_var)) {
    rhs_vars <- c(rhs_vars, "composition_var")
  }
  
  model_formula <- as.formula(
    paste("episode_length_ep ~", paste(rhs_vars, collapse = " + "))
  )
  
  model <- truncreg(
    formula   = model_formula,
    data      = reg_df,
    point     = 1,
    direction = "left"
  )
  
  vcov_use <- tryCatch(
    vcovCL(model, cluster = reg_df$country_code, type = "HC1"),
    error = function(e) vcov(model)
  )
  
  ct <- coeftest(model, vcov. = vcov_use)
  
  coef_tab <- tibble(
    factor   = rownames(ct),
    estimate = unname(ct[, 1]),
    p_value  = unname(ct[, 4])
  )
  
  if (!is.null(money_var)) {
    coef_tab <- coef_tab %>%
      mutate(factor = ifelse(factor == "monetary_var", money_var, factor))
  }
  
  if (!is.null(comp_var)) {
    coef_tab <- coef_tab %>%
      mutate(factor = ifelse(factor == "composition_var", comp_var, factor))
  }
  
  coef_tab <- coef_tab %>%
    filter(factor %in% row_order)
  
  list(
    model    = model,
    coef_tab = coef_tab,
    nobs     = nrow(reg_df)
  )
}

# 7 - Run All Models
results <- imap(
  specs,
  ~ run_length_trunc(
    data        = episode_df,
    include_rer = .x$include_rer,
    money_var   = .x$money_var,
    comp_var    = .x$comp_var
  )
)

# 8 - Formatting Helpers
stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("")
}

fmt_num <- function(x, digits = 2) {
  if (is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_est <- function(est, p) {
  if (is.na(est)) return("")
  paste0(fmt_num(est, 2), stars(p))
}

# 9 - Build Table Body
est_mat <- matrix("", nrow = length(row_order), ncol = length(results))
rownames(est_mat) <- row_order
colnames(est_mat) <- names(results)

for (j in seq_along(results)) {
  coef_j <- results[[j]]$coef_tab
  
  for (v in row_order) {
    hit <- coef_j %>% filter(factor == v)
    if (nrow(hit) > 0) {
      est_mat[v, j] <- fmt_est(hit$estimate[1], hit$p_value[1])
    }
  }
}

# 10 - Create LaTeX Lines
latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Length of adjustment episode (OECD Episode Identification)}",
  "\\label{tab:reg7_length}",
  "\\begin{threeparttable}",
  "\\tiny",
  "\\setlength{\\tabcolsep}{1.5pt}",
  "\\renewcommand{\\arraystretch}{1.00}",
  "\\begin{tabular}{@{}>{\\raggedright\\arraybackslash}p{0.34\\linewidth}*{13}{>{\\centering\\arraybackslash}p{0.045\\linewidth}}@{}}",
  "\\toprule",
  " & \\multicolumn{13}{c}{Regression results} \\\\",
  "\\cmidrule(lr){2-14}",
  "Type of regression & \\multicolumn{13}{l}{Truncated regressions; pooled sample - one observation per adjustment episode} \\\\",
  "Left-hand-side variable & \\multicolumn{13}{l}{Length of adjustment episode (in years)} \\\\",
  "\\midrule",
  "Explanatory variables & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 & 11 & 12 & 13 \\\\",
  "\\midrule"
)

for (i in seq_along(row_order)) {
  row_name <- row_labels[row_order[i]]
  est_vals <- paste(est_mat[row_order[i], ], collapse = " & ")
  
  latex_lines <- c(
    latex_lines,
    paste0(row_name, " & ", est_vals, " \\\\")
  )
}

obs_line <- paste(
  "Observations",
  paste(map_chr(results, ~ as.character(.x$nobs)), collapse = " & "),
  sep = " & "
)

latex_lines <- c(
  latex_lines,
  "(Constant not reported) &  &  &  &  &  &  &  &  &  &  &  &  &  \\\\",
  "\\midrule",
  paste0(obs_line, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item Reported coefficients are truncated-regression coefficients.",
  "\\item $^{*}$ significant at 10\\%; $^{**}$ significant at 5\\%; $^{***}$ significant at 1\\%.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

# 11 - Save LaTeX Table
dir.create("Output", showWarnings = FALSE)
writeLines(latex_lines, "Output/OECD_reg7.tex")