# Script: exo_reg8.R
# Purpose: Regression 8 on Exogenous Episode Classification
# Input: Data/Clean Data/full_gen.csv
# Output: Output/exo_reg8.tex

# 1 - Load Libraries
library(readr)
library(dplyr)
library(purrr)
library(lmtest)
library(sandwich)
library(tibble)

# 2 - Helper Functions
first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[1]
}

safe_sum <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else sum(x)
}

# 3 - Load Dataset
df <- read_csv("Data/Clean Data/full_gen.csv", show_col_types = FALSE) %>%
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
    
    # EA dummies in the last year of the episode
    EA_pre1992_at_end   = if_else(END == 1, EA_pre1992,   NA_real_),
    EA_qual_9297_at_end = if_else(END == 1, EA_qual_9297, NA_real_),
    EA_1998on_at_end    = if_else(END == 1, EA_1998on,    NA_real_)
  ) %>%
  ungroup() %>%
  filter(year >= 1978, year <= 2014)

# 4 - Build Episode-Level Dataset
# One observation per episode
episode_df <- df %>%
  filter(!is.na(episode_id)) %>%
  group_by(country_code, episode_id) %>%
  summarise(
    # Dependent variable:
    # slippage = deterioration in CAPB in the two years after the episode ends
    slippage_ep = first_non_missing(-post2_cum_dCAPB),
    
    # Controls measured in the last year of the episode
    NEED_ADJ_last = first_non_missing(if_else(END == 1, as.numeric(NEED_ADJ), NA_real_)),
    GAP_last      = first_non_missing(if_else(END == 1, as.numeric(GAP),      NA_real_)),
    
    # EA period dummies in the last year of the episode
    EA_pre1992   = first_non_missing(EA_pre1992_at_end),
    EA_qual_9297 = first_non_missing(EA_qual_9297_at_end),
    EA_1998on    = first_non_missing(EA_1998on_at_end),
    
    # Cumulated adjustment over the episode
    cum_adjustment = first_non_missing(cum_dCAPB_episode),
    
    # Monetary variables cumulated during the episode
    d_IRS_episode_cum    = safe_sum(d_IRS),
    d_r_sh_episode_cum   = safe_sum(d_r_sh),
    d_IRL_episode_cum    = safe_sum(d_IRL),
    d_r_lo_episode_cum   = safe_sum(d_r_lo),
    d_taylor_episode_cum = safe_sum(as.numeric(d_real_short_taylorgap)),
    
    # Composition variables cumulated over the episode
    share_exp_episode   = first_non_missing(share_exp_cuts),
    share_capex_episode = first_non_missing(share_capex_cuts),
    
    .groups = "drop"
  ) %>%
  filter(!is.na(slippage_ep))

# 5 - Specifications
# 1: need adj + output gap
# 2: need adj + output gap + cumulated fiscal adjustment
# 3: need adj + output gap + share of expenditure cuts
# 4: need adj + output gap + share of capital-expenditure cuts
# 5-9: need adj + output gap + one monetary variable
# All specifications also include the three EA-period dummies as controls
specs <- list(
  "1" = list(add_vars = character(0)),
  "2" = list(add_vars = c("cum_adjustment")),
  "3" = list(add_vars = c("share_exp_episode")),
  "4" = list(add_vars = c("share_capex_episode")),
  "5" = list(add_vars = c("d_IRS_episode_cum")),
  "6" = list(add_vars = c("d_r_sh_episode_cum")),
  "7" = list(add_vars = c("d_IRL_episode_cum")),
  "8" = list(add_vars = c("d_r_lo_episode_cum")),
  "9" = list(add_vars = c("d_taylor_episode_cum"))
)

row_labels <- c(
  NEED_ADJ_last        = "Need for adjustment (last year of episode)",
  GAP_last             = "Output gap (last year of episode)",
  EA_pre1992           = "Euro-area countries 1978-1991",
  EA_qual_9297         = "Euro-area countries 1992-1997",
  EA_1998on            = "Euro-area countries 1998 onward",
  cum_adjustment       = "Change in cyclically adjusted fiscal balance (cumulated over episode)",
  share_exp_episode    = "Share of expenditure cuts in total adjustment (cumulated over episode)",
  share_capex_episode  = "Share of capital-expenditure cuts in total adj. (cumulated over episode)",
  d_IRS_episode_cum    = "Nominal short rate (cumulated change during episode)",
  d_r_sh_episode_cum   = "Real short rate (cumulated change during episode)",
  d_IRL_episode_cum    = "Nominal long rate (cumulated change during episode)",
  d_r_lo_episode_cum   = "Real long rate (cumulated change during episode)",
  d_taylor_episode_cum = "Short rate relative to Taylor rule (cumulated change during episode)"
)

row_order <- names(row_labels)

# 6 - Function to Run One Robust OLS Regression
run_slippage_ols <- function(data, add_vars = character(0)) {
  
  vars_keep <- c(
    "country_code",
    "slippage_ep",
    "NEED_ADJ_last",
    "GAP_last",
    "EA_pre1992",
    "EA_qual_9297",
    "EA_1998on",
    add_vars
  )
  
  reg_df <- data %>%
    select(all_of(vars_keep)) %>%
    filter(complete.cases(.))
  
  rhs_vars <- c(
    "NEED_ADJ_last",
    "GAP_last",
    "EA_pre1992",
    "EA_qual_9297",
    "EA_1998on",
    add_vars
  )
  
  model_formula <- as.formula(
    paste("slippage_ep ~", paste(rhs_vars, collapse = " + "))
  )
  
  model <- lm(model_formula, data = reg_df)
  
  vcov_use <- vcovCL(model, cluster = reg_df$country_code, type = "HC1")
  ct <- coeftest(model, vcov. = vcov_use)
  
  coef_tab <- tibble(
    factor   = rownames(ct),
    estimate = unname(ct[, 1]),
    p_value  = unname(ct[, 4])
  ) %>%
    filter(factor %in% row_order)
  
  list(
    model    = model,
    coef_tab = coef_tab,
    nobs     = nrow(reg_df),
    r2       = summary(model)$r.squared
  )
}

# 7 - Run All Models
results <- imap(
  specs,
  ~ run_slippage_ols(
    data     = episode_df,
    add_vars = .x$add_vars
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
  "\\caption{Slippage after episode (Exogenous Identification)}",
  "\\label{tab:reg8_slippage}",
  "\\begin{threeparttable}",
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{1.5pt}",
  "\\renewcommand{\\arraystretch}{1.15}",
  "\\begin{tabular}{@{}>{\\raggedright\\arraybackslash}p{0.40\\linewidth}*{9}{>{\\centering\\arraybackslash}p{0.055\\linewidth}}@{}}",
  "\\toprule",
  " & \\multicolumn{9}{c}{Regression results} \\\\",
  "\\cmidrule(lr){2-10}",
  paste0(
    "Dataset and type of regression & \\multicolumn{9}{l}{",
    "\\parbox[t]{0.52\\linewidth}{Robust OLS regressions; pooled sample - one observation per adjustment episode}",
    "} \\\\[0.4ex]"
  ),
  paste0(
    "Left-hand-side variable & \\multicolumn{9}{l}{",
    "\\parbox[t]{0.52\\linewidth}{Slippage, i.e. deterioration in the primary cyclically adjusted fiscal balance in the two years after the end of the adjustment episode}",
    "} \\\\[0.6ex]"
  ),
  "\\midrule",
  "Explanatory variable & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 \\\\",
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

r2_line <- paste(
  "R-squared",
  paste(map_chr(results, ~ fmt_num(.x$r2, 2)), collapse = " & "),
  sep = " & "
)

latex_lines <- c(
  latex_lines,
  "(Constant not reported) &  &  &  &  &  &  &  &  &  \\\\",
  "\\midrule",
  paste0(obs_line, " \\\\"),
  paste0(r2_line, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item Reported coefficients are robust OLS coefficients.",
  "\\item $^{*}$ significant at 10\\%; $^{**}$ significant at 5\\%; $^{***}$ significant at 1\\%.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

# 11 - Save LaTeX Table
dir.create("Output", showWarnings = FALSE)
writeLines(latex_lines, "Output/exo_reg8.tex")