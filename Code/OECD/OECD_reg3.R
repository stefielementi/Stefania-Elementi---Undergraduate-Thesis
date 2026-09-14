# Script: OECD_reg3.R
# Purpose: Regression 3 on OECD Episode Classification
# Input: Data/Clean Data/full_OECD_dataset.csv
# Output: Output/OECD_reg3.tex

# 1 - Load Libraries
library(readr)
library(dplyr)
library(purrr)
library(lmtest)
library(sandwich)
library(tibble)

# 2 - Load Dataset
df <- read_csv("Data/Clean Data/full_OECD_dataset.csv", show_col_types = FALSE) %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    # Episode indicator
    episode = as.integer(start == 1 | continue == 1),
    
    # Ensure EA period dummies are numeric
    EA_pre1992   = as.numeric(EA_pre1992),
    EA_qual_9297 = as.numeric(EA_qual_9297),
    EA_1998on    = as.numeric(EA_1998on),
    EA_excl_9297 = as.numeric(EA_pre1992 == 1 | EA_1998on == 1),
    
    # Dependent variable: yearly change in CAPB (% of potential GDP)
    d_CAPB_pot = as.numeric(NLGXQA) - lag(as.numeric(NLGXQA)),
    
    # Changes in monetary variables
    d_IRS  = as.numeric(IRS)  - lag(as.numeric(IRS)),   # nominal short rate change
    d_r_sh = as.numeric(r_sh) - lag(as.numeric(r_sh)),  # real short rate change
    d_IRL  = as.numeric(IRL)  - lag(as.numeric(IRL)),   # nominal long rate change
    d_r_lo = as.numeric(r_lo) - lag(as.numeric(r_lo)),  # real long rate change
    
    # RER relative to country-specific historic average
    rexr_hist_avg = mean(as.numeric(rexr), na.rm = TRUE),
    rexr_rel_hist = 100 * ((as.numeric(rexr) / rexr_hist_avg) - 1),
    
    # Lagged episode status
    episode_L1 = lag(episode),
    
    # Lagged controls
    NEED_ADJ_L1      = lag(as.numeric(NEED_ADJ)),
    GAP_L1           = lag(as.numeric(GAP)),
    rexr_rel_hist_L1 = lag(rexr_rel_hist),
    
    # Lagged monetary variables
    d_IRS_L1    = lag(d_IRS),
    d_r_sh_L1   = lag(d_r_sh),
    d_IRL_L1    = lag(d_IRL),
    d_r_lo_L1   = lag(d_r_lo),
    d_taylor_L1 = lag(as.numeric(d_real_short_taylorgap)),
    
    # Lagged composition controls
    share_exp_cuts_L1   = lag(as.numeric(share_exp_cuts)),
    share_capex_cuts_L1 = lag(as.numeric(share_capex_cuts))
  ) %>%
  ungroup() %>%
  filter(year >= 1978, year <= 2014)

# 3 - Estimation sample:
# keep only observations where previous year was already part of an episode
reg_base <- df %>%
  filter(episode_L1 == 1)

# 4 - Specifications
# 1: need for adjustment + output gap
# 2: need for adjustment + output gap + RER relative to historic average
# 3-7: need for adjustment + output gap + one monetary variable
# 8: need for adjustment + output gap + share of expenditure cuts
# 9: need for adjustment + output gap + share of capital-expenditure cuts
# 10-14: need for adjustment + output gap + share of expenditure cuts + one monetary variable
fe_specs <- list(
  "1"  = list(include_rer = FALSE, money_var = NULL,          comp_var = NULL),
  "2"  = list(include_rer = TRUE,  money_var = NULL,          comp_var = NULL),
  "3"  = list(include_rer = FALSE, money_var = "d_IRS_L1",    comp_var = NULL),
  "4"  = list(include_rer = FALSE, money_var = "d_r_sh_L1",   comp_var = NULL),
  "5"  = list(include_rer = FALSE, money_var = "d_IRL_L1",    comp_var = NULL),
  "6"  = list(include_rer = FALSE, money_var = "d_r_lo_L1",   comp_var = NULL),
  "7"  = list(include_rer = FALSE, money_var = "d_taylor_L1", comp_var = NULL),
  "8"  = list(include_rer = FALSE, money_var = NULL,          comp_var = "share_exp_cuts_L1"),
  "9"  = list(include_rer = FALSE, money_var = NULL,          comp_var = "share_capex_cuts_L1"),
  "10" = list(include_rer = FALSE, money_var = "d_IRS_L1",    comp_var = "share_exp_cuts_L1"),
  "11" = list(include_rer = FALSE, money_var = "d_r_sh_L1",   comp_var = "share_exp_cuts_L1"),
  "12" = list(include_rer = FALSE, money_var = "d_IRL_L1",    comp_var = "share_exp_cuts_L1"),
  "13" = list(include_rer = FALSE, money_var = "d_r_lo_L1",   comp_var = "share_exp_cuts_L1"),
  "14" = list(include_rer = FALSE, money_var = "d_taylor_L1", comp_var = "share_exp_cuts_L1")
)

# 15-16: pooled OLS regressions with euro-area dummies
pool_specs <- list(
  "15" = list(ea_vars = c("EA_excl_9297", "EA_qual_9297")),
  "16" = list(ea_vars = c("EA_pre1992", "EA_qual_9297", "EA_1998on"))
)

row_labels <- c(
  NEED_ADJ_L1         = "Need for adjustment (lagged)",
  GAP_L1              = "Output gap (lagged)",
  rexr_rel_hist_L1    = "RER relative to historic average (lagged)",
  d_IRS_L1            = "Nominal short rate (lagged change)",
  d_r_sh_L1           = "Real short rate (lagged change)",
  d_IRL_L1            = "Nominal long rate (lagged change)",
  d_r_lo_L1           = "Real long rate (lagged change)",
  d_taylor_L1         = "Changes in short rates relative to Taylor rule",
  share_exp_cuts_L1   = "Share of expenditure cuts in total adjustment (lagged)",
  share_capex_cuts_L1 = "Share of capital-expenditure cuts in total adj. (lagged)",
  EA_excl_9297        = "Euro-area countries excluding 1992--1997",
  EA_qual_9297        = "Euro-area countries 1992--1997",
  EA_pre1992          = "Euro-area countries 1978--1991",
  EA_1998on           = "Euro-area countries 1998 onward"
)

row_order <- names(row_labels)

# 5 - Function to run one fixed-effect regression and extract clustered results
run_size_fe <- function(data, include_rer = FALSE, money_var = NULL, comp_var = NULL) {
  
  vars_keep <- c("country_code", "year", "d_CAPB_pot", "NEED_ADJ_L1", "GAP_L1")
  
  if (include_rer) {
    vars_keep <- c(vars_keep, "rexr_rel_hist_L1")
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
      rename(monetary_L1 = all_of(money_var))
  }
  
  if (!is.null(comp_var)) {
    reg_df <- reg_df %>%
      rename(composition_L1 = all_of(comp_var))
  }
  
  rhs_vars <- c("NEED_ADJ_L1", "GAP_L1")
  
  if (include_rer) {
    rhs_vars <- c(rhs_vars, "rexr_rel_hist_L1")
  }
  
  if (!is.null(money_var)) {
    rhs_vars <- c(rhs_vars, "monetary_L1")
  }
  
  if (!is.null(comp_var)) {
    rhs_vars <- c(rhs_vars, "composition_L1")
  }
  
  model_formula <- as.formula(
    paste("d_CAPB_pot ~", paste(c(rhs_vars, "factor(country_code)"), collapse = " + "))
  )
  
  model <- lm(model_formula, data = reg_df)
  
  vcov_cl <- vcovCL(model, cluster = reg_df$country_code, type = "HC1")
  ct <- coeftest(model, vcov. = vcov_cl)
  
  coef_tab <- tibble(
    factor   = rownames(ct),
    estimate = unname(ct[, 1]),
    stat     = unname(ct[, 3]),
    p_value  = unname(ct[, 4])
  )
  
  if (!is.null(money_var)) {
    coef_tab <- coef_tab %>%
      mutate(
        factor = ifelse(factor == "monetary_L1", money_var, factor)
      )
  }
  
  if (!is.null(comp_var)) {
    coef_tab <- coef_tab %>%
      mutate(
        factor = ifelse(factor == "composition_L1", comp_var, factor)
      )
  }
  
  coef_tab <- coef_tab %>%
    filter(factor %in% row_order)
  
  list(
    model = model,
    coef_tab = coef_tab,
    nobs = nobs(model),
    ngroups = n_distinct(reg_df$country_code),
    adj_r2 = summary(model)$adj.r.squared
  )
}

# 5b - Function to run pooled OLS euro-area regressions and extract clustered results
run_size_pool <- function(data, ea_vars) {
  
  vars_keep <- c("country_code", "year", "d_CAPB_pot", "NEED_ADJ_L1", "GAP_L1", ea_vars)
  
  reg_df <- data %>%
    select(all_of(vars_keep)) %>%
    filter(complete.cases(.))
  
  model_formula <- as.formula(
    paste("d_CAPB_pot ~", paste(c("NEED_ADJ_L1", "GAP_L1", ea_vars), collapse = " + "))
  )
  
  model <- lm(model_formula, data = reg_df)
  
  vcov_cl <- vcovCL(model, cluster = reg_df$country_code, type = "HC1")
  ct <- coeftest(model, vcov. = vcov_cl)
  
  coef_tab <- tibble(
    factor   = rownames(ct),
    estimate = unname(ct[, 1]),
    stat     = unname(ct[, 3]),
    p_value  = unname(ct[, 4])
  ) %>%
    filter(factor %in% row_order)
  
  list(
    model = model,
    coef_tab = coef_tab,
    nobs = nobs(model),
    ngroups = NA_integer_,
    adj_r2 = summary(model)$adj.r.squared
  )
}

# 6 - Run all models
results_fe <- imap(
  fe_specs,
  ~ run_size_fe(
    data        = reg_base,
    include_rer = .x$include_rer,
    money_var   = .x$money_var,
    comp_var    = .x$comp_var
  )
)

results_pool <- imap(
  pool_specs,
  ~ run_size_pool(
    data    = reg_base,
    ea_vars = .x$ea_vars
  )
)

results <- c(results_fe, results_pool)

# 7 - Formatting helpers
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

fmt_cell <- function(est, stat, p) {
  if (is.na(est)) return("")
  paste0("\\makecell[c]{", fmt_num(est, 2), stars(p), " \\\\ [", fmt_num(stat, 2), "]}")
}

# 8 - Build table body
table_mat <- matrix("", nrow = length(row_order), ncol = length(results))
rownames(table_mat) <- row_order
colnames(table_mat) <- names(results)

for (j in seq_along(results)) {
  coef_j <- results[[j]]$coef_tab
  
  for (v in row_order) {
    hit <- coef_j %>% filter(factor == v)
    if (nrow(hit) > 0) {
      table_mat[v, j] <- fmt_cell(hit$estimate[1], hit$stat[1], hit$p_value[1])
    }
  }
}

# 9 - Create LaTeX lines
tab_cols <- paste(rep("Y", length(results)), collapse = "")
results_header <- paste(names(results), collapse = " & ")
constant_line <- paste(c("(Constant not reported)", rep("", length(results))), collapse = " & ")

latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Size of yearly fiscal adjustment (OECD Episode Identification)}",
  "\\label{tab:reg3_size}",
  "\\begin{threeparttable}",
  "\\tiny",
  "\\setlength{\\tabcolsep}{1.8pt}",
  "\\renewcommand{\\arraystretch}{1.10}",
  paste0("\\begin{tabularx}{\\linewidth}{@{}>{\\raggedright\\arraybackslash}p{0.28\\linewidth}", tab_cols, "@{}}"),
  "\\toprule",
  " & \\multicolumn{14}{c}{Regression results} & \\multicolumn{2}{c}{ } \\\\",
  "\\cmidrule(lr){2-15}\\cmidrule(lr){16-17}",
  "Type of regression: & \\multicolumn{14}{l}{Fixed-effect panel regressions} & \\multicolumn{2}{l}{Pooled OLS regressions} \\\\",
  "Left-hand-side variable & \\multicolumn{16}{l}{Change in the primary adjusted fiscal balance (by year)} \\\\",
  "\\midrule",
  paste0("Explanatory variables & ", results_header, " \\\\"),
  "\\midrule"
)

for (i in seq_along(row_order)) {
  row_name <- row_labels[row_order[i]]
  row_vals <- paste(table_mat[row_order[i], ], collapse = " & ")
  latex_lines <- c(
    latex_lines,
    paste0(row_name, " & ", row_vals, " \\\\")
  )
}

obs_line <- paste(
  "Observations",
  paste(map_chr(results, ~ as.character(.x$nobs)), collapse = " & "),
  sep = " & "
)

groups_line <- paste(
  "Number of groups",
  paste(map_chr(results, ~ ifelse(is.na(.x$ngroups), "", as.character(.x$ngroups))), collapse = " & "),
  sep = " & "
)

r2_line <- paste(
  "Adjusted R2",
  paste(map_chr(results, ~ fmt_num(.x$adj_r2, 2)), collapse = " & "),
  sep = " & "
)

latex_lines <- c(
  latex_lines,
  paste0(constant_line, " \\\\"),
  "\\midrule",
  paste0(obs_line, " \\\\"),
  paste0(groups_line, " \\\\"),
  paste0(r2_line, " \\\\"),
  "\\bottomrule",
  "\\end{tabularx}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item $^{*}$ significant at 10\\%; $^{**}$ significant at 5\\%; $^{***}$ significant at 1\\%.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

# 10 - Save LaTeX table
dir.create("Output", showWarnings = FALSE)
writeLines(latex_lines, "Output/OECD_reg3.tex")