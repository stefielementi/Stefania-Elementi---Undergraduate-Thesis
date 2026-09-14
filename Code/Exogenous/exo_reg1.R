# Script: exo_reg1.R
# Purpose: Regression 1 on Exogenous Episode Classification
# Input: Data/Clean Data/full_gen.csv
# Output: Output/exo_reg1.tex

# 1 - Load Libraries
library(readr)
library(dplyr)
library(purrr)
library(lmtest)
library(sandwich)
library(margins)
library(tibble)

# 2 - Load Dataset
df <- read_csv("Data/Clean Data/full_gen.csv", show_col_types = FALSE) %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    # Episode indicator
    episode = as.integer(start == 1 | continue == 1),
    
    # Ensure EA period dummies are numeric
    EA_pre1992   = as.numeric(EA_pre1992),
    EA_qual_9297 = as.numeric(EA_qual_9297),
    EA_1998on    = as.numeric(EA_1998on),
    
    # Changes in monetary / control variables
    d_IRS  = as.numeric(IRS)  - lag(as.numeric(IRS)),   # nominal short rate change
    d_r_sh = as.numeric(r_sh) - lag(as.numeric(r_sh)),  # real short rate change
    d_IRL  = as.numeric(IRL)  - lag(as.numeric(IRL)),   # nominal long rate change
    d_r_lo = as.numeric(r_lo) - lag(as.numeric(r_lo)),  # real long rate change
    d_rexr = as.numeric(rexr) - lag(as.numeric(rexr)),  # change in RER
    
    # Lagged "at risk" indicator
    episode_L1 = lag(episode),
    
    # Lagged controls
    NEED_ADJ_L1 = lag(as.numeric(NEED_ADJ)),
    GAP_L1      = lag(as.numeric(GAP)),
    d_rexr_L1   = lag(d_rexr),
    
    # Lagged monetary variables
    d_IRS_L1    = lag(d_IRS),
    d_r_sh_L1   = lag(d_r_sh),
    d_IRL_L1    = lag(d_IRL),
    d_r_lo_L1   = lag(d_r_lo),
    d_taylor_L1 = lag(as.numeric(d_real_short_taylorgap))
  ) %>%
  ungroup() %>%
  filter(year >= 1978, year <= 2014)

# 3 - Estimation sample:
# keep only observations where previous year was not already in an episode
reg_base <- df %>%
  filter(is.na(episode_L1) | episode_L1 == 0)

# 4 - Specifications
specs <- list(
  "1" = list(include_gap = TRUE,  money_var = NULL),
  "2" = list(include_gap = FALSE, money_var = "d_IRS_L1"),
  "3" = list(include_gap = FALSE, money_var = "d_r_sh_L1"),
  "4" = list(include_gap = FALSE, money_var = "d_IRL_L1"),
  "5" = list(include_gap = FALSE, money_var = "d_r_lo_L1"),
  "6" = list(include_gap = FALSE, money_var = "d_taylor_L1")
)

row_labels <- c(
  NEED_ADJ_L1   = "Need for adjustment (lagged)",
  GAP_L1        = "Output gap (lagged)",
  d_rexr_L1     = "Change in RER (lagged)",
  EA_pre1992    = "Euro-area countries 1978--1991",
  EA_qual_9297  = "Euro-area countries 1992--1997",
  EA_1998on     = "Euro-area countries 1998 onward",
  d_IRS_L1      = "Nominal short rate (lagged change)",
  d_r_sh_L1     = "Real short rate (lagged change)",
  d_IRL_L1      = "Nominal long rate (lagged change)",
  d_r_lo_L1     = "Real long rate (lagged change)",
  d_taylor_L1   = "Changes in short rates relative to Taylor rule"
)

row_order <- names(row_labels)

# 5 - Function to run one pooled probit and extract AMEs
run_start_probit <- function(data, include_gap = TRUE, money_var = NULL) {
  
  vars_keep <- c(
    "country_code", "year", "start", "NEED_ADJ_L1", "d_rexr_L1",
    "EA_pre1992", "EA_qual_9297", "EA_1998on"
  )
  
  if (include_gap) {
    vars_keep <- c(vars_keep, "GAP_L1")
  }
  
  if (!is.null(money_var)) {
    vars_keep <- c(vars_keep, money_var)
  }
  
  reg_df <- data %>%
    select(all_of(vars_keep)) %>%
    filter(complete.cases(.))
  
  if (!is.null(money_var)) {
    reg_df <- reg_df %>%
      rename(monetary_L1 = all_of(money_var))
  }
  
  if (include_gap && is.null(money_var)) {
    model <- glm(
      start ~ NEED_ADJ_L1 + GAP_L1 + d_rexr_L1 + EA_pre1992 + EA_qual_9297 + EA_1998on,
      data   = reg_df,
      family = binomial(link = "probit")
    )
  } else if (!include_gap && !is.null(money_var)) {
    model <- glm(
      start ~ NEED_ADJ_L1 + d_rexr_L1 + EA_pre1992 + EA_qual_9297 + EA_1998on + monetary_L1,
      data   = reg_df,
      family = binomial(link = "probit")
    )
  } else {
    stop("Invalid specification: choose either output gap or one monetary variable.")
  }
  
  vcov_cl <- vcovCL(model, cluster = reg_df$country_code, type = "HC1")
  
  ame <- summary(margins(model, vcov = vcov_cl)) %>%
    as_tibble()
  
  if (!is.null(money_var)) {
    ame <- ame %>%
      mutate(
        factor = ifelse(factor == "monetary_L1", money_var, factor)
      )
  }
  
  pseudo_r2 <- 1 - as.numeric(logLik(model) / logLik(update(model, . ~ 1)))
  
  list(
    model = model,
    ame = ame,
    nobs = nobs(model),
    pseudo_r2 = pseudo_r2
  )
}

# 6 - Run all models
results <- imap(
  specs,
  ~ run_start_probit(
    data = reg_base,
    include_gap = .x$include_gap,
    money_var   = .x$money_var
  )
)

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

fmt_cell <- function(est, z, p) {
  if (is.na(est)) return("")
  paste0("\\makecell[c]{", fmt_num(est, 2), stars(p), " \\\\ [", fmt_num(z, 2), "]}")
}

# 8 - Build table body
table_mat <- matrix("", nrow = length(row_order), ncol = length(results))
rownames(table_mat) <- row_order
colnames(table_mat) <- names(results)

for (j in seq_along(results)) {
  ame_j <- results[[j]]$ame
  
  for (v in row_order) {
    hit <- ame_j %>% filter(factor == v)
    if (nrow(hit) > 0) {
      table_mat[v, j] <- fmt_cell(hit$AME[1], hit$z[1], hit$p[1])
    }
  }
}

# 9 - Create LaTeX lines
latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Probability that a consolidation episode is started (Exogenous Identification)}",
  "\\label{tab:reg1_start}",
  "\\begin{threeparttable}",
  "\\small",
  "\\setlength{\\tabcolsep}{4pt}",
  "\\renewcommand{\\arraystretch}{1.05}",
  "\\begin{tabularx}{\\linewidth}{@{}>{\\raggedright\\arraybackslash}p{0.40\\linewidth}YYYYYY@{}}",
  "\\toprule",
  " & \\multicolumn{6}{c}{Regression results} \\\\",
  "\\cmidrule(lr){2-7}",
  "Type of regression: & \\multicolumn{6}{l}{Pooled probit regressions} \\\\",
  "Left-hand-side variable & \\multicolumn{6}{l}{Binary variable indicating exogenous start of an adjustment episode} \\\\",
  "\\midrule",
  "Explanatory variables & 1 & 2 & 3 & 4 & 5 & 6 \\\\",
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

r2_line <- paste(
  "Pseudo R2",
  paste(map_chr(results, ~ fmt_num(.x$pseudo_r2, 2)), collapse = " & "),
  sep = " & "
)

latex_lines <- c(
  latex_lines,
  "(constant not reported) &  &  &  &  &  &  \\\\",
  "\\midrule",
  paste0(obs_line, " \\\\"),
  paste0(r2_line, " \\\\"),
  "\\bottomrule",
  "\\end{tabularx}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item Reported coefficients are average marginal effects.",
  "\\item $^{*}$ significant at 10\\%; $^{**}$ significant at 5\\%; $^{***}$ significant at 1\\%.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

# 10 - Save LaTeX table
dir.create("Output", showWarnings = FALSE)
writeLines(latex_lines, "Output/exo_reg1.tex")