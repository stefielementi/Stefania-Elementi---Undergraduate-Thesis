# Script: OECD_reg4.R
# Purpose: Regression 4 on OECD Episode Classification
# Input: Data/Clean Data/full_OECD_dataset.csv
# Output: Output/OECD_reg4.tex

# 1 - Load Libraries
library(readr)
library(dplyr)
library(purrr)
library(lmtest)
library(sandwich)
library(margins)
library(tibble)

# 2 - Load Dataset
df <- read_csv("Data/Clean Data/full_OECD_dataset.csv", show_col_types = FALSE) %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    # Ensure EA period dummies are numeric
    EA_pre1992   = as.numeric(EA_pre1992),
    EA_qual_9297 = as.numeric(EA_qual_9297),
    EA_1998on    = as.numeric(EA_1998on),
    
    # Yearly change in CAPB (% of potential GDP)
    d_CAPB_pot = as.numeric(NLGXQA) - lag(as.numeric(NLGXQA)),
    
    # Changes in monetary variables
    d_IRS  = as.numeric(IRS)  - lag(as.numeric(IRS)),
    d_r_sh = as.numeric(r_sh) - lag(as.numeric(r_sh)),
    d_IRL  = as.numeric(IRL)  - lag(as.numeric(IRL)),
    d_r_lo = as.numeric(r_lo) - lag(as.numeric(r_lo)),
    
    # Relative RER: deviation from country-specific historic average
    rexr_hist_avg = mean(as.numeric(rexr), na.rm = TRUE),
    rexr_rel_hist = 100 * ((as.numeric(rexr) / rexr_hist_avg) - 1),
    
    # Starting phase length
    start_phase_len = case_when(
      start == 1 &
        !is.na(d_CAPB_pot) &
        d_CAPB_pot >= 1 ~ 1L,
      
      start == 1 &
        !is.na(d_CAPB_pot) &
        !is.na(lead(d_CAPB_pot, 1)) &
        d_CAPB_pot >= 0.5 &
        lead(d_CAPB_pot, 1) >= 0.5 &
        (d_CAPB_pot + lead(d_CAPB_pot, 1)) >= 1 ~ 2L,
      
      TRUE ~ NA_integer_
    ),
    
    # Controls measured at the end of the starting phase
    NEED_ADJ_end = case_when(
      start_phase_len == 1L ~ as.numeric(NEED_ADJ),
      start_phase_len == 2L ~ lead(as.numeric(NEED_ADJ), 1),
      TRUE ~ NA_real_
    ),
    GAP_end = case_when(
      start_phase_len == 1L ~ as.numeric(GAP),
      start_phase_len == 2L ~ lead(as.numeric(GAP), 1),
      TRUE ~ NA_real_
    ),
    rexr_rel_hist_end = case_when(
      start_phase_len == 1L ~ rexr_rel_hist,
      start_phase_len == 2L ~ lead(rexr_rel_hist, 1),
      TRUE ~ NA_real_
    ),
    
    # Euro-area period dummies measured at the end of the starting phase
    EA_pre1992_end = case_when(
      start_phase_len == 1L ~ EA_pre1992,
      start_phase_len == 2L ~ lead(EA_pre1992, 1),
      TRUE ~ NA_real_
    ),
    EA_qual_9297_end = case_when(
      start_phase_len == 1L ~ EA_qual_9297,
      start_phase_len == 2L ~ lead(EA_qual_9297, 1),
      TRUE ~ NA_real_
    ),
    EA_1998on_end = case_when(
      start_phase_len == 1L ~ EA_1998on,
      start_phase_len == 2L ~ lead(EA_1998on, 1),
      TRUE ~ NA_real_
    ),
    
    # Average change in monetary conditions during the starting phase
    d_IRS_startavg = case_when(
      start_phase_len == 1L ~ d_IRS,
      start_phase_len == 2L ~ (d_IRS + lead(d_IRS, 1)) / 2,
      TRUE ~ NA_real_
    ),
    d_r_sh_startavg = case_when(
      start_phase_len == 1L ~ d_r_sh,
      start_phase_len == 2L ~ (d_r_sh + lead(d_r_sh, 1)) / 2,
      TRUE ~ NA_real_
    ),
    d_IRL_startavg = case_when(
      start_phase_len == 1L ~ d_IRL,
      start_phase_len == 2L ~ (d_IRL + lead(d_IRL, 1)) / 2,
      TRUE ~ NA_real_
    ),
    d_r_lo_startavg = case_when(
      start_phase_len == 1L ~ d_r_lo,
      start_phase_len == 2L ~ (d_r_lo + lead(d_r_lo, 1)) / 2,
      TRUE ~ NA_real_
    ),
    d_taylor_startavg = case_when(
      start_phase_len == 1L ~ as.numeric(d_real_short_taylorgap),
      start_phase_len == 2L ~ (
        as.numeric(d_real_short_taylorgap) +
          lead(as.numeric(d_real_short_taylorgap), 1)
      ) / 2,
      TRUE ~ NA_real_
    ),
    
    # Starting-phase composition variables
    CAPB_pre_start = lag(as.numeric(NLGXQA)),
    CAPB_end_startphase = case_when(
      start_phase_len == 1L ~ as.numeric(NLGXQA),
      start_phase_len == 2L ~ lead(as.numeric(NLGXQA), 1),
      TRUE ~ NA_real_
    ),
    cum_dCAPB_startphase = CAPB_end_startphase - CAPB_pre_start,
    
    currentexp_pre_start = lag(as.numeric(currentexp_pot)),
    currentexp_end_startphase = case_when(
      start_phase_len == 1L ~ as.numeric(currentexp_pot),
      start_phase_len == 2L ~ lead(as.numeric(currentexp_pot), 1),
      TRUE ~ NA_real_
    ),
    
    capex_pre_start = lag(as.numeric(capex_pot)),
    capex_end_startphase = case_when(
      start_phase_len == 1L ~ as.numeric(capex_pot),
      start_phase_len == 2L ~ lead(as.numeric(capex_pot), 1),
      TRUE ~ NA_real_
    ),
    
    share_exp_cuts_startphase = case_when(
      !is.na(cum_dCAPB_startphase) & cum_dCAPB_startphase > 0 ~
        (currentexp_pre_start - currentexp_end_startphase) / cum_dCAPB_startphase,
      TRUE ~ NA_real_
    ),
    share_capex_cuts_startphase = case_when(
      !is.na(cum_dCAPB_startphase) & cum_dCAPB_startphase > 0 ~
        (capex_pre_start - capex_end_startphase) / cum_dCAPB_startphase,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  filter(year >= 1978, year <= 2014)

# 3 - Episode-level sample: one observation per adjustment episode
reg_base <- df %>%
  filter(start == 1, !is.na(start_phase_len)) %>%
  transmute(
    country_code,
    year,
    serious = as.integer(serious),
    NEED_ADJ_end,
    GAP_end,
    rexr_rel_hist_end,
    EA_pre1992_end,
    EA_qual_9297_end,
    EA_1998on_end,
    d_IRS_startavg,
    d_r_sh_startavg,
    d_IRL_startavg,
    d_r_lo_startavg,
    d_taylor_startavg,
    share_exp_cuts_startphase,
    share_capex_cuts_startphase
  )

# 4 - Specifications
specs <- list(
  "1"  = list(include_rer = TRUE,  money_var = NULL,                comp_var = NULL),
  "2"  = list(include_rer = FALSE, money_var = NULL,                comp_var = NULL),
  "3"  = list(include_rer = FALSE, money_var = "d_IRS_startavg",    comp_var = NULL),
  "4"  = list(include_rer = FALSE, money_var = "d_r_sh_startavg",   comp_var = NULL),
  "5"  = list(include_rer = FALSE, money_var = "d_IRL_startavg",    comp_var = NULL),
  "6"  = list(include_rer = FALSE, money_var = "d_r_lo_startavg",   comp_var = NULL),
  "7"  = list(include_rer = FALSE, money_var = "d_taylor_startavg", comp_var = NULL),
  "8"  = list(include_rer = FALSE, money_var = NULL,                comp_var = "share_exp_cuts_startphase"),
  "9"  = list(include_rer = FALSE, money_var = NULL,                comp_var = "share_capex_cuts_startphase"),
  "10" = list(include_rer = FALSE, money_var = "d_IRS_startavg",    comp_var = "share_exp_cuts_startphase"),
  "11" = list(include_rer = FALSE, money_var = "d_r_sh_startavg",   comp_var = "share_exp_cuts_startphase"),
  "12" = list(include_rer = FALSE, money_var = "d_IRL_startavg",    comp_var = "share_exp_cuts_startphase"),
  "13" = list(include_rer = FALSE, money_var = "d_r_lo_startavg",   comp_var = "share_exp_cuts_startphase"),
  "14" = list(include_rer = FALSE, money_var = "d_taylor_startavg", comp_var = "share_exp_cuts_startphase")
)

row_labels <- c(
  NEED_ADJ_end                = "Need for adjustment (end start phase)",
  GAP_end                     = "Output gap (end start phase)",
  rexr_rel_hist_end           = "Relative RER (end start phase)",
  EA_pre1992_end              = "Euro-area countries 1978-1991",
  EA_qual_9297_end            = "Euro-area countries 1992-1997",
  EA_1998on_end               = "Euro-area countries 1998 onward",
  d_IRS_startavg              = "Nominal short rate (avg change, start phase)",
  d_r_sh_startavg             = "Real short rate (avg change, start phase)",
  d_IRL_startavg              = "Nominal long rate (avg change, start phase)",
  d_r_lo_startavg             = "Real long rate (avg change, start phase)",
  d_taylor_startavg           = "Short rates vs Taylor rule (avg change, start phase)",
  share_exp_cuts_startphase   = "Share of expenditure cuts (start phase)",
  share_capex_cuts_startphase = "Share of capex cuts (start phase)"
)

row_order <- names(row_labels)

# 5 - Function to run one pooled probit and extract AMEs
run_serious_probit <- function(data, include_rer = FALSE, money_var = NULL, comp_var = NULL) {
  
  vars_keep <- c(
    "country_code", "year", "serious", "NEED_ADJ_end", "GAP_end",
    "EA_pre1992_end", "EA_qual_9297_end", "EA_1998on_end"
  )
  
  if (include_rer) {
    vars_keep <- c(vars_keep, "rexr_rel_hist_end")
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
  
  rhs_vars <- c("NEED_ADJ_end", "GAP_end", "EA_pre1992_end", "EA_qual_9297_end", "EA_1998on_end")
  
  if (include_rer) {
    rhs_vars <- c(rhs_vars, "rexr_rel_hist_end")
  }
  
  if (!is.null(money_var)) {
    rhs_vars <- c(rhs_vars, "monetary_var")
  }
  
  if (!is.null(comp_var)) {
    rhs_vars <- c(rhs_vars, "composition_var")
  }
  
  model_formula <- as.formula(
    paste("serious ~", paste(rhs_vars, collapse = " + "))
  )
  
  model <- glm(
    model_formula,
    data   = reg_df,
    family = binomial(link = "probit")
  )
  
  vcov_cl <- vcovCL(model, cluster = reg_df$country_code, type = "HC1")
  
  ame <- summary(margins(model, vcov = vcov_cl)) %>%
    as_tibble()
  
  if (!is.null(money_var)) {
    ame <- ame %>%
      mutate(factor = ifelse(factor == "monetary_var", money_var, factor))
  }
  
  if (!is.null(comp_var)) {
    ame <- ame %>%
      mutate(factor = ifelse(factor == "composition_var", comp_var, factor))
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
  ~ run_serious_probit(
    data        = reg_base,
    include_rer = .x$include_rer,
    money_var   = .x$money_var,
    comp_var    = .x$comp_var
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
  x <- round(as.numeric(x), digits)
  if (identical(x, -0) || abs(x) < 10^(-digits)) x <- 0
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_est <- function(est, p) {
  if (is.na(est)) return("")
  paste0(fmt_num(est, 2), stars(p))
}

# Uniform cell height helper
cell_strut <- "\\rule{0pt}{2.6ex}"

pad_cell <- function(x, align = "c") {
  if (is.na(x) || !nzchar(trimws(x))) {
    return(paste0("\\makecell[", align, "]{", cell_strut, "}"))
  }
  paste0("\\makecell[", align, "]{", cell_strut, " ", x, "}")
}

# 8 - Build table body: coefficients only
est_mat <- matrix("", nrow = length(row_order), ncol = length(results))
rownames(est_mat) <- row_order
colnames(est_mat) <- names(results)

for (j in seq_along(results)) {
  ame_j <- results[[j]]$ame
  
  for (v in row_order) {
    hit <- ame_j %>% filter(factor == v)
    if (nrow(hit) > 0) {
      est_mat[v, j] <- fmt_est(hit$AME[1], hit$p[1])
    }
  }
}

# Remove any stray placeholder dashes and negative-zero strings
est_mat[is.na(est_mat)] <- ""
est_mat[grepl("^\\s*-\\s*$", est_mat)] <- ""
est_mat[est_mat %in% c("-0.00", "-0.0", "-0")] <- "0.00"

# 9 - Create LaTeX lines
latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Probability of episode being seriously pursued after initial phase (OECD Episode Identification)}",
  "\\label{tab:reg4_serious}",
  "\\begin{threeparttable}",
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\renewcommand{\\arraystretch}{1.15}",
  "\\begin{tabular}{@{}l*{14}{>{\\centering\\arraybackslash}m{4.0em}}@{}}",
  "\\toprule",
  paste0(pad_cell("", "l"), " & \\multicolumn{14}{c}{", pad_cell("Regression results", "c"), "} \\\\"),
  "\\cmidrule(lr){2-15}",
  paste0(
    pad_cell("Type of regression:", "l"),
    " & \\multicolumn{14}{l}{",
    pad_cell("Pooled probit regressions (one observation per adjustment episode)", "l"),
    "} \\\\"
  ),
  paste0(
    pad_cell("Left-hand-side variable", "l"),
    " & \\multicolumn{14}{l}{",
    "\\makecell[l]{", cell_strut,
    " Binary variable indicating whether a consolidation episode was seriously \\\\ continued after the initial starting phase}",
    "} \\\\"
  ),
  "\\midrule",
  paste0(
    pad_cell("Explanatory variables", "l"), " & ",
    paste(vapply(names(results), pad_cell, character(1), align = "c"), collapse = " & "),
    " \\\\"
  ),
  "\\midrule"
)

for (i in seq_along(row_order)) {
  row_name <- pad_cell(row_labels[row_order[i]], "l")
  row_cells <- vapply(est_mat[row_order[i], ], pad_cell, character(1), align = "c")
  
  latex_lines <- c(
    latex_lines,
    paste0(row_name, " & ", paste(row_cells, collapse = " & "), " \\\\")
  )
}

obs_cells <- vapply(map_chr(results, ~ as.character(.x$nobs)), pad_cell, character(1), align = "c")
r2_cells  <- vapply(map_chr(results, ~ fmt_num(.x$pseudo_r2, 2)), pad_cell, character(1), align = "c")

latex_lines <- c(
  latex_lines,
  paste0(
    pad_cell("(Constant not reported)", "l"),
    " & ",
    paste(rep(pad_cell("", "c"), length(results)), collapse = " & "),
    " \\\\"
  ),
  "\\midrule",
  paste0(pad_cell("Observations", "l"), " & ", paste(obs_cells, collapse = " & "), " \\\\"),
  paste0(pad_cell("Pseudo R2", "l"), " & ", paste(r2_cells, collapse = " & "), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
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
writeLines(latex_lines, "Output/OECD_reg4.tex")