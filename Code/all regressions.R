#CODE TO RUN ALL REGRESSIONS#

library(readr)
library(dplyr)
library(purrr)
library(lmtest)
library(sandwich)
library(margins)
library(tibble)
library(truncreg)

table_font_lines <- c(
  "\\fontspec{Verdana}",
  "\\fontsize{6}{8}\\selectfont"
)

cell_strut <- "\\rule{0pt}{3.0ex}"

format_latex_table <- function(latex_lines) {
  latex_lines <- gsub("(OECD Episode Identification)", "(CAPB-Based Episode Identification)", latex_lines, fixed = TRUE)
  latex_lines <- gsub("(Exogenous Identification)", "(Narrative-Based Episode Identification)", latex_lines, fixed = TRUE)
  latex_lines <- latex_lines[!latex_lines %in% c("\\small", "\\scriptsize", "\\tiny", "\\footnotesize")]
  centering_idx <- match("\\centering", latex_lines)
  if (!is.na(centering_idx)) {
    latex_lines <- append(latex_lines, table_font_lines, after = centering_idx)
  } else {
    threepart_idx <- match("\\begin{threeparttable}", latex_lines)
    if (!is.na(threepart_idx)) latex_lines <- append(latex_lines, table_font_lines, after = threepart_idx)
  }
  latex_lines
}

stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("\\textsuperscript{***}")
  if (p < 0.05) return("\\textsuperscript{**}")
  if (p < 0.10) return("\\textsuperscript{*}")
  return("")
}

stars_sup <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("\\textsuperscript{***}")
  if (p < 0.05) return("\\textsuperscript{**}")
  if (p < 0.10) return("\\textsuperscript{*}")
  return("")
}

fmt_num <- function(x, digits = 2) {
  if (is.na(x)) return("")
  x <- round(as.numeric(x), digits)
  if (identical(x, -0) || abs(x) < 10^(-digits)) x <- 0
  sprintf(paste0("%.", digits, "f"), x)
}


star_line <- function(p) {
  s <- stars(p)
  if (!nzchar(s)) return("\\phantom{\\textsuperscript{***}}")
  s
}

star_line_sup <- function(p) {
  s <- stars_sup(p)
  if (!nzchar(s)) return("\\phantom{\\textsuperscript{***}}")
  s
}

fmt_est_plain <- function(est, p) {
  if (is.na(est)) return("")
  paste0(
    "\\makecell[c]{",
    "\\rule{0pt}{2.6ex} ", fmt_num(est, 2),
    " \\\\ ",
    "\\rule{0pt}{2.0ex} ", star_line(p),
    "}"
  )
}

fmt_est_math <- function(est, p) {
  if (is.na(est)) return("")
  paste0(
    "\\makecell[c]{",
    "\\rule{0pt}{2.6ex} $", fmt_num(est, 2), "$",
    " \\\\ ",
    "\\rule{0pt}{2.0ex} ", star_line_sup(p),
    "}"
  )
}

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

make_table_mat <- function(results, row_order, value_name = "ame") {
  table_mat <- matrix("", nrow = length(row_order), ncol = length(results))
  rownames(table_mat) <- row_order
  colnames(table_mat) <- names(results)
  for (j in seq_along(results)) {
    tab_j <- results[[j]][[value_name]]
    for (v in row_order) {
      hit <- tab_j %>% filter(factor == v)
      if (nrow(hit) > 0) {
        if (value_name == "ame") table_mat[v, j] <- fmt_est_plain(hit$AME[1], hit$p[1])
        if (value_name == "coef_tab") table_mat[v, j] <- fmt_est_plain(hit$estimate[1], hit$p_value[1])
      }
    }
  }
  table_mat
}

write_table <- function(latex_lines, output_file) {
  dir.create("Output", showWarnings = FALSE)
  latex_lines <- format_latex_table(latex_lines)
  writeLines(latex_lines, output_file)
}

### REGRESSION 1 ###

run_reg1 <- function(input_file, output_file, identification_label) {
  df <- read_csv(input_file, show_col_types = FALSE) %>%
    arrange(country_code, year) %>%
    group_by(country_code) %>%
    mutate(
      episode = as.integer(start == 1 | continue == 1),
      EA_pre1992   = as.numeric(EA_pre1992),
      EA_qual_9297 = as.numeric(EA_qual_9297),
      EA_1998on    = as.numeric(EA_1998on),
      d_IRS  = as.numeric(IRS)  - lag(as.numeric(IRS)),
      d_r_sh = as.numeric(r_sh) - lag(as.numeric(r_sh)),
      d_IRL  = as.numeric(IRL)  - lag(as.numeric(IRL)),
      d_r_lo = as.numeric(r_lo) - lag(as.numeric(r_lo)),
      d_rexr = as.numeric(rexr) - lag(as.numeric(rexr)),
      episode_L1 = lag(episode),
      NEED_ADJ_L1 = lag(as.numeric(NEED_ADJ)),
      GAP_L1      = lag(as.numeric(GAP)),
      d_rexr_L1   = lag(d_rexr),
      d_IRS_L1    = lag(d_IRS),
      d_r_sh_L1   = lag(d_r_sh),
      d_IRL_L1    = lag(d_IRL),
      d_r_lo_L1   = lag(d_r_lo),
      d_taylor_L1 = lag(as.numeric(d_real_short_taylorgap))
    ) %>%
    ungroup() %>%
    filter(year >= 1978, year <= 2014)
  
  reg_base <- df %>% filter(is.na(episode_L1) | episode_L1 == 0)
  
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
    d_rexr_L1     = "Change in real exchange rate (lagged)",
    EA_pre1992    = "Euro-area countries, 1978--1991",
    EA_qual_9297  = "Euro-area countries, 1992--1997",
    EA_1998on     = "Euro-area countries, 1998 onward",
    d_IRS_L1      = "Nominal short-term interest rate (lagged change)",
    d_r_sh_L1     = "Real short-term interest rate (lagged change)",
    d_IRL_L1      = "Nominal long-term interest rate (lagged change)",
    d_r_lo_L1     = "Real long-term interest rate (lagged change)",
    d_taylor_L1   = "Short-term interest rate relative to Taylor rule (lagged change)"
  )
  row_order <- names(row_labels)
  
  run_start_probit <- function(data, include_gap = TRUE, money_var = NULL) {
    vars_keep <- c("country_code", "year", "start", "NEED_ADJ_L1", "d_rexr_L1", "EA_pre1992", "EA_qual_9297", "EA_1998on")
    if (include_gap) vars_keep <- c(vars_keep, "GAP_L1")
    if (!is.null(money_var)) vars_keep <- c(vars_keep, money_var)
    reg_df <- data %>% select(all_of(vars_keep)) %>% filter(complete.cases(.))
    if (!is.null(money_var)) reg_df <- reg_df %>% rename(monetary_L1 = all_of(money_var))
    if (include_gap && is.null(money_var)) {
      model <- glm(start ~ NEED_ADJ_L1 + GAP_L1 + d_rexr_L1 + EA_pre1992 + EA_qual_9297 + EA_1998on, data = reg_df, family = binomial(link = "probit"))
    } else if (!include_gap && !is.null(money_var)) {
      model <- glm(start ~ NEED_ADJ_L1 + d_rexr_L1 + EA_pre1992 + EA_qual_9297 + EA_1998on + monetary_L1, data = reg_df, family = binomial(link = "probit"))
    } else {
      stop("Invalid specification: choose either output gap or one monetary variable.")
    }
    vcov_cl <- vcovCL(model, cluster = reg_df$country_code, type = "HC1")
    ame <- summary(margins(model, vcov = vcov_cl)) %>% as_tibble()
    if (!is.null(money_var)) ame <- ame %>% mutate(factor = ifelse(factor == "monetary_L1", money_var, factor))
    pseudo_r2 <- 1 - as.numeric(logLik(model) / logLik(update(model, . ~ 1)))
    list(model = model, ame = ame, nobs = nobs(model), pseudo_r2 = pseudo_r2)
  }
  
  results <- imap(specs, ~ run_start_probit(data = reg_base, include_gap = .x$include_gap, money_var = .x$money_var))
  table_mat <- make_table_mat(results, row_order, "ame")
  
  latex_lines <- c(
    "\\begin{table}[!htbp]", "\\centering",
    paste0("\\caption{Probability that a consolidation episode is started (", identification_label, ")}"),
    "\\label{tab:reg1_start}", "\\begin{threeparttable}", "\\small",
    "\\setlength{\\tabcolsep}{4pt}", "\\renewcommand{\\arraystretch}{1.10}",
    "\\begin{tabularx}{\\linewidth}{@{}>{\\raggedright\\arraybackslash}p{0.28\\linewidth}YYYYYY@{}}",
    "\\toprule", " & \\multicolumn{6}{c}{Regression results} \\\\", "\\cmidrule(lr){2-7}",
    "Type of regression: & \\multicolumn{6}{l}{Pooled probit regressions} \\\\",
    "Left-hand-side variable & \\multicolumn{6}{l}{Binary variable indicating exogenous start of an adjustment episode} \\\\",
    "\\midrule", "Explanatory variables & 1 & 2 & 3 & 4 & 5 & 6 \\\\", "\\midrule"
  )
  for (i in seq_along(row_order)) latex_lines <- c(latex_lines, paste0(row_labels[row_order[i]], " & ", paste(table_mat[row_order[i], ], collapse = " & "), " \\\\"))
  obs_line <- paste("Observations", paste(map_chr(results, ~ as.character(.x$nobs)), collapse = " & "), sep = " & ")
  r2_line <- paste("Pseudo R2", paste(map_chr(results, ~ fmt_num(.x$pseudo_r2, 2)), collapse = " & "), sep = " & ")
  latex_lines <- c(latex_lines, "(constant not reported) &  &  &  &  &  &  \\\\", "\\midrule", paste0(obs_line, " \\\\"), paste0(r2_line, " \\\\"), "\\bottomrule", "\\end{tabularx}", "\\begin{tablenotes}[flushleft]", "\\footnotesize", "\\item Reported coefficients are average marginal effects.", "\\item $^{*}$ significant at 10\\%; $^{**}$ significant at 5\\%; $^{***}$ significant at 1\\%.", "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  write_table(latex_lines, output_file)
}

run_reg1("Data/Clean Data/full_gen.csv", "Output/exo_reg1.tex", "Narrative-Based Episode Identification")
run_reg1("Data/Clean Data/full_OECD_dataset.csv", "Output/OECD_reg1.tex", "CAPB-Based Episode Identification")

### REGRESSION 2 ###

run_reg2 <- function(input_file, output_file, identification_label) {
  df <- read_csv(input_file, show_col_types = FALSE) %>%
    arrange(country_code, year) %>%
    group_by(country_code) %>%
    mutate(
      episode = as.integer(start == 1 | continue == 1),
      EA_pre1992   = as.numeric(EA_pre1992),
      EA_qual_9297 = as.numeric(EA_qual_9297),
      EA_1998on    = as.numeric(EA_1998on),
      d_IRS  = as.numeric(IRS)  - lag(as.numeric(IRS)),
      d_r_sh = as.numeric(r_sh) - lag(as.numeric(r_sh)),
      d_IRL  = as.numeric(IRL)  - lag(as.numeric(IRL)),
      d_r_lo = as.numeric(r_lo) - lag(as.numeric(r_lo)),
      rexr_hist_avg = mean(as.numeric(rexr), na.rm = TRUE),
      rexr_rel_hist = 100 * ((as.numeric(rexr) / rexr_hist_avg) - 1),
      episode_L1 = lag(episode),
      NEED_ADJ_L1      = lag(as.numeric(NEED_ADJ)),
      GAP_L1           = lag(as.numeric(GAP)),
      rexr_rel_hist_L1 = lag(rexr_rel_hist),
      d_IRS_L1    = lag(d_IRS),
      d_r_sh_L1   = lag(d_r_sh),
      d_IRL_L1    = lag(d_IRL),
      d_r_lo_L1   = lag(d_r_lo),
      d_taylor_L1 = lag(as.numeric(d_real_short_taylorgap)),
      share_exp_cuts_L1   = lag(as.numeric(share_exp_cuts)),
      share_capex_cuts_L1 = lag(as.numeric(share_capex_cuts))
    ) %>%
    ungroup() %>%
    filter(year >= 1978, year <= 2014)
  
  reg_base <- df %>% filter(episode_L1 == 1)
  
  specs <- list(
    "1"  = list(money_var = NULL,          comp_var = NULL),
    "2"  = list(money_var = "d_IRS_L1",    comp_var = NULL),
    "3"  = list(money_var = "d_r_sh_L1",   comp_var = NULL),
    "4"  = list(money_var = "d_IRL_L1",    comp_var = NULL),
    "5"  = list(money_var = "d_r_lo_L1",   comp_var = NULL),
    "6"  = list(money_var = "d_taylor_L1", comp_var = NULL),
    "7"  = list(money_var = "d_IRS_L1",    comp_var = "share_exp_cuts_L1"),
    "8"  = list(money_var = "d_r_sh_L1",   comp_var = "share_exp_cuts_L1"),
    "9"  = list(money_var = "d_IRL_L1",    comp_var = "share_exp_cuts_L1"),
    "10" = list(money_var = "d_r_lo_L1",   comp_var = "share_exp_cuts_L1"),
    "11" = list(money_var = "d_taylor_L1", comp_var = "share_exp_cuts_L1"),
    "12" = list(money_var = NULL,          comp_var = "share_capex_cuts_L1")
  )
  
  row_labels <- c(
    NEED_ADJ_L1         = "Need for adjustment (lagged)",
    GAP_L1              = "Output gap (lagged)",
    rexr_rel_hist_L1    = "Real exchange rate relative to historical average (lagged)",
    EA_pre1992          = "Euro-area countries, 1978--1991",
    EA_qual_9297        = "Euro-area countries, 1992--1997",
    EA_1998on           = "Euro-area countries, 1998 onward",
    d_IRS_L1            = "Nominal short-term interest rate (lagged change)",
    d_r_sh_L1           = "Real short-term interest rate (lagged change)",
    d_IRL_L1            = "Nominal long-term interest rate (lagged change)",
    d_r_lo_L1           = "Real long-term interest rate (lagged change)",
    d_taylor_L1         = "Short-term interest rate relative to Taylor rule (lagged change)",
    share_exp_cuts_L1   = "Share of expenditure cuts in total adjustment (lagged)",
    share_capex_cuts_L1 = "Share of capital-expenditure cuts in total adjustment (lagged)"
  )
  row_order <- names(row_labels)
  
  run_continue_probit <- function(data, money_var = NULL, comp_var = NULL) {
    vars_keep <- c("country_code", "year", "continue", "NEED_ADJ_L1", "GAP_L1", "rexr_rel_hist_L1", "EA_pre1992", "EA_qual_9297", "EA_1998on")
    if (!is.null(money_var)) vars_keep <- c(vars_keep, money_var)
    if (!is.null(comp_var)) vars_keep <- c(vars_keep, comp_var)
    reg_df <- data %>% select(all_of(vars_keep)) %>% filter(complete.cases(.))
    if (!is.null(money_var)) reg_df <- reg_df %>% rename(monetary_L1 = all_of(money_var))
    if (!is.null(comp_var)) reg_df <- reg_df %>% rename(composition_L1 = all_of(comp_var))
    rhs_vars <- c("NEED_ADJ_L1", "GAP_L1", "rexr_rel_hist_L1", "EA_pre1992", "EA_qual_9297", "EA_1998on")
    if (!is.null(money_var)) rhs_vars <- c(rhs_vars, "monetary_L1")
    if (!is.null(comp_var)) rhs_vars <- c(rhs_vars, "composition_L1")
    model <- glm(as.formula(paste("continue ~", paste(rhs_vars, collapse = " + "))), data = reg_df, family = binomial(link = "probit"))
    vcov_cl <- vcovCL(model, cluster = reg_df$country_code, type = "HC1")
    ame <- summary(margins(model, vcov = vcov_cl)) %>% as_tibble()
    if (!is.null(money_var)) ame <- ame %>% mutate(factor = ifelse(factor == "monetary_L1", money_var, factor))
    if (!is.null(comp_var)) ame <- ame %>% mutate(factor = ifelse(factor == "composition_L1", comp_var, factor))
    pseudo_r2 <- 1 - as.numeric(logLik(model) / logLik(update(model, . ~ 1)))
    list(model = model, ame = ame, nobs = nobs(model), pseudo_r2 = pseudo_r2)
  }
  
  results <- imap(specs, ~ run_continue_probit(data = reg_base, money_var = .x$money_var, comp_var = .x$comp_var))
  table_mat <- make_table_mat(results, row_order, "ame")
  
  latex_lines <- c(
    "\\begin{table}[!htbp]", "\\centering",
    paste0("\\caption{Probability that adjustment is continued from previous year (", identification_label, ")}"),
    "\\label{tab:reg2_continue}", "\\begin{threeparttable}", "\\scriptsize",
    "\\setlength{\\tabcolsep}{2.5pt}", "\\renewcommand{\\arraystretch}{1.10}",
    "\\begin{tabularx}{\\linewidth}{@{}>{\\raggedright\\arraybackslash}p{0.20\\linewidth}YYYYYYYYYYYY@{}}",
    "\\toprule", " & \\multicolumn{12}{c}{Regression results} \\\\", "\\cmidrule(lr){2-13}",
    "Type of regression: & \\multicolumn{12}{l}{Pooled probit regressions} \\\\",
    "Left-hand-side variable & \\multicolumn{12}{l}{Binary variable indicating continuation of an ongoing adjustment episode} \\\\",
    "\\midrule", "Explanatory variables & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 & 11 & 12 \\\\", "\\midrule"
  )
  for (i in seq_along(row_order)) latex_lines <- c(latex_lines, paste0(row_labels[row_order[i]], " & ", paste(table_mat[row_order[i], ], collapse = " & "), " \\\\"))
  obs_line <- paste("Observations", paste(map_chr(results, ~ as.character(.x$nobs)), collapse = " & "), sep = " & ")
  r2_line <- paste("Pseudo R2", paste(map_chr(results, ~ fmt_num(.x$pseudo_r2, 2)), collapse = " & "), sep = " & ")
  latex_lines <- c(latex_lines, "(constant not reported) &  &  &  &  &  &  &  &  &  &  &  &  \\\\", "\\midrule", paste0(obs_line, " \\\\"), paste0(r2_line, " \\\\"), "\\bottomrule", "\\end{tabularx}", "\\begin{tablenotes}[flushleft]", "\\footnotesize", "\\item Reported coefficients are average marginal effects.", "\\item $^{*}$ significant at 10\\%; $^{**}$ significant at 5\\%; $^{***}$ significant at 1\\%.", "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  write_table(latex_lines, output_file)
}

run_reg2("Data/Clean Data/full_gen.csv", "Output/exo_reg2.tex", "Narrative-Based Episode Identification")
run_reg2("Data/Clean Data/full_OECD_dataset.csv", "Output/OECD_reg2.tex", "CAPB-Based Episode Identification")

### REGRESSION 3 ###

run_reg3 <- function(input_file, output_file, identification_label) {
  df <- read_csv(input_file, show_col_types = FALSE) %>%
    arrange(country_code, year) %>%
    group_by(country_code) %>%
    mutate(
      episode = as.integer(start == 1 | continue == 1),
      EA_pre1992   = as.numeric(EA_pre1992),
      EA_qual_9297 = as.numeric(EA_qual_9297),
      EA_1998on    = as.numeric(EA_1998on),
      EA_excl_9297 = as.numeric(EA_pre1992 == 1 | EA_1998on == 1),
      d_CAPB_pot = as.numeric(NLGXQA) - lag(as.numeric(NLGXQA)),
      d_IRS  = as.numeric(IRS)  - lag(as.numeric(IRS)),
      d_r_sh = as.numeric(r_sh) - lag(as.numeric(r_sh)),
      d_IRL  = as.numeric(IRL)  - lag(as.numeric(IRL)),
      d_r_lo = as.numeric(r_lo) - lag(as.numeric(r_lo)),
      rexr_hist_avg = mean(as.numeric(rexr), na.rm = TRUE),
      rexr_rel_hist = 100 * ((as.numeric(rexr) / rexr_hist_avg) - 1),
      episode_L1 = lag(episode),
      NEED_ADJ_L1      = lag(as.numeric(NEED_ADJ)),
      GAP_L1           = lag(as.numeric(GAP)),
      rexr_rel_hist_L1 = lag(rexr_rel_hist),
      d_IRS_L1    = lag(d_IRS),
      d_r_sh_L1   = lag(d_r_sh),
      d_IRL_L1    = lag(d_IRL),
      d_r_lo_L1   = lag(d_r_lo),
      d_taylor_L1 = lag(as.numeric(d_real_short_taylorgap)),
      share_exp_cuts_L1   = lag(as.numeric(share_exp_cuts)),
      share_capex_cuts_L1 = lag(as.numeric(share_capex_cuts))
    ) %>%
    ungroup() %>%
    filter(year >= 1978, year <= 2014)
  
  reg_base <- df %>% filter(episode_L1 == 1)
  
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
  
  pool_specs <- list(
    "15" = list(ea_vars = c("EA_excl_9297", "EA_qual_9297")),
    "16" = list(ea_vars = c("EA_pre1992", "EA_qual_9297", "EA_1998on"))
  )
  
  row_labels <- c(
    NEED_ADJ_L1         = "Need for adjustment (lagged)",
    GAP_L1              = "Output gap (lagged)",
    rexr_rel_hist_L1    = "Real exchange rate relative to historical average (lagged)",
    EA_excl_9297        = "Euro-area countries, excluding 1992--1997",
    EA_pre1992          = "Euro-area countries, 1978--1991",
    EA_qual_9297        = "Euro-area countries, 1992--1997",
    EA_1998on           = "Euro-area countries, 1998 onward",
    d_IRS_L1            = "Nominal short-term interest rate (lagged change)",
    d_r_sh_L1           = "Real short-term interest rate (lagged change)",
    d_IRL_L1            = "Nominal long-term interest rate (lagged change)",
    d_r_lo_L1           = "Real long-term interest rate (lagged change)",
    d_taylor_L1         = "Short-term interest rate relative to Taylor rule (lagged change)",
    share_exp_cuts_L1   = "Share of expenditure cuts in total adjustment (lagged)",
    share_capex_cuts_L1 = "Share of capital-expenditure cuts in total adjustment (lagged)"
  )
  row_order <- names(row_labels)
  
  run_size_fe <- function(data, include_rer = FALSE, money_var = NULL, comp_var = NULL) {
    vars_keep <- c("country_code", "year", "d_CAPB_pot", "NEED_ADJ_L1", "GAP_L1")
    if (include_rer) vars_keep <- c(vars_keep, "rexr_rel_hist_L1")
    if (!is.null(money_var)) vars_keep <- c(vars_keep, money_var)
    if (!is.null(comp_var)) vars_keep <- c(vars_keep, comp_var)
    reg_df <- data %>% select(all_of(vars_keep)) %>% filter(complete.cases(.))
    if (!is.null(money_var)) reg_df <- reg_df %>% rename(monetary_L1 = all_of(money_var))
    if (!is.null(comp_var)) reg_df <- reg_df %>% rename(composition_L1 = all_of(comp_var))
    rhs_vars <- c("NEED_ADJ_L1", "GAP_L1")
    if (include_rer) rhs_vars <- c(rhs_vars, "rexr_rel_hist_L1")
    if (!is.null(money_var)) rhs_vars <- c(rhs_vars, "monetary_L1")
    if (!is.null(comp_var)) rhs_vars <- c(rhs_vars, "composition_L1")
    model <- lm(as.formula(paste("d_CAPB_pot ~", paste(c(rhs_vars, "factor(country_code)"), collapse = " + "))), data = reg_df)
    vcov_cl <- vcovCL(model, cluster = reg_df$country_code, type = "HC1")
    ct <- coeftest(model, vcov. = vcov_cl)
    coef_tab <- tibble(factor = rownames(ct), estimate = unname(ct[, 1]), stat = unname(ct[, 3]), p_value = unname(ct[, 4]))
    if (!is.null(money_var)) coef_tab <- coef_tab %>% mutate(factor = ifelse(factor == "monetary_L1", money_var, factor))
    if (!is.null(comp_var)) coef_tab <- coef_tab %>% mutate(factor = ifelse(factor == "composition_L1", comp_var, factor))
    coef_tab <- coef_tab %>% filter(factor %in% row_order)
    list(model = model, coef_tab = coef_tab, nobs = nobs(model), ngroups = n_distinct(reg_df$country_code), adj_r2 = summary(model)$adj.r.squared)
  }
  
  run_size_pool <- function(data, ea_vars) {
    vars_keep <- c("country_code", "year", "d_CAPB_pot", "NEED_ADJ_L1", "GAP_L1", ea_vars)
    reg_df <- data %>% select(all_of(vars_keep)) %>% filter(complete.cases(.))
    model <- lm(as.formula(paste("d_CAPB_pot ~", paste(c("NEED_ADJ_L1", "GAP_L1", ea_vars), collapse = " + "))), data = reg_df)
    vcov_cl <- vcovCL(model, cluster = reg_df$country_code, type = "HC1")
    ct <- coeftest(model, vcov. = vcov_cl)
    coef_tab <- tibble(factor = rownames(ct), estimate = unname(ct[, 1]), stat = unname(ct[, 3]), p_value = unname(ct[, 4])) %>% filter(factor %in% row_order)
    list(model = model, coef_tab = coef_tab, nobs = nobs(model), ngroups = NA_integer_, adj_r2 = summary(model)$adj.r.squared)
  }
  
  results_fe <- imap(fe_specs, ~ run_size_fe(data = reg_base, include_rer = .x$include_rer, money_var = .x$money_var, comp_var = .x$comp_var))
  results_pool <- imap(pool_specs, ~ run_size_pool(data = reg_base, ea_vars = .x$ea_vars))
  results <- c(results_fe, results_pool)
  table_mat <- make_table_mat(results, row_order, "coef_tab")
  
  tab_cols <- paste(rep("Y", length(results)), collapse = "")
  results_header <- paste(names(results), collapse = " & ")
  constant_line <- paste(c("(Constant not reported)", rep("", length(results))), collapse = " & ")
  latex_lines <- c(
    "\\begin{table}[!htbp]", "\\centering",
    paste0("\\caption{Size of yearly fiscal adjustment (", identification_label, ")}"),
    "\\label{tab:reg3_size}", "\\begin{threeparttable}", "\\tiny",
    "\\setlength{\\tabcolsep}{0.25pt}", "\\renewcommand{\\arraystretch}{1.02}",
    "\\begin{tabular}{@{}>{\\raggedright\\arraybackslash}p{0.13\\linewidth}*{16}{>{\\centering\\arraybackslash}p{0.052\\linewidth}}@{}}",
    "\\toprule", " & \\multicolumn{14}{c}{Regression results} & \\multicolumn{2}{c}{ } \\\\",
    "\\cmidrule(lr){2-15}\\cmidrule(lr){16-17}",
    "Type of regression: & \\multicolumn{14}{l}{Fixed-effect panel regressions} & \\multicolumn{2}{l}{Pooled OLS regressions} \\\\",
    "Left-hand-side variable & \\multicolumn{16}{l}{Change in the primary adjusted fiscal balance (by year)} \\\\",
    "\\midrule", paste0("Explanatory variables & ", results_header, " \\\\"), "\\midrule"
  )
  for (i in seq_along(row_order)) latex_lines <- c(latex_lines, paste0(row_labels[row_order[i]], " & ", paste(table_mat[row_order[i], ], collapse = " & "), " \\\\"))
  obs_line <- paste("Observations", paste(map_chr(results, ~ as.character(.x$nobs)), collapse = " & "), sep = " & ")
  groups_line <- paste("Number of groups", paste(map_chr(results, ~ ifelse(is.na(.x$ngroups), "", as.character(.x$ngroups))), collapse = " & "), sep = " & ")
  r2_line <- paste("Adjusted R2", paste(map_chr(results, ~ fmt_num(.x$adj_r2, 2)), collapse = " & "), sep = " & ")
  latex_lines <- c(latex_lines, paste0(constant_line, " \\\\"), "\\midrule", paste0(obs_line, " \\\\"), paste0(groups_line, " \\\\"), paste0(r2_line, " \\\\"), "\\bottomrule", "\\end{tabular}", "\\begin{tablenotes}[flushleft]", "\\footnotesize", "\\item $^{*}$ significant at 10\\%; $^{**}$ significant at 5\\%; $^{***}$ significant at 1\\%.", "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  write_table(latex_lines, output_file)
}

run_reg3("Data/Clean Data/full_gen.csv", "Output/exo_reg3.tex", "Narrative-Based Episode Identification")
run_reg3("Data/Clean Data/full_OECD_dataset.csv", "Output/OECD_reg3.tex", "CAPB-Based Episode Identification")

### REGRESSION 4 ###

run_reg4 <- function(input_file, output_file, identification_label) {
  df <- read_csv(input_file, show_col_types = FALSE) %>%
    arrange(country_code, year) %>%
    group_by(country_code) %>%
    mutate(
      EA_pre1992   = as.numeric(EA_pre1992),
      EA_qual_9297 = as.numeric(EA_qual_9297),
      EA_1998on    = as.numeric(EA_1998on),
      d_CAPB_pot = as.numeric(NLGXQA) - lag(as.numeric(NLGXQA)),
      d_IRS  = as.numeric(IRS)  - lag(as.numeric(IRS)),
      d_r_sh = as.numeric(r_sh) - lag(as.numeric(r_sh)),
      d_IRL  = as.numeric(IRL)  - lag(as.numeric(IRL)),
      d_r_lo = as.numeric(r_lo) - lag(as.numeric(r_lo)),
      rexr_hist_avg = mean(as.numeric(rexr), na.rm = TRUE),
      rexr_rel_hist = 100 * ((as.numeric(rexr) / rexr_hist_avg) - 1),
      start_phase_len = case_when(
        start == 1 & !is.na(d_CAPB_pot) & d_CAPB_pot >= 1 ~ 1L,
        start == 1 & !is.na(d_CAPB_pot) & !is.na(lead(d_CAPB_pot, 1)) & d_CAPB_pot >= 0.5 & lead(d_CAPB_pot, 1) >= 0.5 & (d_CAPB_pot + lead(d_CAPB_pot, 1)) >= 1 ~ 2L,
        TRUE ~ NA_integer_
      ),
      NEED_ADJ_end = case_when(start_phase_len == 1L ~ as.numeric(NEED_ADJ), start_phase_len == 2L ~ lead(as.numeric(NEED_ADJ), 1), TRUE ~ NA_real_),
      GAP_end = case_when(start_phase_len == 1L ~ as.numeric(GAP), start_phase_len == 2L ~ lead(as.numeric(GAP), 1), TRUE ~ NA_real_),
      rexr_rel_hist_end = case_when(start_phase_len == 1L ~ rexr_rel_hist, start_phase_len == 2L ~ lead(rexr_rel_hist, 1), TRUE ~ NA_real_),
      EA_pre1992_end = case_when(start_phase_len == 1L ~ EA_pre1992, start_phase_len == 2L ~ lead(EA_pre1992, 1), TRUE ~ NA_real_),
      EA_qual_9297_end = case_when(start_phase_len == 1L ~ EA_qual_9297, start_phase_len == 2L ~ lead(EA_qual_9297, 1), TRUE ~ NA_real_),
      EA_1998on_end = case_when(start_phase_len == 1L ~ EA_1998on, start_phase_len == 2L ~ lead(EA_1998on, 1), TRUE ~ NA_real_),
      d_IRS_startavg = case_when(start_phase_len == 1L ~ d_IRS, start_phase_len == 2L ~ (d_IRS + lead(d_IRS, 1)) / 2, TRUE ~ NA_real_),
      d_r_sh_startavg = case_when(start_phase_len == 1L ~ d_r_sh, start_phase_len == 2L ~ (d_r_sh + lead(d_r_sh, 1)) / 2, TRUE ~ NA_real_),
      d_IRL_startavg = case_when(start_phase_len == 1L ~ d_IRL, start_phase_len == 2L ~ (d_IRL + lead(d_IRL, 1)) / 2, TRUE ~ NA_real_),
      d_r_lo_startavg = case_when(start_phase_len == 1L ~ d_r_lo, start_phase_len == 2L ~ (d_r_lo + lead(d_r_lo, 1)) / 2, TRUE ~ NA_real_),
      d_taylor_startavg = case_when(start_phase_len == 1L ~ as.numeric(d_real_short_taylorgap), start_phase_len == 2L ~ (as.numeric(d_real_short_taylorgap) + lead(as.numeric(d_real_short_taylorgap), 1)) / 2, TRUE ~ NA_real_),
      CAPB_pre_start = lag(as.numeric(NLGXQA)),
      CAPB_end_startphase = case_when(start_phase_len == 1L ~ as.numeric(NLGXQA), start_phase_len == 2L ~ lead(as.numeric(NLGXQA), 1), TRUE ~ NA_real_),
      cum_dCAPB_startphase = CAPB_end_startphase - CAPB_pre_start,
      currentexp_pre_start = lag(as.numeric(currentexp_pot)),
      currentexp_end_startphase = case_when(start_phase_len == 1L ~ as.numeric(currentexp_pot), start_phase_len == 2L ~ lead(as.numeric(currentexp_pot), 1), TRUE ~ NA_real_),
      capex_pre_start = lag(as.numeric(capex_pot)),
      capex_end_startphase = case_when(start_phase_len == 1L ~ as.numeric(capex_pot), start_phase_len == 2L ~ lead(as.numeric(capex_pot), 1), TRUE ~ NA_real_),
      share_exp_cuts_startphase = case_when(!is.na(cum_dCAPB_startphase) & cum_dCAPB_startphase > 0 ~ (currentexp_pre_start - currentexp_end_startphase) / cum_dCAPB_startphase, TRUE ~ NA_real_),
      share_capex_cuts_startphase = case_when(!is.na(cum_dCAPB_startphase) & cum_dCAPB_startphase > 0 ~ (capex_pre_start - capex_end_startphase) / cum_dCAPB_startphase, TRUE ~ NA_real_)
    ) %>%
    ungroup() %>%
    filter(year >= 1978, year <= 2014)
  
  reg_base <- df %>%
    filter(start == 1, !is.na(start_phase_len)) %>%
    transmute(country_code, year, serious = as.integer(serious), NEED_ADJ_end, GAP_end, rexr_rel_hist_end, EA_pre1992_end, EA_qual_9297_end, EA_1998on_end, d_IRS_startavg, d_r_sh_startavg, d_IRL_startavg, d_r_lo_startavg, d_taylor_startavg, share_exp_cuts_startphase, share_capex_cuts_startphase)
  
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
    NEED_ADJ_end                = "Need for adjustment (end of starting phase)",
    GAP_end                     = "Output gap (end of starting phase)",
    rexr_rel_hist_end           = "Real exchange rate relative to historical average (end of starting phase)",
    EA_pre1992_end              = "Euro-area countries, 1978--1991",
    EA_qual_9297_end            = "Euro-area countries, 1992--1997",
    EA_1998on_end               = "Euro-area countries, 1998 onward",
    d_IRS_startavg              = "Nominal short-term interest rate (average change during starting phase)",
    d_r_sh_startavg             = "Real short-term interest rate (average change during starting phase)",
    d_IRL_startavg              = "Nominal long-term interest rate (average change during starting phase)",
    d_r_lo_startavg             = "Real long-term interest rate (average change during starting phase)",
    d_taylor_startavg           = "Short-term interest rate relative to Taylor rule (average change during starting phase)",
    share_exp_cuts_startphase   = "Share of expenditure cuts in total adjustment (starting phase)",
    share_capex_cuts_startphase = "Share of capital-expenditure cuts in total adjustment (starting phase)"
  )
  row_order <- names(row_labels)
  
  run_serious_probit <- function(data, include_rer = FALSE, money_var = NULL, comp_var = NULL) {
    vars_keep <- c("country_code", "year", "serious", "NEED_ADJ_end", "GAP_end", "EA_pre1992_end", "EA_qual_9297_end", "EA_1998on_end")
    if (include_rer) vars_keep <- c(vars_keep, "rexr_rel_hist_end")
    if (!is.null(money_var)) vars_keep <- c(vars_keep, money_var)
    if (!is.null(comp_var)) vars_keep <- c(vars_keep, comp_var)
    reg_df <- data %>% select(all_of(vars_keep)) %>% filter(complete.cases(.))
    if (!is.null(money_var)) reg_df <- reg_df %>% rename(monetary_var = all_of(money_var))
    if (!is.null(comp_var)) reg_df <- reg_df %>% rename(composition_var = all_of(comp_var))
    rhs_vars <- c("NEED_ADJ_end", "GAP_end", "EA_pre1992_end", "EA_qual_9297_end", "EA_1998on_end")
    if (include_rer) rhs_vars <- c(rhs_vars, "rexr_rel_hist_end")
    if (!is.null(money_var)) rhs_vars <- c(rhs_vars, "monetary_var")
    if (!is.null(comp_var)) rhs_vars <- c(rhs_vars, "composition_var")
    model <- glm(as.formula(paste("serious ~", paste(rhs_vars, collapse = " + "))), data = reg_df, family = binomial(link = "probit"))
    vcov_cl <- vcovCL(model, cluster = reg_df$country_code, type = "HC1")
    ame <- summary(margins(model, vcov = vcov_cl)) %>% as_tibble()
    if (!is.null(money_var)) ame <- ame %>% mutate(factor = ifelse(factor == "monetary_var", money_var, factor))
    if (!is.null(comp_var)) ame <- ame %>% mutate(factor = ifelse(factor == "composition_var", comp_var, factor))
    pseudo_r2 <- 1 - as.numeric(logLik(model) / logLik(update(model, . ~ 1)))
    list(model = model, ame = ame, nobs = nobs(model), pseudo_r2 = pseudo_r2)
  }
  
  results <- imap(specs, ~ run_serious_probit(data = reg_base, include_rer = .x$include_rer, money_var = .x$money_var, comp_var = .x$comp_var))
  est_mat <- matrix("", nrow = length(row_order), ncol = length(results))
  rownames(est_mat) <- row_order
  colnames(est_mat) <- names(results)
  for (j in seq_along(results)) {
    ame_j <- results[[j]]$ame
    for (v in row_order) {
      hit <- ame_j %>% filter(factor == v)
      if (nrow(hit) > 0) est_mat[v, j] <- fmt_est_plain(hit$AME[1], hit$p[1])
    }
  }
  est_mat[is.na(est_mat)] <- ""
  est_mat[grepl("^\\s*-\\s*$", est_mat)] <- ""
  est_mat[est_mat %in% c("-0.00", "-0.0", "-0")] <- "0.00"
  
  label_cell <- function(x) {
    if (is.na(x) || !nzchar(trimws(x))) return(paste0("\\parbox[t]{\\linewidth}{", cell_strut, "}"))
    paste0("\\parbox[t]{\\linewidth}{\\raggedright ", cell_strut, " ", x, "}")
  }
  
  result_cell <- function(x) {
    if (is.na(x) || !nzchar(trimws(x))) return(paste0("\\makecell[c]{", cell_strut, "}"))
    if (grepl("^\\\\makecell", x)) return(x)
    paste0("\\makecell[c]{", cell_strut, " ", x, "}")
  }
  
  latex_lines <- c(
    "\\begin{table}[!htbp]", "\\centering",
    paste0("\\caption{Probability of episode being seriously pursued after initial phase (", identification_label, ")}"),
    "\\label{tab:reg4_serious}", "\\begin{threeparttable}", "\\scriptsize",
    "\\setlength{\\tabcolsep}{0.8pt}", "\\renewcommand{\\arraystretch}{1.25}",
    "\\begin{tabular}{@{}>{\\raggedright\\arraybackslash}p{0.25\\linewidth}*{14}{>{\\centering\\arraybackslash}m{0.045\\linewidth}}@{}}",
    "\\toprule",
    paste0(label_cell(""), " & \\multicolumn{14}{c}{", result_cell("Regression results"), "} \\\\"),
    "\\cmidrule(lr){2-15}",
    paste0(
      label_cell("Type of regression:"),
      " & \\multicolumn{14}{>{\\raggedright\\arraybackslash}p{0.63\\linewidth}}{",
      "Pooled probit regressions (one observation per adjustment episode)",
      "} \\\\"
    ),
    paste0(
      label_cell("Left-hand-side variable"),
      " & \\multicolumn{14}{>{\\raggedright\\arraybackslash}p{0.63\\linewidth}}{",
      "Binary variable indicating whether a consolidation episode was seriously continued after the initial starting phase",
      "} \\\\[0.5ex]"
    ),
    "\\midrule",
    paste0(label_cell("Explanatory variables"), " & ", paste(vapply(names(results), result_cell, character(1)), collapse = " & "), " \\\\"),
    "\\midrule"
  )
  for (i in seq_along(row_order)) latex_lines <- c(latex_lines, paste0(label_cell(row_labels[row_order[i]]), " & ", paste(vapply(est_mat[row_order[i], ], result_cell, character(1)), collapse = " & "), " \\\\"))
  obs_cells <- vapply(map_chr(results, ~ as.character(.x$nobs)), result_cell, character(1))
  r2_cells  <- vapply(map_chr(results, ~ fmt_num(.x$pseudo_r2, 2)), result_cell, character(1))
  latex_lines <- c(latex_lines, paste0(label_cell("(Constant not reported)"), " & ", paste(rep(result_cell(""), length(results)), collapse = " & "), " \\\\"), "\\midrule", paste0(label_cell("Observations"), " & ", paste(obs_cells, collapse = " & "), " \\\\"), paste0(label_cell("Pseudo R2"), " & ", paste(r2_cells, collapse = " & "), " \\\\"), "\\bottomrule", "\\end{tabular}", "\\begin{tablenotes}[flushleft]", "\\footnotesize", "\\item Reported coefficients are average marginal effects.", "\\item $^{*}$ significant at 10\\%; $^{**}$ significant at 5\\%; $^{***}$ significant at 1\\%.", "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  write_table(latex_lines, output_file)
}

run_reg4("Data/Clean Data/full_gen.csv", "Output/exo_reg4.tex", "Narrative-Based Episode Identification")
run_reg4("Data/Clean Data/full_OECD_dataset.csv", "Output/OECD_reg4.tex", "CAPB-Based Episode Identification")

make_episode_start_df <- function(input_file) {
  df <- read_csv(input_file, show_col_types = FALSE) %>%
    arrange(country_code, year) %>%
    group_by(country_code) %>%
    mutate(
      EA_pre1992   = as.numeric(EA_pre1992),
      EA_qual_9297 = as.numeric(EA_qual_9297),
      EA_1998on    = as.numeric(EA_1998on),
      d_IRS  = as.numeric(IRS)  - lag(as.numeric(IRS)),
      d_r_sh = as.numeric(r_sh) - lag(as.numeric(r_sh)),
      d_IRL  = as.numeric(IRL)  - lag(as.numeric(IRL)),
      d_r_lo = as.numeric(r_lo) - lag(as.numeric(r_lo)),
      rexr_hist_avg = mean(as.numeric(rexr), na.rm = TRUE),
      rexr_rel_hist = 100 * ((as.numeric(rexr) / rexr_hist_avg) - 1),
      NEED_ADJ_pre_at_start      = if_else(start == 1, lag(as.numeric(NEED_ADJ)), NA_real_),
      GAP_pre_at_start           = if_else(start == 1, lag(as.numeric(GAP)), NA_real_),
      rexr_rel_hist_pre_at_start = if_else(start == 1, lag(rexr_rel_hist), NA_real_),
      EA_pre1992_at_start   = if_else(start == 1, EA_pre1992,   NA_real_),
      EA_qual_9297_at_start = if_else(start == 1, EA_qual_9297, NA_real_),
      EA_1998on_at_start    = if_else(start == 1, EA_1998on,    NA_real_)
    ) %>%
    ungroup() %>%
    filter(year >= 1978, year <= 2014)
  
  df %>%
    filter(!is.na(episode_id)) %>%
    group_by(country_code, episode_id) %>%
    summarise(
      cum_adjustment = first_non_missing(cum_dCAPB_episode),
      decel_debt_ep = first_non_missing(decel_debt),
      episode_length_ep = first_non_missing(episode_length),
      NEED_ADJ_pre      = first_non_missing(NEED_ADJ_pre_at_start),
      GAP_pre           = first_non_missing(GAP_pre_at_start),
      rexr_rel_hist_pre = first_non_missing(rexr_rel_hist_pre_at_start),
      EA_pre1992   = first_non_missing(EA_pre1992_at_start),
      EA_qual_9297 = first_non_missing(EA_qual_9297_at_start),
      EA_1998on    = first_non_missing(EA_1998on_at_start),
      d_IRS_episode_avg  = safe_mean(d_IRS),
      d_r_sh_episode_avg = safe_mean(d_r_sh),
      d_IRL_episode_avg  = safe_mean(d_IRL),
      d_r_lo_episode_avg = safe_mean(d_r_lo),
      d_taylor_episode_cum = safe_sum(as.numeric(d_real_short_taylorgap)),
      share_exp_episode   = first_non_missing(share_exp_cuts),
      share_capex_episode = first_non_missing(share_capex_cuts),
      .groups = "drop"
    )
}

row_labels_episode_start <- c(
  NEED_ADJ_pre         = "Need for adjustment (year before episode start)",
  GAP_pre              = "Output gap (year before episode start)",
  rexr_rel_hist_pre    = "Real exchange rate relative to historical average (year before episode start)",
  EA_pre1992           = "Euro-area countries, 1978--1991",
  EA_qual_9297         = "Euro-area countries, 1992--1997",
  EA_1998on            = "Euro-area countries, 1998 onward",
  d_IRS_episode_avg    = "Nominal short-term interest rate (average change during episode)",
  d_r_sh_episode_avg   = "Real short-term interest rate (average change during episode)",
  d_IRL_episode_avg    = "Nominal long-term interest rate (average change during episode)",
  d_r_lo_episode_avg   = "Real long-term interest rate (average change during episode)",
  d_taylor_episode_cum = "Short-term interest rate relative to Taylor rule (cumulated change during episode)",
  share_exp_episode    = "Share of expenditure cuts in total adjustment (cumulated over episode)",
  share_capex_episode  = "Share of capital-expenditure cuts in total adjustment (cumulated over episode)"
)

specs_episode <- list(
  "1"  = list(include_rer = TRUE,  money_var = NULL,                   comp_var = NULL),
  "2"  = list(include_rer = FALSE, money_var = "d_IRS_episode_avg",    comp_var = NULL),
  "3"  = list(include_rer = FALSE, money_var = "d_r_sh_episode_avg",   comp_var = NULL),
  "4"  = list(include_rer = FALSE, money_var = "d_IRL_episode_avg",    comp_var = NULL),
  "5"  = list(include_rer = FALSE, money_var = "d_r_lo_episode_avg",   comp_var = NULL),
  "6"  = list(include_rer = FALSE, money_var = "d_taylor_episode_cum", comp_var = NULL),
  "7"  = list(include_rer = FALSE, money_var = NULL,                   comp_var = "share_exp_episode"),
  "8"  = list(include_rer = FALSE, money_var = NULL,                   comp_var = "share_capex_episode"),
  "9"  = list(include_rer = FALSE, money_var = "d_IRS_episode_avg",    comp_var = "share_exp_episode"),
  "10" = list(include_rer = FALSE, money_var = "d_r_sh_episode_avg",   comp_var = "share_exp_episode"),
  "11" = list(include_rer = FALSE, money_var = "d_IRL_episode_avg",    comp_var = "share_exp_episode"),
  "12" = list(include_rer = FALSE, money_var = "d_r_lo_episode_avg",   comp_var = "share_exp_episode"),
  "13" = list(include_rer = FALSE, money_var = "d_taylor_episode_cum", comp_var = "share_exp_episode")
)

build_episode_table <- function(results, row_order, fmt_fun) {
  est_mat <- matrix("", nrow = length(row_order), ncol = length(results))
  rownames(est_mat) <- row_order
  colnames(est_mat) <- names(results)
  for (j in seq_along(results)) {
    coef_j <- results[[j]]$coef_tab
    for (v in row_order) {
      hit <- coef_j %>% filter(factor == v)
      if (nrow(hit) > 0) est_mat[v, j] <- fmt_fun(hit$estimate[1], hit$p_value[1])
    }
  }
  est_mat
}

run_cum_trunc_model <- function(data, row_order, include_rer = FALSE, money_var = NULL, comp_var = NULL) {
  vars_keep <- c("country_code", "cum_adjustment", "NEED_ADJ_pre", "GAP_pre", "EA_pre1992", "EA_qual_9297", "EA_1998on")
  if (include_rer) vars_keep <- c(vars_keep, "rexr_rel_hist_pre")
  if (!is.null(money_var)) vars_keep <- c(vars_keep, money_var)
  if (!is.null(comp_var)) vars_keep <- c(vars_keep, comp_var)
  reg_df <- data %>% select(all_of(vars_keep)) %>% filter(complete.cases(.))
  if (!is.null(money_var)) reg_df <- reg_df %>% rename(monetary_var = all_of(money_var))
  if (!is.null(comp_var)) reg_df <- reg_df %>% rename(composition_var = all_of(comp_var))
  rhs_vars <- c("NEED_ADJ_pre", "GAP_pre", "EA_pre1992", "EA_qual_9297", "EA_1998on")
  if (include_rer) rhs_vars <- c(rhs_vars, "rexr_rel_hist_pre")
  if (!is.null(money_var)) rhs_vars <- c(rhs_vars, "monetary_var")
  if (!is.null(comp_var)) rhs_vars <- c(rhs_vars, "composition_var")
  model <- truncreg(formula = as.formula(paste("cum_adjustment ~", paste(rhs_vars, collapse = " + "))), data = reg_df, point = 1, direction = "left")
  vcov_use <- tryCatch(vcovCL(model, cluster = reg_df$country_code, type = "HC1"), error = function(e) vcov(model))
  ct <- coeftest(model, vcov. = vcov_use)
  coef_tab <- tibble(factor = rownames(ct), estimate = unname(ct[, 1]), p_value = unname(ct[, 4]))
  if (!is.null(money_var)) coef_tab <- coef_tab %>% mutate(factor = ifelse(factor == "monetary_var", money_var, factor))
  if (!is.null(comp_var)) coef_tab <- coef_tab %>% mutate(factor = ifelse(factor == "composition_var", comp_var, factor))
  coef_tab <- coef_tab %>% filter(factor %in% row_order)
  list(model = model, coef_tab = coef_tab, nobs = nrow(reg_df))
}

### REGRESSION 5 ###

run_reg5 <- function(input_file, output_file, identification_label) {
  episode_df <- make_episode_start_df(input_file) %>% filter(!is.na(cum_adjustment), cum_adjustment >= 1)
  row_labels <- row_labels_episode_start
  row_order <- names(row_labels)
  results <- imap(specs_episode, ~ run_cum_trunc_model(data = episode_df, row_order = row_order, include_rer = .x$include_rer, money_var = .x$money_var, comp_var = .x$comp_var))
  est_mat <- build_episode_table(results, row_order, fmt_est_plain)
  latex_lines <- c(
    "\\begin{table}[!htbp]", "\\centering",
    paste0("\\caption{Size of cumulated adjustment during episode (", identification_label, ")}"),
    "\\label{tab:reg5_cumadjust}", "\\begin{threeparttable}", "\\scriptsize",
    "\\setlength{\\tabcolsep}{2.2pt}", "\\renewcommand{\\arraystretch}{1.10}",
    "\\begin{tabular}{@{}>{\\raggedright\\arraybackslash}p{0.18\\linewidth}*{13}{>{\\centering\\arraybackslash}p{0.055\\linewidth}}@{}}",
    "\\toprule", " & \\multicolumn{13}{c}{Regression results} \\\\", "\\cmidrule(lr){2-14}",
    "Type of regression & \\multicolumn{13}{l}{Truncated regressions; pooled sample - one observation per adjustment episode} \\\\",
    "Left-hand-side variable & \\multicolumn{13}{l}{Cumulated change in the primary cyclically adjusted fiscal balance over the whole adjustment episode} \\\\",
    "\\midrule", "Explanatory variables & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 & 11 & 12 & 13 \\\\", "\\midrule"
  )
  for (i in seq_along(row_order)) latex_lines <- c(latex_lines, paste0(row_labels[row_order[i]], " & ", paste(est_mat[row_order[i], ], collapse = " & "), " \\\\"))
  obs_line <- paste("Observations", paste(map_chr(results, ~ as.character(.x$nobs)), collapse = " & "), sep = " & ")
  latex_lines <- c(latex_lines, "(Constant not reported) &  &  &  &  &  &  &  &  &  &  &  &  &  \\\\", "\\midrule", paste0(obs_line, " \\\\"), "\\bottomrule", "\\end{tabular}", "\\begin{tablenotes}[flushleft]", "\\footnotesize", "\\item Reported coefficients are truncated-regression coefficients.", "\\item $^{*}$ significant at 10\\%; $^{**}$ significant at 5\\%; $^{***}$ significant at 1\\%.", "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  write_table(latex_lines, output_file)
}

run_reg5("Data/Clean Data/full_gen.csv", "Output/exo_reg5.tex", "Narrative-Based Episode Identification")
run_reg5("Data/Clean Data/full_OECD_dataset.csv", "Output/OECD_reg5.tex", "CAPB-Based Episode Identification")

### REGRESSION 6 ###

run_debt_ols_model <- function(data, row_order, include_rer = FALSE, money_var = NULL, comp_var = NULL) {
  vars_keep <- c("country_code", "decel_debt_ep", "NEED_ADJ_pre", "GAP_pre", "EA_pre1992", "EA_qual_9297", "EA_1998on")
  if (include_rer) vars_keep <- c(vars_keep, "rexr_rel_hist_pre")
  if (!is.null(money_var)) vars_keep <- c(vars_keep, money_var)
  if (!is.null(comp_var)) vars_keep <- c(vars_keep, comp_var)
  reg_df <- data %>% select(all_of(vars_keep)) %>% filter(complete.cases(.))
  if (!is.null(money_var)) reg_df <- reg_df %>% rename(monetary_var = all_of(money_var))
  if (!is.null(comp_var)) reg_df <- reg_df %>% rename(composition_var = all_of(comp_var))
  rhs_vars <- c("NEED_ADJ_pre", "GAP_pre", "EA_pre1992", "EA_qual_9297", "EA_1998on")
  if (include_rer) rhs_vars <- c(rhs_vars, "rexr_rel_hist_pre")
  if (!is.null(money_var)) rhs_vars <- c(rhs_vars, "monetary_var")
  if (!is.null(comp_var)) rhs_vars <- c(rhs_vars, "composition_var")
  model <- lm(as.formula(paste("decel_debt_ep ~", paste(rhs_vars, collapse = " + "))), data = reg_df)
  vcov_use <- tryCatch(vcovCL(model, cluster = reg_df$country_code, type = "HC1"), error = function(e) vcov(model))
  ct <- coeftest(model, vcov. = vcov_use)
  coef_tab <- tibble(factor = rownames(ct), estimate = unname(ct[, 1]), p_value = unname(ct[, 4]))
  if (!is.null(money_var)) coef_tab <- coef_tab %>% mutate(factor = ifelse(factor == "monetary_var", money_var, factor))
  if (!is.null(comp_var)) coef_tab <- coef_tab %>% mutate(factor = ifelse(factor == "composition_var", comp_var, factor))
  coef_tab <- coef_tab %>% filter(factor %in% row_order)
  list(model = model, coef_tab = coef_tab, nobs = nrow(reg_df), r2 = summary(model)$r.squared)
}

run_reg6 <- function(input_file, output_file, identification_label) {
  episode_df <- make_episode_start_df(input_file) %>% filter(!is.na(decel_debt_ep))
  row_labels <- row_labels_episode_start
  row_order <- names(row_labels)
  results <- imap(specs_episode, ~ run_debt_ols_model(data = episode_df, row_order = row_order, include_rer = .x$include_rer, money_var = .x$money_var, comp_var = .x$comp_var))
  est_mat <- build_episode_table(results, row_order, fmt_est_math)
  latex_lines <- c(
    "\\begin{table}[!htbp]", "\\centering",
    paste0("\\caption{Deceleration in the pace of debt accumulation (", identification_label, ")}"),
    "\\label{tab:reg6_deceldebt}", "\\begin{threeparttable}", "\\scriptsize",
    "\\setlength{\\tabcolsep}{2.2pt}", "\\renewcommand{\\arraystretch}{1.10}",
    "\\begin{tabular}{@{}>{\\raggedright\\arraybackslash}p{0.18\\linewidth}*{13}{>{\\centering\\arraybackslash}p{0.055\\linewidth}}@{}}",
    "\\toprule", " & \\multicolumn{13}{c}{Regression results} \\\\", "\\cmidrule(lr){2-14}",
    "Type of regression & \\multicolumn{13}{l}{Robust OLS regressions; pooled sample - one observation per adjustment episode} \\\\",
    "Left-hand-side variable & \\multicolumn{13}{l}{Difference in debt accumulation before and after the episode} \\\\",
    "\\midrule", "Explanatory variables & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 & 11 & 12 & 13 \\\\", "\\midrule"
  )
  for (i in seq_along(row_order)) latex_lines <- c(latex_lines, paste0(row_labels[row_order[i]], " & ", paste(est_mat[row_order[i], ], collapse = " & "), " \\\\"))
  obs_line <- paste("Observations", paste(map_chr(results, ~ as.character(.x$nobs)), collapse = " & "), sep = " & ")
  r2_line <- paste("R-squared", paste(map_chr(results, ~ fmt_num(.x$r2, 2)), collapse = " & "), sep = " & ")
  latex_lines <- c(latex_lines, "(Constant not reported) &  &  &  &  &  &  &  &  &  &  &  &  &  \\\\", "\\midrule", paste0(obs_line, " \\\\"), paste0(r2_line, " \\\\"), "\\bottomrule", "\\end{tabular}", "\\begin{tablenotes}[flushleft]", "\\footnotesize", "\\item Reported coefficients are robust OLS coefficients.", "\\item $^{*}$ significant at 10\\%; $^{**}$ significant at 5\\%; $^{***}$ significant at 1\\%.", "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  write_table(latex_lines, output_file)
}

run_reg6("Data/Clean Data/full_gen.csv", "Output/exo_reg6.tex", "Narrative-Based Episode Identification")
run_reg6("Data/Clean Data/full_OECD_dataset.csv", "Output/OECD_reg6.tex", "CAPB-Based Episode Identification")

### REGRESSION 7 ###

specs_length <- list(
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

run_length_trunc_model <- function(data, row_order, include_rer = FALSE, money_var = NULL, comp_var = NULL) {
  vars_keep <- c("country_code", "episode_length_ep", "NEED_ADJ_pre", "GAP_pre", "EA_pre1992", "EA_qual_9297", "EA_1998on")
  if (include_rer) vars_keep <- c(vars_keep, "rexr_rel_hist_pre")
  if (!is.null(money_var)) vars_keep <- c(vars_keep, money_var)
  if (!is.null(comp_var)) vars_keep <- c(vars_keep, comp_var)
  reg_df <- data %>% select(all_of(vars_keep)) %>% filter(complete.cases(.))
  if (!is.null(money_var)) reg_df <- reg_df %>% rename(monetary_var = all_of(money_var))
  if (!is.null(comp_var)) reg_df <- reg_df %>% rename(composition_var = all_of(comp_var))
  rhs_vars <- c("NEED_ADJ_pre", "GAP_pre", "EA_pre1992", "EA_qual_9297", "EA_1998on")
  if (include_rer) rhs_vars <- c(rhs_vars, "rexr_rel_hist_pre")
  if (!is.null(money_var)) rhs_vars <- c(rhs_vars, "monetary_var")
  if (!is.null(comp_var)) rhs_vars <- c(rhs_vars, "composition_var")
  model <- truncreg(formula = as.formula(paste("episode_length_ep ~", paste(rhs_vars, collapse = " + "))), data = reg_df, point = 1, direction = "left")
  vcov_use <- tryCatch(vcovCL(model, cluster = reg_df$country_code, type = "HC1"), error = function(e) vcov(model))
  ct <- coeftest(model, vcov. = vcov_use)
  coef_tab <- tibble(factor = rownames(ct), estimate = unname(ct[, 1]), p_value = unname(ct[, 4]))
  if (!is.null(money_var)) coef_tab <- coef_tab %>% mutate(factor = ifelse(factor == "monetary_var", money_var, factor))
  if (!is.null(comp_var)) coef_tab <- coef_tab %>% mutate(factor = ifelse(factor == "composition_var", comp_var, factor))
  coef_tab <- coef_tab %>% filter(factor %in% row_order)
  list(model = model, coef_tab = coef_tab, nobs = nrow(reg_df))
}

run_reg7 <- function(input_file, output_file, identification_label) {
  episode_df <- make_episode_start_df(input_file) %>% filter(!is.na(episode_length_ep), episode_length_ep >= 1)
  row_labels <- row_labels_episode_start
  row_order <- names(row_labels)
  results <- imap(specs_length, ~ run_length_trunc_model(data = episode_df, row_order = row_order, include_rer = .x$include_rer, money_var = .x$money_var, comp_var = .x$comp_var))
  est_mat <- build_episode_table(results, row_order, fmt_est_plain)
  latex_lines <- c(
    "\\begin{table}[!htbp]", "\\centering",
    paste0("\\caption{Length of adjustment episode (", identification_label, ")}"),
    "\\label{tab:reg7_length}", "\\begin{threeparttable}", "\\tiny",
    "\\setlength{\\tabcolsep}{1.5pt}", "\\renewcommand{\\arraystretch}{1.10}",
    "\\begin{tabular}{@{}>{\\raggedright\\arraybackslash}p{0.18\\linewidth}*{13}{>{\\centering\\arraybackslash}p{0.055\\linewidth}}@{}}",
    "\\toprule", " & \\multicolumn{13}{c}{Regression results} \\\\", "\\cmidrule(lr){2-14}",
    "Type of regression & \\multicolumn{13}{l}{Truncated regressions; pooled sample - one observation per adjustment episode} \\\\",
    "Left-hand-side variable & \\multicolumn{13}{l}{Length of adjustment episode (in years)} \\\\",
    "\\midrule", "Explanatory variables & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 & 11 & 12 & 13 \\\\", "\\midrule"
  )
  for (i in seq_along(row_order)) latex_lines <- c(latex_lines, paste0(row_labels[row_order[i]], " & ", paste(est_mat[row_order[i], ], collapse = " & "), " \\\\"))
  obs_line <- paste("Observations", paste(map_chr(results, ~ as.character(.x$nobs)), collapse = " & "), sep = " & ")
  latex_lines <- c(latex_lines, "(Constant not reported) &  &  &  &  &  &  &  &  &  &  &  &  &  \\\\", "\\midrule", paste0(obs_line, " \\\\"), "\\bottomrule", "\\end{tabular}", "\\begin{tablenotes}[flushleft]", "\\footnotesize", "\\item Reported coefficients are truncated-regression coefficients.", "\\item $^{*}$ significant at 10\\%; $^{**}$ significant at 5\\%; $^{***}$ significant at 1\\%.", "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  write_table(latex_lines, output_file)
}

run_reg7("Data/Clean Data/full_gen.csv", "Output/exo_reg7.tex", "Narrative-Based Episode Identification")
run_reg7("Data/Clean Data/full_OECD_dataset.csv", "Output/OECD_reg7.tex", "CAPB-Based Episode Identification")

### REGRESSION 8 ###

run_reg8 <- function(input_file, output_file, identification_label) {
  df <- read_csv(input_file, show_col_types = FALSE) %>%
    arrange(country_code, year) %>%
    group_by(country_code) %>%
    mutate(
      EA_pre1992   = as.numeric(EA_pre1992),
      EA_qual_9297 = as.numeric(EA_qual_9297),
      EA_1998on    = as.numeric(EA_1998on),
      d_IRS  = as.numeric(IRS)  - lag(as.numeric(IRS)),
      d_r_sh = as.numeric(r_sh) - lag(as.numeric(r_sh)),
      d_IRL  = as.numeric(IRL)  - lag(as.numeric(IRL)),
      d_r_lo = as.numeric(r_lo) - lag(as.numeric(r_lo)),
      EA_pre1992_at_end   = if_else(END == 1, EA_pre1992,   NA_real_),
      EA_qual_9297_at_end = if_else(END == 1, EA_qual_9297, NA_real_),
      EA_1998on_at_end    = if_else(END == 1, EA_1998on,    NA_real_)
    ) %>%
    ungroup() %>%
    filter(year >= 1978, year <= 2014)
  
  episode_df <- df %>%
    filter(!is.na(episode_id)) %>%
    group_by(country_code, episode_id) %>%
    summarise(
      slippage_ep = first_non_missing(-post2_cum_dCAPB),
      NEED_ADJ_last = first_non_missing(if_else(END == 1, as.numeric(NEED_ADJ), NA_real_)),
      GAP_last      = first_non_missing(if_else(END == 1, as.numeric(GAP),      NA_real_)),
      EA_pre1992   = first_non_missing(EA_pre1992_at_end),
      EA_qual_9297 = first_non_missing(EA_qual_9297_at_end),
      EA_1998on    = first_non_missing(EA_1998on_at_end),
      cum_adjustment = first_non_missing(cum_dCAPB_episode),
      d_IRS_episode_cum    = safe_sum(d_IRS),
      d_r_sh_episode_cum   = safe_sum(d_r_sh),
      d_IRL_episode_cum    = safe_sum(d_IRL),
      d_r_lo_episode_cum   = safe_sum(d_r_lo),
      d_taylor_episode_cum = safe_sum(as.numeric(d_real_short_taylorgap)),
      share_exp_episode   = first_non_missing(share_exp_cuts),
      share_capex_episode = first_non_missing(share_capex_cuts),
      .groups = "drop"
    ) %>%
    filter(!is.na(slippage_ep))
  
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
    EA_pre1992           = "Euro-area countries, 1978--1991",
    EA_qual_9297         = "Euro-area countries, 1992--1997",
    EA_1998on            = "Euro-area countries, 1998 onward",
    cum_adjustment       = "Cumulated change in primary cyclically adjusted fiscal balance (over episode)",
    share_exp_episode    = "Share of expenditure cuts in total adjustment (cumulated over episode)",
    share_capex_episode  = "Share of capital-expenditure cuts in total adjustment (cumulated over episode)",
    d_IRS_episode_cum    = "Nominal short-term interest rate (cumulated change during episode)",
    d_r_sh_episode_cum   = "Real short-term interest rate (cumulated change during episode)",
    d_IRL_episode_cum    = "Nominal long-term interest rate (cumulated change during episode)",
    d_r_lo_episode_cum   = "Real long-term interest rate (cumulated change during episode)",
    d_taylor_episode_cum = "Short-term interest rate relative to Taylor rule (cumulated change during episode)"
  )
  row_order <- names(row_labels)
  
  run_slippage_ols <- function(data, add_vars = character(0)) {
    vars_keep <- c("country_code", "slippage_ep", "NEED_ADJ_last", "GAP_last", "EA_pre1992", "EA_qual_9297", "EA_1998on", add_vars)
    reg_df <- data %>% select(all_of(vars_keep)) %>% filter(complete.cases(.))
    rhs_vars <- c("NEED_ADJ_last", "GAP_last", "EA_pre1992", "EA_qual_9297", "EA_1998on", add_vars)
    model <- lm(as.formula(paste("slippage_ep ~", paste(rhs_vars, collapse = " + "))), data = reg_df)
    vcov_use <- vcovCL(model, cluster = reg_df$country_code, type = "HC1")
    ct <- coeftest(model, vcov. = vcov_use)
    coef_tab <- tibble(factor = rownames(ct), estimate = unname(ct[, 1]), p_value = unname(ct[, 4])) %>% filter(factor %in% row_order)
    list(model = model, coef_tab = coef_tab, nobs = nrow(reg_df), r2 = summary(model)$r.squared)
  }
  
  results <- imap(specs, ~ run_slippage_ols(data = episode_df, add_vars = .x$add_vars))
  est_mat <- build_episode_table(results, row_order, fmt_est_plain)
  latex_lines <- c(
    "\\begin{table}[!htbp]", "\\centering",
    paste0("\\caption{Slippage after episode (", identification_label, ")}"),
    "\\label{tab:reg8_slippage}", "\\begin{threeparttable}", "\\scriptsize",
    "\\setlength{\\tabcolsep}{1.5pt}", "\\renewcommand{\\arraystretch}{1.15}",
    "\\begin{tabular}{@{}>{\\raggedright\\arraybackslash}p{0.28\\linewidth}*{9}{>{\\centering\\arraybackslash}p{0.072\\linewidth}}@{}}",
    "\\toprule", " & \\multicolumn{9}{c}{Regression results} \\\\", "\\cmidrule(lr){2-10}",
    paste0("Dataset and type of regression & \\multicolumn{9}{l}{", "\\parbox[t]{0.52\\linewidth}{Robust OLS regressions; pooled sample - one observation per adjustment episode}", "} \\\\[0.4ex]"),
    paste0("Left-hand-side variable & \\multicolumn{9}{l}{", "\\parbox[t]{0.52\\linewidth}{Slippage, i.e. deterioration in the primary cyclically adjusted fiscal balance in the two years after the end of the adjustment episode}", "} \\\\[0.6ex]"),
    "\\midrule", "Explanatory variable & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 \\\\", "\\midrule"
  )
  for (i in seq_along(row_order)) latex_lines <- c(latex_lines, paste0(row_labels[row_order[i]], " & ", paste(est_mat[row_order[i], ], collapse = " & "), " \\\\"))
  obs_line <- paste("Observations", paste(map_chr(results, ~ as.character(.x$nobs)), collapse = " & "), sep = " & ")
  r2_line <- paste("R-squared", paste(map_chr(results, ~ fmt_num(.x$r2, 2)), collapse = " & "), sep = " & ")
  latex_lines <- c(latex_lines, "(Constant not reported) &  &  &  &  &  &  &  &  &  \\\\", "\\midrule", paste0(obs_line, " \\\\"), paste0(r2_line, " \\\\"), "\\bottomrule", "\\end{tabular}", "\\begin{tablenotes}[flushleft]", "\\footnotesize", "\\item Reported coefficients are robust OLS coefficients.", "\\item $^{*}$ significant at 10\\%; $^{**}$ significant at 5\\%; $^{***}$ significant at 1\\%.", "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  write_table(latex_lines, output_file)
}

run_reg8("Data/Clean Data/full_gen.csv", "Output/exo_reg8.tex", "Narrative-Based Episode Identification")
run_reg8("Data/Clean Data/full_OECD_dataset.csv", "Output/OECD_reg8.tex", "CAPB-Based Episode Identification")

#==============================================================================#
