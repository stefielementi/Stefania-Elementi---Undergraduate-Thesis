# Script: episode_figures.R
# Purpose: recreate Figure 2-style country subfigures (1978-2014)
# Input: raw_full_MOD already loaded in memory
# Output: Figures/(1)_countryname.png
#         Figures/episode_figures.pdf

# 1 - Load Libraries
library(tidyverse)
library(readr)
library(gridExtra)
library(grid)

# 2 - Helper to convert 0/1, TRUE/FALSE, yes/no into a clean logical
to_flag <- function(x) {
  if (is.logical(x)) return(replace_na(x, FALSE))
  if (is.numeric(x) || is.integer(x)) return(!is.na(x) & x != 0)
  
  x_chr <- trimws(tolower(as.character(x)))
  x_chr %in% c("1", "true", "t", "yes", "y")
}

# 3 - Load and clean data
df <- raw_full_MOD %>%
  mutate(
    year = as.integer(year),
    NLGXQA = as.numeric(NLGXQA),
    GAP = as.numeric(GAP),
    r_sh = as.numeric(r_sh),
    real_short_taylorgap = as.numeric(real_short_taylorgap),
    start = to_flag(start),
    continue = to_flag(continue),
    serious = to_flag(serious),
    episode = start | continue | serious
  ) %>%
  filter(year >= 1978, year <= 2014) %>%
  arrange(country_code, year)

# 4 - Country labels
country_names <- c(
  AUT = "Austria",
  AUS = "Australia",
  BEL = "Belgium",
  FIN = "Finland",
  CAN = "Canada",
  FRA = "France",
  DEU = "Germany",
  DNK = "Denmark",
  GBR = "Great Britain",
  GRC = "Greece",
  IRL = "Ireland",
  ITA = "Italy",
  JPN = "Japan",
  LUX = "Luxembourg",
  NLD = "Netherlands",
  PRT = "Portugal",
  ESP = "Spain",
  SWE = "Sweeden"
  
)

# 5 - Helper: build shaded rectangles for episode periods
episode_rects <- function(d) {
  d <- d %>% arrange(year)
  
  r <- rle(d$episode %in% TRUE)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  idx <- which(r$values)
  
  if (length(idx) == 0) {
    return(tibble(
      xmin = numeric(),
      xmax = numeric(),
      ymin = numeric(),
      ymax = numeric()
    ))
  }
  
  tibble(
    xmin = d$year[starts[idx]] - 0.5,
    xmax = d$year[ends[idx]] + 0.5,
    ymin = -Inf,
    ymax = Inf
  )
}

# 6 - Plot function for one country
plot_fig2_country <- function(country_code_i, show_legend = TRUE) {
  
  d <- df %>%
    filter(country_code == country_code_i) %>%
    arrange(year)
  
  # Rebase Taylor-gap indicator so its average equals average real short rate
  k <- mean(d$r_sh, na.rm = TRUE) - mean(d$real_short_taylorgap, na.rm = TRUE)
  
  d <- d %>%
    mutate(
      taylor_indicator_rebased = real_short_taylorgap + k
    )
  
  rects <- episode_rects(d)
  
  # Compute dynamic y-axis so all series fit
  y_all <- c(d$NLGXQA, d$GAP, d$r_sh, d$taylor_indicator_rebased)
  y_all <- y_all[is.finite(y_all)]
  
  if (length(y_all) == 0) {
    y_min <- -5
    y_max <- 5
  } else {
    y_range <- range(y_all, na.rm = TRUE)
    pad <- max(0.75, 0.08 * diff(y_range))
    y_min <- floor((y_range[1] - pad) / 2) * 2
    y_max <- ceiling((y_range[2] + pad) / 2) * 2
  }
  
  # Bars: CAPB (% of potential GDP) and output gap
  bars <- d %>%
    select(year, NLGXQA, GAP) %>%
    pivot_longer(
      cols = c(NLGXQA, GAP),
      names_to = "series",
      values_to = "value"
    ) %>%
    mutate(
      series = factor(
        series,
        levels = c("NLGXQA", "GAP"),
        labels = c(
          "CAPB (% of potential GDP)",
          "Output gap"
        )
      )
    )
  
  # Lines: real short rate and rebased Taylor-gap indicator
  lines <- d %>%
    transmute(
      year,
      `Real short-term interest rate` = r_sh,
      `Monetary stance indicator (rebased Taylor gap)` = taylor_indicator_rebased
    ) %>%
    pivot_longer(
      cols = -year,
      names_to = "series",
      values_to = "value"
    )
  
  major_breaks <- seq(1980, 2015, by = 5)
  minor_breaks <- seq(1978, 2014, by = 1)
  
  p <- ggplot() +
    geom_rect(
      data = rects,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "gold",
      alpha = 0.35
    ) +
    geom_col(
      data = bars,
      aes(x = year, y = value, fill = series),
      position = "identity",
      width = 0.75,
      alpha = 0.95
    ) +
    geom_line(
      data = lines,
      aes(x = year, y = value, linetype = series),
      linewidth = 0.7,
      color = "black"
    ) +
    scale_fill_manual(values = c(
      "CAPB (% of potential GDP)" = "blue3",
      "Output gap" = "chartreuse3"
    )) +
    scale_linetype_manual(values = c(
      "Real short-term interest rate" = "solid",
      "Monetary stance indicator (rebased Taylor gap)" = "dotted"
    )) +
    scale_x_continuous(
      limits = c(1978, 2014),
      breaks = major_breaks,
      minor_breaks = minor_breaks,
      expand = c(0, 0.5)
    ) +
    scale_y_continuous(
      limits = c(y_min, y_max),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    labs(
      title = ifelse(country_code_i %in% names(country_names),
                     country_names[[country_code_i]],
                     country_code_i),
      x = NULL,
      y = "Per cent"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.minor.x = element_line(linewidth = 0.3),
      panel.grid.major.x = element_line(linewidth = 0.5),
      plot.margin = margin(8, 8, 8, 8)
    )
  
  p
}

# 7 - Save individual PNGs by country
dir.create("Figures", showWarnings = FALSE)

countries <- sort(unique(df$country_code))

for (cc in countries) {
  country_file <- tolower(ifelse(cc %in% names(country_names),
                                 country_names[[cc]],
                                 cc))
  country_file <- gsub(" ", "_", country_file)
  
  ggsave(
    filename = file.path("Figures", paste0("(1)_", country_file, ".png")),
    plot = plot_fig2_country(cc, show_legend = TRUE),
    width = 10,
    height = 6,
    dpi = 300
  )
}

# 8 - Create multi-page PDF with 2 columns x 2 rows (4 figures per page)
plot_list_pdf <- lapply(countries, function(cc) {
  ggplotGrob(plot_fig2_country(cc, show_legend = TRUE))
})

pdf_pages <- marrangeGrob(
  grobs = plot_list_pdf,
  nrow = 2,
  ncol = 2,
  top = NULL,
  padding = unit(0.9, "cm")
)

ggsave(
  filename = "Figures/episode_figures.pdf",
  plot = pdf_pages,
  width = 16,
  height = 12,
  units = "in"
)

cat(
  "Wrote individual PNGs to Figures/\n",
  "Wrote multi-page PDF to Figures/episode_figures.pdf\n",
  sep = ""
)