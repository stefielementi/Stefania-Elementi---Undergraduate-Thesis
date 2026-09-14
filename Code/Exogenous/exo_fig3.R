# Script: exo_fig3.R
# Purpose: recreate Figure 3-style scatter for consolidation episodes
# Input: Data/Clean Data/raw_full.csv
# Output: Figures/exo_.png

# 1 - Load Libraries
library(tidyverse)
library(readr)
# Register Verdana on Windows
if (.Platform$OS.type == "windows") {
  windowsFonts(Verdana = windowsFont("Verdana"))
}

# 2 - Load Data
infile <- "Data/Clean Data/full_gen.csv"

if (!file.exists(infile)) {
  stop("Input file not found: ", infile)
}

raw_full <- read_csv(infile, show_col_types = FALSE) %>%
  dplyr::select(-dplyr::any_of("...1"))

# 3 - Helper functions
first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[1]
}

to_flag <- function(x) {
  if (is.logical(x)) return(replace_na(x, FALSE))
  if (is.numeric(x) || is.integer(x)) return(!is.na(x) & x != 0)
  
  x_chr <- trimws(tolower(as.character(x)))
  x_chr %in% c("1", "true", "t", "yes", "y")
}

# 4 - Prepare episode-level dataset
df <- raw_full %>%
  mutate(
    year = as.integer(year),
    country_code = as.character(country_code),
    episode_id = as.integer(episode_id),
    cum_dCAPB_episode = as.numeric(cum_dCAPB_episode),
    share_exp_cuts = as.numeric(share_exp_cuts),
    EA_country = as.integer(EA_country),
    start = to_flag(start),
    continue = to_flag(continue),
    serious = to_flag(serious),
    in_episode = start | continue | serious
  ) %>%
  filter(year >= 1978, year <= 2014)

episodes <- df %>%
  filter(!is.na(episode_id), in_episode) %>%
  group_by(country_code, episode_id) %>%
  summarise(
    cum_adj = first_non_missing(cum_dCAPB_episode),
    share_current_exp_cuts = first_non_missing(share_exp_cuts),
    EA_country = first_non_missing(EA_country),
    start_year = min(year, na.rm = TRUE),
    end_year = max(year, na.rm = TRUE),
    episode_length = dplyr::n(),
    .groups = "drop"
  ) %>%
  filter(
    !is.na(cum_adj),
    !is.na(share_current_exp_cuts),
    !is.na(EA_country),
    is.finite(cum_adj),
    is.finite(share_current_exp_cuts)
  ) %>%
  mutate(
    group = if_else(EA_country == 1, "Euro area", "Non-euro area")
  )

# 5 - Create figure
dir.create("Figures", showWarnings = FALSE)

p <- ggplot(episodes, aes(x = share_current_exp_cuts, y = cum_adj)) +
  annotate(
    "rect",
    xmin = 0, xmax = 1,
    ymin = -Inf, ymax = Inf,
    fill = "yellow",
    alpha = 0.45
  ) +
  geom_point(
    aes(shape = group),
    color = "#19D3E0",
    size = 3.2,
    alpha = 0.98
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 0.6
  ) +
  scale_shape_manual(
    values = c("Euro area" = 16, "Non-euro area" = 17),
    breaks = c("Euro area", "Non-euro area"),
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(-2, 1.5),
    breaks = seq(-2, 1.5, by = 0.5),
    labels = function(x) sprintf("%.1f", x)
  ) +
  scale_y_continuous(
    limits = c(0, 14),
    breaks = seq(0, 14, by = 2),
    sec.axis = dup_axis(name = NULL, breaks = seq(0, 14, by = 2))
  ) +
  labs(
    title = "Narrative-Based Consolidation Episodes: Cumlative Fiscal Adjustment and Share of Current Expenditure Cuts",
    subtitle = "Cumulative Fiscal Adjustment\n(per cent of GDP)",
    x = "Ratio of Current Expenditure Cuts to Total Adjustment",
    y = NULL,
    caption = NULL
  ) +
  theme_minimal(base_size = 12, base_family = "Verdana") +
  theme(
    text = element_text(family = "Verdana", size = 12),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#00E5FF", linewidth = 0.6),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "#4F46E5", linewidth = 0.5),
    axis.line.y = element_line(color = "#00E5FF", linewidth = 0.5),
    axis.ticks.x = element_line(color = "#4F46E5", linewidth = 0.5),
    axis.ticks.y = element_line(color = "#00E5FF", linewidth = 0.5),
    axis.text = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 12)),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(b = 18)),
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0, lineheight = 1.5, margin = margin(b = 18)),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    plot.margin = margin(t = 20, r = 24, b = 20, l = 24)
  )

ggsave(
  filename = "Figures/exo_fig3.png",
  plot = p,
  width = 16,
  height = 10,
  dpi = 300
)

print(p)

cat("Wrote: Figures/exo_fig3.png")