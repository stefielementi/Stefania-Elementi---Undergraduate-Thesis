# Script: episode_comparison.R
# Purpose: plot fiscal adjustment episodes by country and identification method
# Input:
#   Data/Clean Data/full_gen.csv           -> narrative episodes
#   Data/Clean Data/full_OECD_dataset.csv  -> CAPB episodes
# Output:
#   Figures/episodes_timeline.pdf

library(readr)
library(dplyr)
library(ggplot2)
library(tibble)

if (.Platform$OS.type == "windows") {
  windowsFonts(Verdana = windowsFont("Verdana"))
}

# 1 - Load data
df_narrative <- read_csv("Data/Clean Data/full_gen.csv", show_col_types = FALSE)
df_capb      <- read_csv("Data/Clean Data/full_OECD_dataset.csv", show_col_types = FALSE)

# 2 - Country names
country_map <- c(
  AUS = "Australia",
  AUT = "Austria",
  BEL = "Belgium",
  CAN = "Canada",
  DNK = "Denmark",
  FIN = "Finland",
  FRA = "France",
  DEU = "Germany",
  IRL = "Ireland",
  ITA = "Italy",
  JPN = "Japan",
  PRT = "Portugal",
  ESP = "Spain",
  SWE = "Sweden",
  GBR = "United Kingdom",
  USA = "United States"
)

# 3 - Country order
country_df <- tibble(
  country_code = names(sort(country_map))
) %>%
  mutate(
    Country = recode(country_code, !!!country_map),
    country_y = rev(seq_along(country_code))
  )

# 4 - Combine datasets
df_all <- bind_rows(
  df_narrative %>% mutate(method = "Narrative"),
  df_capb %>% mutate(method = "CAPB")
) %>%
  mutate(
    Country = recode(country_code, !!!country_map, .default = country_code)
  ) %>%
  filter(year >= 1978, year <= 2014)

# 5 - Keep only episode years and collapse each episode into a start/end range
episode_ranges <- df_all %>%
  filter(!is.na(episode_id), episode_id != 0) %>%
  group_by(country_code, Country, method, episode_id) %>%
  summarise(
    start_year = min(year, na.rm = TRUE),
    end_year   = max(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(country_df %>% select(country_code, country_y), by = "country_code") %>%
  mutate(
    method = factor(method, levels = c("Narrative", "CAPB")),
    y = country_y + if_else(method == "Narrative", 0.18, -0.18)
  )

# 6 - Plot
p <- ggplot(episode_ranges) +
  geom_hline(
    yintercept = country_df$country_y - 0.5,
    color = "grey90",
    linewidth = 0.25
  ) +
  geom_rect(
    aes(
      xmin = start_year - 0.45,
      xmax = end_year + 0.45,
      ymin = y - 0.13,
      ymax = y + 0.13,
      fill = method
    )
  ) +
  scale_y_continuous(
    breaks = country_df$country_y,
    labels = country_df$Country,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_x_continuous(
    breaks = c(1978, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2014),
    limits = c(1977.5, 2014.5),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c(
      "Narrative" = "#D55E00",
      "CAPB" = "#0072B2"
    )
  ) +
  labs(
    title = "Fiscal Consolidation Episodes by Country and Identification Method",
    x = "Year",
    y = NULL,
    fill = "Identification Method"
  ) +
  theme_minimal(base_size = 12, base_family = "Verdana") +
  theme(
    text = element_text(family = "Verdana", size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.25),
    axis.text = element_text(family = "Verdana", size = 12),
    axis.title = element_text(family = "Verdana", size = 12),
    legend.title = element_text(family = "Verdana", size = 12),
    legend.text = element_text(family = "Verdana", size = 12),
    legend.position = "bottom",
    plot.title = element_text(family = "Verdana", size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(family = "Verdana", size = 12)
  )

# 7 - Save output

ggsave(
  filename = "Figures/episodes_timeline.png",
  plot = p,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300
)