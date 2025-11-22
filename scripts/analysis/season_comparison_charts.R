#!/usr/bin/env Rscript
# Season Comparison Charts for Garmin Cycling Data
# Creates volume comparison, training consistency, and HR zone distribution charts

library(tidyverse)
library(scales)

# Load cleaned data
data_path <- file.path(dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path))),
                       "data/cleaned/garmin_cleaned.csv")
# Fallback for command line execution
if (!exists("data_path") || !file.exists(data_path)) {
  data_path <- "data/cleaned/garmin_cleaned.csv"
}

df <- read_csv(data_path, show_col_types = FALSE)

# Filter to only 2024 and 2025 seasons
seasons_df <- df %>%
  filter(season %in% c("2024 Season", "2025 Season")) %>%
  mutate(
    season = factor(season, levels = c("2024 Season", "2025 Season")),
    month_num = month,
    month_name = month.abb[month]
  )

# Define HR zones by season
get_hr_zone <- function(avg_hr, season) {
  if (is.na(avg_hr)) return(NA_character_)

  if (season == "2024 Season") {
    case_when(
      avg_hr <= 114 ~ "Zone 1",
      avg_hr <= 151 ~ "Zone 2",
      avg_hr <= 169 ~ "Zone 3",
      avg_hr <= 187 ~ "Zone 4",
      TRUE ~ "Zone 5"
    )
  } else {  # 2025 Season
    case_when(
      avg_hr <= 117 ~ "Zone 1",
      avg_hr <= 157 ~ "Zone 2",
      avg_hr <= 177 ~ "Zone 3",
      avg_hr <= 193 ~ "Zone 4",
      TRUE ~ "Zone 5"
    )
  }
}

# Add HR zones
seasons_df <- seasons_df %>%
  rowwise() %>%
  mutate(hr_zone = get_hr_zone(avg_heart_rate, as.character(season))) %>%
  ungroup() %>%
  mutate(hr_zone = factor(hr_zone, levels = c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5")))

# Output directory
output_dir <- "docs/visualizations"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Color palette for seasons
season_colors <- c("2024 Season" = "#2E86AB", "2025 Season" = "#E94F37")

# ==============================================================================
# Figure 1: Volume Comparison Bar Chart
# ==============================================================================

volume_summary <- seasons_df %>%
  group_by(season) %>%
  summarise(
    total_distance_miles = sum(distance_miles, na.rm = TRUE),
    total_hours = sum(moving_hours, na.rm = TRUE),
    num_rides = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(total_distance_miles, total_hours, num_rides),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric_label = case_when(
      metric == "total_distance_miles" ~ "Total Distance\n(miles)",
      metric == "total_hours" ~ "Total Time\n(hours)",
      metric == "num_rides" ~ "Number of\nRides"
    ),
    metric_label = factor(metric_label, levels = c(
      "Total Distance\n(miles)",
      "Total Time\n(hours)",
      "Number of\nRides"
    ))
  )

fig1 <- ggplot(volume_summary, aes(x = metric_label, y = value, fill = season)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = case_when(
      metric == "total_distance_miles" ~ comma(round(value)),
      metric == "total_hours" ~ comma(round(value)),
      metric == "num_rides" ~ as.character(round(value))
    )),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_fill_manual(values = season_colors) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    limits = c(0, NA),
    labels = comma
  ) +
  labs(
    title = "Figure 1: Season Volume Comparison",
    subtitle = "2024 Season (Mar-Oct) vs 2025 Season (Apr-Oct)",
    x = "Metric",
    y = "Value",
    fill = "Season"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 10)
  )

ggsave(file.path(output_dir, "fig1_volume_comparison.png"), fig1,
       width = 8, height = 6, dpi = 300, bg = "white")
cat("Saved: fig1_volume_comparison.png\n")

# ==============================================================================
# Figure 2: Training Consistency Line Chart
# ==============================================================================

# Define month ranges for each season
# 2024: Mar-Oct (3-10), 2025: Apr-Oct (4-10)
# For comparison, we'll align by "season month" (1 = first month of season)

consistency_df <- seasons_df %>%
  mutate(
    # Create a normalized month for comparison
    season_month = case_when(
      season == "2024 Season" ~ month_num - 2,  # Mar=1, Apr=2, ..., Oct=8
      season == "2025 Season" ~ month_num - 3   # Apr=1, May=2, ..., Oct=7
    )
  ) %>%
  group_by(season, month_num, month_name) %>%
  summarise(
    rides_count = n(),
    .groups = "drop"
  ) %>%
  # Add month labels

  mutate(
    month_label = factor(month_name, levels = month.abb)
  )

fig2 <- ggplot(consistency_df, aes(x = month_label, y = rides_count,
                                    color = season, group = season)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = rides_count), vjust = -1, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = season_colors) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    limits = c(0, NA),
    breaks = seq(0, 100, by = 10)
  ) +
  labs(
    title = "Figure 2: Training Consistency by Month",
    subtitle = "Number of rides per month during each season",
    x = "Month",
    y = "Number of Rides (count)",
    color = "Season"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(output_dir, "fig2_training_consistency.png"), fig2,
       width = 10, height = 6, dpi = 300, bg = "white")
cat("Saved: fig2_training_consistency.png\n")

# ==============================================================================
# Figure 3: Heart Rate Zone Distribution
# ==============================================================================

hr_zone_summary <- seasons_df %>%
  filter(!is.na(hr_zone)) %>%
  group_by(season, hr_zone) %>%
  summarise(
    count = n(),
    .groups = "drop"
  ) %>%
  group_by(season) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100
  ) %>%
  ungroup()

# Calculate standard deviation of percentages across zones for each season
hr_zone_sd <- hr_zone_summary %>%
  group_by(season) %>%
  summarise(
    sd_percentage = sd(percentage),
    .groups = "drop"
  )

# Add SD to the data for annotation
hr_zone_summary <- hr_zone_summary %>%
  left_join(hr_zone_sd, by = "season")

# Zone colors (physiological gradient)
zone_colors <- c(
  "Zone 1" = "#3498db",  # Recovery - blue
  "Zone 2" = "#2ecc71",  # Endurance - green
  "Zone 3" = "#f1c40f",  # Tempo - yellow
  "Zone 4" = "#e67e22",  # Threshold - orange
  "Zone 5" = "#e74c3c"   # VO2max - red
)

fig3 <- ggplot(hr_zone_summary, aes(x = hr_zone, y = percentage, fill = hr_zone)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            vjust = -0.5, size = 3.5) +
  # Add SD annotation
  geom_text(
    data = hr_zone_sd,
    aes(x = 4.5, y = Inf, label = sprintf("SD = %.1f%%", sd_percentage)),
    inherit.aes = FALSE,
    vjust = 1.5,
    hjust = 1,
    size = 3.5,
    fontface = "italic"
  ) +
  facet_wrap(~season, ncol = 2) +
  scale_fill_manual(values = zone_colors) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    limits = c(0, NA),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Figure 3: Heart Rate Zone Distribution by Season",
    subtitle = paste0(
      "2024 Zones: Z1 ≤114, Z2 115-151, Z3 152-169, Z4 170-187, Z5 ≥188 bpm\n",
      "2025 Zones: Z1 ≤117, Z2 118-157, Z3 158-177, Z4 178-193, Z5 ≥194 bpm"
    ),
    x = "Heart Rate Zone",
    y = "Percentage of Rides (%)",
    fill = "HR Zone"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 9),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave(file.path(output_dir, "fig3_hr_zone_distribution.png"), fig3,
       width = 10, height = 7, dpi = 300, bg = "white")
cat("Saved: fig3_hr_zone_distribution.png\n")

# ==============================================================================
# Print Summary Statistics
# ==============================================================================

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("Summary Statistics\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")

cat("\nVolume Comparison:\n")
volume_summary %>%
  pivot_wider(names_from = season, values_from = value) %>%
  print()

cat("\nRides per Month:\n")
consistency_df %>%
  select(season, month_name, rides_count) %>%
  pivot_wider(names_from = season, values_from = rides_count) %>%
  print()

cat("\nHR Zone Distribution:\n")
hr_zone_summary %>%
  select(season, hr_zone, percentage) %>%
  pivot_wider(names_from = season, values_from = percentage) %>%
  print()

cat("\nHR Zone Standard Deviation:\n")
print(hr_zone_sd)

cat("\nAll charts saved to:", output_dir, "\n")
