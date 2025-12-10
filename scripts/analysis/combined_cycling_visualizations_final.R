#!/usr/bin/env Rscript
# Combined Cycling Visualizations - INFOST 582 Final Project
# Generates all season comparison visualizations for 2024 vs 2025
#
# This script combines functionality from:
#   - cycling_visualizations.R (7 plots)
#   - season_comparison_charts.R (3 plots)
#
# Dependencies: base R, tidyverse, ggplot2 only

library(tidyverse)
library(ggplot2)

# ==============================================================================
# 1. LOAD AND PREPARE DATA
# ==============================================================================

# Load cleaned data
data_path <- "data/cleaned/garmin_cleaned.csv"
df <- read_csv(data_path, show_col_types = FALSE)

# Filter to 2024 and 2025 seasons only
season_data <- df %>%
  filter(season %in% c("2024 Season", "2025 Season")) %>%
  mutate(
    # Create month_name column
    month_name = month.abb[month],
    # Create is_indoor column
    is_indoor = (ride_type == "Indoor"),
    # Simplify season labels for some visualizations
    season_short = case_when(
      season == "2024 Season" ~ "2024",
      season == "2025 Season" ~ "2025",
      TRUE ~ season
    ),
    # Calculate total hours if needed
    total_hours = moving_hours
  )

# ==============================================================================
# 2. DEFINE HEART RATE ZONE FUNCTIONS
# ==============================================================================

get_hr_zone_2024 <- function(hr) {
  case_when(
    is.na(hr) ~ NA_character_,
    hr <= 114 ~ "Zone 1",
    hr <= 151 ~ "Zone 2",
    hr <= 169 ~ "Zone 3",
    hr <= 187 ~ "Zone 4",
    TRUE ~ "Zone 5"
  )
}

get_hr_zone_2025 <- function(hr) {
  case_when(
    is.na(hr) ~ NA_character_,
    hr <= 117 ~ "Zone 1",
    hr <= 157 ~ "Zone 2",  # CORRECTED: 157 (not 155)
    hr <= 177 ~ "Zone 3",  # CORRECTED: 177 (not 174)
    hr <= 193 ~ "Zone 4",
    TRUE ~ "Zone 5"
  )
}

# ==============================================================================
# 3. DEFINE POWER ZONE FUNCTIONS
# ==============================================================================

get_power_zone_2024 <- function(power) {
  case_when(
    is.na(power) ~ NA_character_,
    power <= 169 ~ "Zone 1",
    power <= 231 ~ "Zone 2",
    power <= 277 ~ "Zone 3",
    power <= 323 ~ "Zone 4",
    power <= 370 ~ "Zone 5",
    power <= 462 ~ "Zone 6",
    TRUE ~ "Zone 7"
  )
}

get_power_zone_2025 <- function(power) {
  case_when(
    is.na(power) ~ NA_character_,
    power <= 179 ~ "Zone 1",
    power <= 257 ~ "Zone 2",
    power <= 293 ~ "Zone 3",
    power <= 341 ~ "Zone 4",
    power <= 390 ~ "Zone 5",
    power <= 488 ~ "Zone 6",
    TRUE ~ "Zone 7"
  )
}

# ==============================================================================
# 4. APPLY ZONE CLASSIFICATIONS
# ==============================================================================

season_data <- season_data %>%
  mutate(
    hr_zone = case_when(
      season == "2024 Season" ~ get_hr_zone_2024(avg_heart_rate),
      season == "2025 Season" ~ get_hr_zone_2025(avg_heart_rate),
      TRUE ~ NA_character_
    ),
    power_zone = case_when(
      season == "2024 Season" ~ get_power_zone_2024(avg_power),
      season == "2025 Season" ~ get_power_zone_2025(avg_power),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    hr_zone = factor(hr_zone, levels = c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5")),
    power_zone = factor(power_zone, levels = c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5", "Zone 6", "Zone 7"))
  )

# ==============================================================================
# 5. CREATE OUTPUT DIRECTORY
# ==============================================================================

output_dir <- "outputs/figures/final"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 6. COLOR SCHEMES
# ==============================================================================

# Season colors
season_colors <- c("2024 Season" = "#2E86AB", "2025 Season" = "#E94F37")
season_colors_short <- c("2024" = "#2E86AB", "2025" = "#E94F37")

# HR zone colors (physiological gradient)
hr_zone_colors <- c(
  "Zone 1" = "#3498db",  # Recovery - blue
  "Zone 2" = "#2ecc71",  # Endurance - green
  "Zone 3" = "#f1c40f",  # Tempo - yellow
  "Zone 4" = "#e67e22",  # Threshold - orange
  "Zone 5" = "#e74c3c"   # VO2max - red
)

# Power zone colors
power_zone_colors <- c(
  "Zone 1" = "#00CC00",
  "Zone 2" = "#66CC00",
  "Zone 3" = "#CCCC00",
  "Zone 4" = "#FFCC00",
  "Zone 5" = "#FF9900",
  "Zone 6" = "#FF6600",
  "Zone 7" = "#FF0000"
)

# ==============================================================================
# SECTION A: CYCLING_VISUALIZATIONS.R OUTPUTS (7 PLOTS)
# ==============================================================================

cat("Generating visualizations from cycling_visualizations.R...\n")

# ==============================================================================
# VIZ 1: Season Comparison Bar Plot (Faceted)
# ==============================================================================

# Calculate season totals
season_summary <- season_data %>%
  group_by(season_short) %>%
  summarise(
    total_distance_miles = sum(distance_miles, na.rm = TRUE),
    total_hours = sum(total_hours, na.rm = TRUE),
    total_rides = n(),
    .groups = "drop"
  )

# Reshape for grouped bar chart
season_long <- season_summary %>%
  pivot_longer(cols = c(total_distance_miles, total_hours, total_rides),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = factor(metric,
                         levels = c("total_distance_miles", "total_hours", "total_rides"),
                         labels = c("Total Distance (miles)", "Total Hours", "Number of Rides")))

# Create faceted bar chart
p1 <- ggplot(season_long, aes(x = season_short, y = value, fill = season_short)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", linewidth = 0.3) +
  geom_text(aes(label = round(value, 1)),
            vjust = -0.5, size = 4, fontface = "bold") +
  facet_wrap(~metric, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = season_colors_short) +
  labs(title = "Cycling Season Comparison: 2024 vs 2025",
       subtitle = "March through October of each year",
       x = "Season",
       y = "Value",
       fill = "Season") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.5, "lines")
  )

ggsave(file.path(output_dir, "season_comparison_barplot_final.png"), p1,
       width = 12, height = 6, dpi = 300, bg = "white")
cat("  ✓ season_comparison_barplot_final.png\n")

# ==============================================================================
# VIZ 2a & 2b: Monthly Ride Frequency (Separate Charts)
# ==============================================================================

# Prepare data for monthly rides with indoor/outdoor breakdown
monthly_rides <- season_data %>%
  mutate(month_num = month) %>%
  group_by(season_short, month_num, is_indoor) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(ride_type = ifelse(is_indoor, "Indoor", "Outdoor"))

# Create complete month grid to ensure all months are represented
expand_months <- function(df, season_name, month_range) {
  complete_grid <- expand.grid(
    season_short = season_name,
    month_num = month_range,
    ride_type = c("Indoor", "Outdoor"),
    stringsAsFactors = FALSE
  )
  result <- left_join(complete_grid, df, by = c("season_short", "month_num", "ride_type"))
  result$count[is.na(result$count)] <- 0
  return(result)
}

# 2024 Season: March-October (months 3-10)
monthly_2024 <- monthly_rides %>%
  filter(season_short == "2024") %>%
  expand_months("2024", 3:10)

# 2025 Season: March-October (months 3-10) - adjust if data starts in April
available_2025_months <- unique(monthly_rides$month_num[monthly_rides$season_short == "2025"])
monthly_2025 <- monthly_rides %>%
  filter(season_short == "2025") %>%
  expand_months("2025", available_2025_months)

# Calculate trend lines (linear regression)
get_trend_data <- function(df) {
  indoor_data <- df %>% filter(ride_type == "Indoor")
  outdoor_data <- df %>% filter(ride_type == "Outdoor")

  # Fit linear models
  indoor_lm <- lm(count ~ month_num, data = indoor_data)
  outdoor_lm <- lm(count ~ month_num, data = outdoor_data)

  # Calculate total rides per month
  totals <- df %>%
    group_by(month_num) %>%
    summarise(total = sum(count), .groups = "drop")
  total_lm <- lm(total ~ month_num, data = totals)

  # Create prediction data
  month_range <- sort(unique(df$month_num))
  trends <- data.frame(
    month_num = rep(month_range, 3),
    ride_type = rep(c("Indoor", "Outdoor", "Total"), each = length(month_range)),
    trend_value = c(
      predict(indoor_lm, newdata = data.frame(month_num = month_range)),
      predict(outdoor_lm, newdata = data.frame(month_num = month_range)),
      predict(total_lm, newdata = data.frame(month_num = month_range))
    )
  )

  return(list(data = df, totals = totals, trends = trends))
}

# Process 2024 data
data_2024 <- get_trend_data(monthly_2024)

# Process 2025 data
data_2025 <- get_trend_data(monthly_2025)

# Month labels
month_labels <- c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
month_mapping <- setNames(month_labels, 3:10)

# Create 2024 visualization
p2_2024 <- ggplot() +
  # Bars for indoor/outdoor rides
  geom_bar(data = data_2024$data,
           aes(x = factor(month_num, levels = 3:10), y = count, fill = ride_type),
           stat = "identity", position = "stack", width = 0.6) +
  # Trend lines
  geom_line(data = data_2024$trends %>% filter(ride_type == "Indoor"),
            aes(x = as.numeric(factor(month_num, levels = 3:10)), y = trend_value, linetype = "Indoor"),
            color = "#2E86AB", linewidth = 1, alpha = 0.8) +
  geom_line(data = data_2024$trends %>% filter(ride_type == "Outdoor"),
            aes(x = as.numeric(factor(month_num, levels = 3:10)), y = trend_value, linetype = "Outdoor"),
            color = "#E94F37", linewidth = 1, alpha = 0.8) +
  geom_line(data = data_2024$trends %>% filter(ride_type == "Total"),
            aes(x = as.numeric(factor(month_num, levels = 3:10)), y = trend_value, linetype = "Total"),
            color = "black", linewidth = 1.2, alpha = 0.7) +
  scale_fill_manual(values = c("Indoor" = "#6BAED6", "Outdoor" = "#74C476"),
                    name = "Ride Type") +
  scale_linetype_manual(values = c("Indoor" = "dashed", "Outdoor" = "dotted", "Total" = "solid"),
                        name = "Trend Lines") +
  scale_x_discrete(labels = month_mapping[as.character(3:10)]) +
  labs(title = "Monthly Ride Frequency: 2024 Season",
       subtitle = "Bars = Ride counts by type, Lines = Trends (Indoor, Outdoor, Total)",
       x = "Month",
       y = "Number of Rides (count)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  guides(fill = guide_legend(order = 1, nrow = 1),
         linetype = guide_legend(order = 2, nrow = 1))

ggsave(file.path(output_dir, "monthly_rides_2024_final.png"), p2_2024,
       width = 10, height = 7, dpi = 300, bg = "white")
cat("  ✓ monthly_rides_2024_final.png\n")

# Create 2025 visualization
month_labels_2025 <- month_mapping[as.character(available_2025_months)]

p2_2025 <- ggplot() +
  # Bars for indoor/outdoor rides
  geom_bar(data = data_2025$data,
           aes(x = factor(month_num), y = count, fill = ride_type),
           stat = "identity", position = "stack", width = 0.6) +
  # Trend lines
  geom_line(data = data_2025$trends %>% filter(ride_type == "Indoor"),
            aes(x = as.numeric(factor(month_num, levels = available_2025_months)),
                y = trend_value, linetype = "Indoor"),
            color = "#2E86AB", linewidth = 1, alpha = 0.8) +
  geom_line(data = data_2025$trends %>% filter(ride_type == "Outdoor"),
            aes(x = as.numeric(factor(month_num, levels = available_2025_months)),
                y = trend_value, linetype = "Outdoor"),
            color = "#E94F37", linewidth = 1, alpha = 0.8) +
  geom_line(data = data_2025$trends %>% filter(ride_type == "Total"),
            aes(x = as.numeric(factor(month_num, levels = available_2025_months)),
                y = trend_value, linetype = "Total"),
            color = "black", linewidth = 1.2, alpha = 0.7) +
  scale_fill_manual(values = c("Indoor" = "#6BAED6", "Outdoor" = "#74C476"),
                    name = "Ride Type") +
  scale_linetype_manual(values = c("Indoor" = "dashed", "Outdoor" = "dotted", "Total" = "solid"),
                        name = "Trend Lines") +
  scale_x_discrete(labels = month_labels_2025) +
  labs(title = "Monthly Ride Frequency: 2025 Season",
       subtitle = "Bars = Ride counts by type, Lines = Trends (Indoor, Outdoor, Total)",
       x = "Month",
       y = "Number of Rides (count)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  guides(fill = guide_legend(order = 1, nrow = 1),
         linetype = guide_legend(order = 2, nrow = 1))

ggsave(file.path(output_dir, "monthly_rides_2025_final.png"), p2_2025,
       width = 10, height = 7, dpi = 300, bg = "white")
cat("  ✓ monthly_rides_2025_final.png\n")

# ==============================================================================
# VIZ 3a & 3b: Heart Rate Box Plots and Scatter Plots
# ==============================================================================

# Filter for activities with valid HR data
hr_data <- season_data %>%
  filter(!is.na(avg_heart_rate)) %>%
  select(season, season_short, avg_heart_rate, hr_zone)

# Calculate statistics for annotation
hr_stats <- hr_data %>%
  group_by(season_short) %>%
  summarise(
    mean_hr = mean(avg_heart_rate, na.rm = TRUE),
    sd_hr = sd(avg_heart_rate, na.rm = TRUE),
    median_hr = median(avg_heart_rate, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Create Heart Rate Box Plot
p3a_boxplot <- ggplot(hr_data, aes(x = season_short, y = avg_heart_rate, fill = season_short)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 2.5,
               outlier.fill = "gray50", outlier.color = "black",
               width = 0.5) +
  scale_fill_manual(values = season_colors_short) +
  labs(title = "Average Heart Rate Distribution by Season",
       subtitle = paste0("2024: Mean = ", round(hr_stats$mean_hr[hr_stats$season_short == "2024"], 1),
                        " bpm (SD = ", round(hr_stats$sd_hr[hr_stats$season_short == "2024"], 1), ")\n",
                        "2025: Mean = ", round(hr_stats$mean_hr[hr_stats$season_short == "2025"], 1),
                        " bpm (SD = ", round(hr_stats$sd_hr[hr_stats$season_short == "2025"], 1), ")"),
       x = "Season",
       y = "Average Heart Rate (bpm)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  guides(fill = guide_legend(
    title = "Legend:",
    override.aes = list(alpha = 1),
    nrow = 1
  )) +
  annotate("text", x = 0.5, y = Inf, vjust = 2, hjust = 0,
           label = "Box = IQR (25th-75th percentile)\nLine = Median\nWhiskers = 1.5×IQR\nPoints = Outliers",
           size = 3, color = "gray30", fontface = "italic", lineheight = 0.9)

ggsave(file.path(output_dir, "heart_rate_boxplot_final.png"), p3a_boxplot,
       width = 9, height = 7, dpi = 300, bg = "white")
cat("  ✓ heart_rate_boxplot_final.png\n")

# Create Heart Rate Scatter Plot
p3a_scatter <- ggplot(hr_data, aes(x = season_short, y = avg_heart_rate, color = hr_zone)) +
  geom_jitter(width = 0.25, alpha = 0.7, size = 2.5) +
  scale_color_manual(values = hr_zone_colors,
                     name = "HR Zone",
                     labels = c("Zone 1 (≤114/117 bpm)", "Zone 2 (115-151/118-157 bpm)",
                               "Zone 3 (152-169/158-177 bpm)", "Zone 4 (170-187/178-193 bpm)",
                               "Zone 5 (≥188/194 bpm)")) +
  labs(title = "Individual Ride Heart Rates by Zone and Season",
       subtitle = "Each point represents one ride, colored by heart rate zone (2024/2025 zones)",
       x = "Season",
       y = "Average Heart Rate (bpm)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 3)))

ggsave(file.path(output_dir, "heart_rate_scatter_final.png"), p3a_scatter,
       width = 9, height = 7, dpi = 300, bg = "white")
cat("  ✓ heart_rate_scatter_final.png\n")

# ==============================================================================
# VIZ 4a & 4b: Power Box Plots and Scatter Plots
# ==============================================================================

# Filter for activities with valid power data
power_data <- season_data %>%
  filter(!is.na(avg_power)) %>%
  select(season, season_short, avg_power, power_zone)

# Calculate statistics for annotation
power_stats <- power_data %>%
  group_by(season_short) %>%
  summarise(
    mean_power = mean(avg_power, na.rm = TRUE),
    sd_power = sd(avg_power, na.rm = TRUE),
    median_power = median(avg_power, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Create Power Box Plot
p3b_boxplot <- ggplot(power_data, aes(x = season_short, y = avg_power, fill = season_short)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 2.5,
               outlier.fill = "gray50", outlier.color = "black",
               width = 0.5) +
  scale_fill_manual(values = season_colors_short) +
  labs(title = "Average Power Distribution by Season",
       subtitle = paste0("2024: Mean = ", round(power_stats$mean_power[power_stats$season_short == "2024"], 1),
                        " W (SD = ", round(power_stats$sd_power[power_stats$season_short == "2024"], 1), ")\n",
                        "2025: Mean = ", round(power_stats$mean_power[power_stats$season_short == "2025"], 1),
                        " W (SD = ", round(power_stats$sd_power[power_stats$season_short == "2025"], 1), ")"),
       x = "Season",
       y = "Average Power (watts)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  guides(fill = guide_legend(
    title = "Legend:",
    override.aes = list(alpha = 1),
    nrow = 1
  )) +
  annotate("text", x = 0.5, y = Inf, vjust = 2, hjust = 0,
           label = "Box = IQR (25th-75th percentile)\nLine = Median\nWhiskers = 1.5×IQR\nPoints = Outliers",
           size = 3, color = "gray30", fontface = "italic", lineheight = 0.9)

ggsave(file.path(output_dir, "power_boxplot_final.png"), p3b_boxplot,
       width = 9, height = 7, dpi = 300, bg = "white")
cat("  ✓ power_boxplot_final.png\n")

# Create Power Scatter Plot
p3b_scatter <- ggplot(power_data, aes(x = season_short, y = avg_power, color = power_zone)) +
  geom_jitter(width = 0.25, alpha = 0.7, size = 2.5) +
  scale_color_manual(values = power_zone_colors,
                     name = "Power Zone",
                     labels = c("Zone 1 (≤169/179 W)", "Zone 2 (170-231/180-257 W)",
                               "Zone 3 (232-277/258-293 W)", "Zone 4 (278-323/294-341 W)",
                               "Zone 5 (324-370/342-390 W)", "Zone 6 (371-462/391-488 W)",
                               "Zone 7 (≥463/489 W)")) +
  labs(title = "Individual Ride Power Output by Zone and Season",
       subtitle = "Each point represents one ride, colored by power zone (2024/2025 zones)",
       x = "Season",
       y = "Average Power (watts)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 3)))

ggsave(file.path(output_dir, "power_scatter_final.png"), p3b_scatter,
       width = 9, height = 7, dpi = 300, bg = "white")
cat("  ✓ power_scatter_final.png\n")

# ==============================================================================
# SECTION B: SEASON_COMPARISON_CHARTS.R OUTPUTS (3 PLOTS)
# ==============================================================================

cat("\nGenerating visualizations from season_comparison_charts.R...\n")

# ==============================================================================
# FIG 1: Volume Comparison Bar Chart (Grouped)
# ==============================================================================

volume_summary <- season_data %>%
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
      metric == "total_distance_miles" ~ format(round(value), big.mark = ",", scientific = FALSE),
      metric == "total_hours" ~ format(round(value), big.mark = ",", scientific = FALSE),
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
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)
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

ggsave(file.path(output_dir, "fig1_volume_comparison_final.png"), fig1,
       width = 8, height = 6, dpi = 300, bg = "white")
cat("  ✓ fig1_volume_comparison_final.png\n")

# ==============================================================================
# FIG 2: Training Consistency Line Chart
# ==============================================================================

consistency_df <- season_data %>%
  mutate(
    # Create a normalized month for comparison
    season_month = case_when(
      season == "2024 Season" ~ month - 2,  # Mar=1, Apr=2, ..., Oct=8
      season == "2025 Season" ~ month - 3   # Apr=1, May=2, ..., Oct=7
    )
  ) %>%
  group_by(season, month, month_name) %>%
  summarise(
    rides_count = n(),
    .groups = "drop"
  ) %>%
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

ggsave(file.path(output_dir, "fig2_training_consistency_final.png"), fig2,
       width = 10, height = 6, dpi = 300, bg = "white")
cat("  ✓ fig2_training_consistency_final.png\n")

# ==============================================================================
# FIG 3: Heart Rate Zone Distribution
# ==============================================================================

hr_zone_summary <- season_data %>%
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
  scale_fill_manual(values = hr_zone_colors) +
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

ggsave(file.path(output_dir, "fig3_hr_zone_distribution_final.png"), fig3,
       width = 10, height = 7, dpi = 300, bg = "white")
cat("  ✓ fig3_hr_zone_distribution_final.png\n")

# ==============================================================================
# FIG 4: Power Zone Distribution
# ==============================================================================

power_zone_summary <- season_data %>%
  filter(!is.na(power_zone)) %>%
  group_by(season, power_zone) %>%
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
power_zone_sd <- power_zone_summary %>%
  group_by(season) %>%
  summarise(
    sd_percentage = sd(percentage),
    .groups = "drop"
  )

# Add SD to the data for annotation
power_zone_summary <- power_zone_summary %>%
  left_join(power_zone_sd, by = "season")

fig4 <- ggplot(power_zone_summary, aes(x = power_zone, y = percentage, fill = power_zone)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            vjust = -0.5, size = 3) +
  # Add SD annotation
  geom_text(
    data = power_zone_sd,
    aes(x = 6.5, y = Inf, label = sprintf("SD = %.1f%%", sd_percentage)),
    inherit.aes = FALSE,
    vjust = 1.5,
    hjust = 1,
    size = 3.5,
    fontface = "italic"
  ) +
  facet_wrap(~season, ncol = 2) +
  scale_fill_manual(values = power_zone_colors) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    limits = c(0, NA),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Figure 4: Power Zone Distribution by Season",
    subtitle = paste0(
      "2024 Zones: Z1 ≤169, Z2 170-231, Z3 232-277, Z4 278-323, Z5 324-370, Z6 371-462, Z7 ≥463 W\n",
      "2025 Zones: Z1 ≤179, Z2 180-257, Z3 258-293, Z4 294-341, Z5 342-390, Z6 391-488, Z7 ≥489 W"
    ),
    x = "Power Zone",
    y = "Percentage of Rides (%)",
    fill = "Power Zone"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 8),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave(file.path(output_dir, "fig4_power_zone_distribution_final.png"), fig4,
       width = 10, height = 7, dpi = 300, bg = "white")
cat("  ✓ fig4_power_zone_distribution_final.png\n")

# ==============================================================================
# 7. PRINT SUMMARY STATISTICS
# ==============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SUMMARY STATISTICS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

cat("\n--- Season Summary ---\n")
print(season_summary)

cat("\n--- Heart Rate Statistics ---\n")
print(hr_stats)

cat("\n--- Power Statistics ---\n")
print(power_stats)

cat("\n--- HR Zone Distribution ---\n")
hr_zone_summary %>%
  select(season, hr_zone, percentage) %>%
  pivot_wider(names_from = season, values_from = percentage) %>%
  print()

cat("\n--- HR Zone Standard Deviation ---\n")
print(hr_zone_sd)

cat("\n--- Power Zone Distribution ---\n")
power_zone_summary %>%
  select(season, power_zone, percentage) %>%
  pivot_wider(names_from = season, values_from = percentage) %>%
  print()

cat("\n--- Power Zone Standard Deviation ---\n")
print(power_zone_sd)

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("All visualizations saved to:", output_dir, "\n")
cat("Total files generated: 11\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
