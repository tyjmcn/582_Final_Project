# Cycling Data Visualizations for 2024 vs 2025 Season Analysis
# INFOST 582 Final Project

library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# Load data
data <- read.csv("data/raw/garmin_activities.csv", stringsAsFactors = FALSE)

# Convert start_time to datetime and extract components
data$start_time <- as.POSIXct(data$start_time, format = "%Y-%m-%d %H:%M:%S")
data$year <- year(data$start_time)
data$month <- month(data$start_time)
data$month_name <- month(data$start_time, label = TRUE, abbr = TRUE)

# Filter for cycling activities only
cycling_data <- data %>%
  filter(sport == "cycling")

# Determine if activity is indoor
# Indoor activities: sub_sport is "indoor_cycling" or "virtual_activity"
cycling_data <- cycling_data %>%
  mutate(is_indoor = sub_sport %in% c("indoor_cycling", "virtual_activity"))

# Define cycling seasons (March - October)
# 2024 season: March 2024 - October 2024
# 2025 season: March 2025 - October 2025
cycling_data <- cycling_data %>%
  mutate(season = case_when(
    year == 2024 & month >= 3 & month <= 10 ~ "2024",
    year == 2025 & month >= 3 & month <= 10 ~ "2025",
    # Include off-season training for context
    year == 2023 & month >= 11 ~ "2024 Off-Season",
    year == 2024 & month <= 2 ~ "2024 Off-Season",
    year == 2024 & month >= 11 ~ "2025 Off-Season",
    year == 2025 & month <= 2 ~ "2025 Off-Season",
    TRUE ~ NA_character_
  ))

# Filter for in-season rides only for main analysis
season_data <- cycling_data %>%
  filter(season %in% c("2024", "2025"))

# Convert distance from meters to miles
season_data <- season_data %>%
  mutate(distance_miles = total_distance / 1609.34,
         total_hours = total_elapsed_time / 3600)

# ============================================================================
# VISUALIZATION 1: Grouped Bar Chart - 2024 vs 2025 Season Comparison
# ============================================================================

# Calculate season totals
season_summary <- season_data %>%
  group_by(season) %>%
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

# Create faceted bar chart for better readability
p1 <- ggplot(season_long, aes(x = season, y = value, fill = season)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", size = 0.3) +
  geom_text(aes(label = round(value, 1)),
            vjust = -0.5, size = 4, fontface = "bold") +
  facet_wrap(~metric, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("2024" = "#2E86AB", "2025" = "#E94F37")) +
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

ggsave("outputs/figures/season_comparison_barplot.png", p1,
       width = 12, height = 6, dpi = 300, bg = "white")

cat("Visualization 1 saved: outputs/figures/season_comparison_barplot.png\n")

# ============================================================================
# VISUALIZATION 2: Monthly Ride Frequency - Separate Charts for 2024 and 2025
# ============================================================================

# Prepare data for monthly rides with indoor/outdoor breakdown
monthly_rides <- season_data %>%
  mutate(month_num = month) %>%
  group_by(season, month_num, is_indoor) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(ride_type = ifelse(is_indoor, "Indoor", "Outdoor"))

# Create complete month grid to ensure all months are represented
expand_months <- function(df, season_name, month_range) {
  complete_grid <- expand.grid(
    season = season_name,
    month_num = month_range,
    ride_type = c("Indoor", "Outdoor"),
    stringsAsFactors = FALSE
  )
  result <- left_join(complete_grid, df, by = c("season", "month_num", "ride_type"))
  result$count[is.na(result$count)] <- 0
  return(result)
}

# 2024 Season: March-October (months 3-10)
monthly_2024 <- monthly_rides %>%
  filter(season == "2024") %>%
  expand_months("2024", 3:10)

# 2025 Season: March-October (months 3-10) - adjust if data starts in April
available_2025_months <- unique(monthly_rides$month_num[monthly_rides$season == "2025"])
monthly_2025 <- monthly_rides %>%
  filter(season == "2025") %>%
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
            color = "#2E86AB", size = 1, alpha = 0.8) +
  geom_line(data = data_2024$trends %>% filter(ride_type == "Outdoor"),
            aes(x = as.numeric(factor(month_num, levels = 3:10)), y = trend_value, linetype = "Outdoor"),
            color = "#E94F37", size = 1, alpha = 0.8) +
  geom_line(data = data_2024$trends %>% filter(ride_type == "Total"),
            aes(x = as.numeric(factor(month_num, levels = 3:10)), y = trend_value, linetype = "Total"),
            color = "black", size = 1.2, alpha = 0.7) +
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
            color = "#2E86AB", size = 1, alpha = 0.8) +
  geom_line(data = data_2025$trends %>% filter(ride_type == "Outdoor"),
            aes(x = as.numeric(factor(month_num, levels = available_2025_months)),
                y = trend_value, linetype = "Outdoor"),
            color = "#E94F37", size = 1, alpha = 0.8) +
  geom_line(data = data_2025$trends %>% filter(ride_type == "Total"),
            aes(x = as.numeric(factor(month_num, levels = available_2025_months)),
                y = trend_value, linetype = "Total"),
            color = "black", size = 1.2, alpha = 0.7) +
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

ggsave("outputs/figures/monthly_rides_2024.png", p2_2024,
       width = 10, height = 7, dpi = 300, bg = "white")

ggsave("outputs/figures/monthly_rides_2025.png", p2_2025,
       width = 10, height = 7, dpi = 300, bg = "white")

cat("Visualization 2a saved: outputs/figures/monthly_rides_2024.png\n")
cat("Visualization 2b saved: outputs/figures/monthly_rides_2025.png\n")

# ============================================================================
# VISUALIZATION 3: Heart Rate and Power Zone Box Plots with Statistics
# ============================================================================

# Define zone functions for each year
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
    hr <= 155 ~ "Zone 2",
    hr <= 174 ~ "Zone 3",
    hr <= 193 ~ "Zone 4",
    TRUE ~ "Zone 5"
  )
}

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

# Apply zone classification based on year
season_data <- season_data %>%
  mutate(
    hr_zone = case_when(
      season == "2024" ~ get_hr_zone_2024(avg_heart_rate),
      season == "2025" ~ get_hr_zone_2025(avg_heart_rate),
      TRUE ~ NA_character_
    ),
    power_zone = case_when(
      season == "2024" ~ get_power_zone_2024(avg_power),
      season == "2025" ~ get_power_zone_2025(avg_power),
      TRUE ~ NA_character_
    )
  )

# Filter for activities with valid HR and power data
hr_data <- season_data %>%
  filter(!is.na(avg_heart_rate)) %>%
  select(season, avg_heart_rate, hr_zone)

power_data <- season_data %>%
  filter(!is.na(avg_power)) %>%
  select(season, avg_power, power_zone)

# Calculate statistics for annotation
hr_stats <- hr_data %>%
  group_by(season) %>%
  summarise(
    mean_hr = mean(avg_heart_rate, na.rm = TRUE),
    sd_hr = sd(avg_heart_rate, na.rm = TRUE),
    median_hr = median(avg_heart_rate, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

power_stats <- power_data %>%
  group_by(season) %>%
  summarise(
    mean_power = mean(avg_power, na.rm = TRUE),
    sd_power = sd(avg_power, na.rm = TRUE),
    median_power = median(avg_power, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Create Heart Rate Box Plot (pure boxplot without scatter overlay)
p3a_boxplot <- ggplot(hr_data, aes(x = season, y = avg_heart_rate, fill = season)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 2.5,
               outlier.fill = "gray50", outlier.color = "black",
               width = 0.5) +
  scale_fill_manual(values = c("2024" = "#2E86AB", "2025" = "#E94F37")) +
  labs(title = "Average Heart Rate Distribution by Season",
       subtitle = paste0("2024: Mean = ", round(hr_stats$mean_hr[hr_stats$season == "2024"], 1),
                        " bpm (SD = ", round(hr_stats$sd_hr[hr_stats$season == "2024"], 1), ")\n",
                        "2025: Mean = ", round(hr_stats$mean_hr[hr_stats$season == "2025"], 1),
                        " bpm (SD = ", round(hr_stats$sd_hr[hr_stats$season == "2025"], 1), ")"),
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

# Create Heart Rate Scatter Plot (separate by season with HR zones)
p3a_scatter <- ggplot(hr_data, aes(x = season, y = avg_heart_rate, color = hr_zone)) +
  geom_jitter(width = 0.25, alpha = 0.7, size = 2.5) +
  scale_color_manual(values = c("Zone 1" = "#00CC00", "Zone 2" = "#66CC00",
                                "Zone 3" = "#FFCC00", "Zone 4" = "#FF6600",
                                "Zone 5" = "#FF0000"),
                     name = "HR Zone",
                     labels = c("Zone 1 (≤114/117 bpm)", "Zone 2 (115-151/118-155 bpm)",
                               "Zone 3 (152-169/156-174 bpm)", "Zone 4 (170-187/175-193 bpm)",
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

# Create Power Box Plot (pure boxplot without scatter overlay)
p3b_boxplot <- ggplot(power_data, aes(x = season, y = avg_power, fill = season)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 2.5,
               outlier.fill = "gray50", outlier.color = "black",
               width = 0.5) +
  scale_fill_manual(values = c("2024" = "#2E86AB", "2025" = "#E94F37")) +
  labs(title = "Average Power Distribution by Season",
       subtitle = paste0("2024: Mean = ", round(power_stats$mean_power[power_stats$season == "2024"], 1),
                        " W (SD = ", round(power_stats$sd_power[power_stats$season == "2024"], 1), ")\n",
                        "2025: Mean = ", round(power_stats$mean_power[power_stats$season == "2025"], 1),
                        " W (SD = ", round(power_stats$sd_power[power_stats$season == "2025"], 1), ")"),
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

# Create Power Scatter Plot (separate by season with power zones)
p3b_scatter <- ggplot(power_data, aes(x = season, y = avg_power, color = power_zone)) +
  geom_jitter(width = 0.25, alpha = 0.7, size = 2.5) +
  scale_color_manual(values = c("Zone 1" = "#00CC00", "Zone 2" = "#66CC00",
                                "Zone 3" = "#CCCC00", "Zone 4" = "#FFCC00",
                                "Zone 5" = "#FF9900", "Zone 6" = "#FF6600",
                                "Zone 7" = "#FF0000"),
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

# Save heart rate plots
ggsave("outputs/figures/heart_rate_boxplot_2024_2025.png", p3a_boxplot,
       width = 9, height = 7, dpi = 300, bg = "white")

ggsave("outputs/figures/heart_rate_scatter_2024_2025.png", p3a_scatter,
       width = 9, height = 7, dpi = 300, bg = "white")

# Save power plots
ggsave("outputs/figures/power_boxplot_2024_2025.png", p3b_boxplot,
       width = 9, height = 7, dpi = 300, bg = "white")

ggsave("outputs/figures/power_scatter_2024_2025.png", p3b_scatter,
       width = 9, height = 7, dpi = 300, bg = "white")

cat("Visualization 3a saved: outputs/figures/heart_rate_boxplot_2024_2025.png\n")
cat("Visualization 3b saved: outputs/figures/heart_rate_scatter_2024_2025.png\n")
cat("Visualization 3c saved: outputs/figures/power_boxplot_2024_2025.png\n")
cat("Visualization 3d saved: outputs/figures/power_scatter_2024_2025.png\n")

# Print summary statistics
cat("\n=== Summary Statistics ===\n\n")

cat("Season Summary:\n")
print(season_summary)

cat("\nHeart Rate Statistics:\n")
print(hr_stats)

cat("\nPower Statistics:\n")
print(power_stats)

cat("\nAll visualizations have been saved to outputs/figures/\n")
