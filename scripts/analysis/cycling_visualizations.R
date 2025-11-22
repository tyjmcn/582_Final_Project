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
# VISUALIZATION 2: Line Graph - Rides Per Month (March-October)
# ============================================================================

# Count rides per month, distinguishing indoor vs outdoor
monthly_rides <- season_data %>%
  mutate(month_num = month) %>%
  group_by(season, month_num, is_indoor) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(ride_type = ifelse(is_indoor, "Indoor", "Outdoor"))

# Create month labels for x-axis
month_labels <- c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")

# Pivot wider to get total rides per month for the line
monthly_totals <- season_data %>%
  group_by(season, month) %>%
  summarise(total_rides = n(),
            indoor_rides = sum(is_indoor),
            outdoor_rides = sum(!is_indoor),
            .groups = "drop")

# Create visualization with points colored by ride type
p2 <- ggplot() +
  # Lines for total rides
  geom_line(data = monthly_totals,
            aes(x = month, y = total_rides, color = season, group = season),
            size = 1.2) +
  # Points for outdoor rides
  geom_point(data = season_data %>% filter(!is_indoor) %>%
               group_by(season, month) %>%
               summarise(count = n(), .groups = "drop"),
             aes(x = month, y = count, color = season),
             size = 4, shape = 16) +
  # Points for indoor rides (different shape)
  geom_point(data = season_data %>% filter(is_indoor) %>%
               group_by(season, month) %>%
               summarise(count = n(), .groups = "drop"),
             aes(x = month, y = count, color = season),
             size = 4, shape = 17) +
  scale_x_continuous(breaks = 3:10, labels = month_labels) +
  scale_color_manual(values = c("2024" = "#2E86AB", "2025" = "#E94F37")) +
  labs(title = "Monthly Ride Frequency: 2024 vs 2025 Season",
       subtitle = "Circles = Outdoor Rides, Triangles = Indoor Rides",
       x = "Month",
       y = "Number of Rides",
       color = "Season") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold")
  )

ggsave("outputs/figures/monthly_rides_lineplot.png", p2,
       width = 10, height = 7, dpi = 300, bg = "white")

cat("Visualization 2 saved: outputs/figures/monthly_rides_lineplot.png\n")

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

# Create Heart Rate Box Plot
p3a <- ggplot(hr_data, aes(x = season, y = avg_heart_rate, fill = season)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 2, width = 0.5) +
  geom_jitter(aes(color = hr_zone), width = 0.15, alpha = 0.6, size = 2) +
  scale_fill_manual(values = c("2024" = "#2E86AB", "2025" = "#E94F37")) +
  scale_color_manual(values = c("Zone 1" = "#00CC00", "Zone 2" = "#66CC00",
                                "Zone 3" = "#FFCC00", "Zone 4" = "#FF6600",
                                "Zone 5" = "#FF0000"),
                     name = "HR Zone") +
  labs(title = "Average Heart Rate Distribution by Season",
       subtitle = paste0("2024: Mean = ", round(hr_stats$mean_hr[hr_stats$season == "2024"], 1),
                        " bpm (SD = ", round(hr_stats$sd_hr[hr_stats$season == "2024"], 1), ")\n",
                        "2025: Mean = ", round(hr_stats$mean_hr[hr_stats$season == "2025"], 1),
                        " bpm (SD = ", round(hr_stats$sd_hr[hr_stats$season == "2025"], 1), ")"),
       x = "Season",
       y = "Average Heart Rate (bpm)",
       fill = "Season") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  guides(fill = "none")

# Create Power Box Plot
p3b <- ggplot(power_data, aes(x = season, y = avg_power, fill = season)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 2, width = 0.5) +
  geom_jitter(aes(color = power_zone), width = 0.15, alpha = 0.6, size = 2) +
  scale_fill_manual(values = c("2024" = "#2E86AB", "2025" = "#E94F37")) +
  scale_color_manual(values = c("Zone 1" = "#00CC00", "Zone 2" = "#66CC00",
                                "Zone 3" = "#CCCC00", "Zone 4" = "#FFCC00",
                                "Zone 5" = "#FF9900", "Zone 6" = "#FF6600",
                                "Zone 7" = "#FF0000"),
                     name = "Power Zone") +
  labs(title = "Average Power Distribution by Season",
       subtitle = paste0("2024: Mean = ", round(power_stats$mean_power[power_stats$season == "2024"], 1),
                        " W (SD = ", round(power_stats$sd_power[power_stats$season == "2024"], 1), ")\n",
                        "2025: Mean = ", round(power_stats$mean_power[power_stats$season == "2025"], 1),
                        " W (SD = ", round(power_stats$sd_power[power_stats$season == "2025"], 1), ")"),
       x = "Season",
       y = "Average Power (watts)",
       fill = "Season") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  guides(fill = "none")

# Save individual plots
ggsave("outputs/figures/heart_rate_boxplot.png", p3a,
       width = 9, height = 7, dpi = 300, bg = "white")

ggsave("outputs/figures/power_boxplot.png", p3b,
       width = 9, height = 7, dpi = 300, bg = "white")

# Create combined plot using gridExtra or patchwork
library(gridExtra)

combined_plot <- grid.arrange(p3a, p3b, ncol = 2,
                              top = grid::textGrob("Heart Rate & Power Trends: 2024 vs 2025 Season",
                                                   gp = grid::gpar(fontsize = 18, fontface = "bold")))

ggsave("outputs/figures/hr_power_combined_boxplot.png", combined_plot,
       width = 16, height = 8, dpi = 300, bg = "white")

cat("Visualization 3 saved: outputs/figures/heart_rate_boxplot.png\n")
cat("Visualization 3 saved: outputs/figures/power_boxplot.png\n")
cat("Visualization 3 saved: outputs/figures/hr_power_combined_boxplot.png\n")

# Print summary statistics
cat("\n=== Summary Statistics ===\n\n")

cat("Season Summary:\n")
print(season_summary)

cat("\nHeart Rate Statistics:\n")
print(hr_stats)

cat("\nPower Statistics:\n")
print(power_stats)

cat("\nAll visualizations have been saved to outputs/figures/\n")
