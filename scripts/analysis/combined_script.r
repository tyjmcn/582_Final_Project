library(tidyverse)

# =============================================================================
# DATA LOADING AND PREPROCESSING
# =============================================================================

# Load cleaned data from CSV
data <- read.csv("data/cleaned/garmin_cleaned.csv", stringsAsFactors = FALSE)

# Remap season labels and create is_indoor flag
data <- data %>%
  mutate(
    # Python uses "2024 Season" / "2025 Season", R expects "2024" / "2025"
    season = case_when(
      season == "2024 Season" ~ "2024",
      season == "2025 Season" ~ "2025",
      TRUE ~ NA_character_
    ),
    # Python created ride_type ("Indoor"/"Outdoor"), create is_indoor flag
    is_indoor = (ride_type == "Indoor")
  )

# Filter for in-season rides only (remove off-season and NAs)
season_data <- data %>%
  filter(season %in% c("2024", "2025"))

# Rename moving_hours to total_hours for consistency
season_data <- season_data %>%
  rename(total_hours = moving_hours)

# Optional: Save processed data for future use
saveRDS(season_data, "data/processed/season_data.rds")

# =============================================================================
# VISUALIZATION 1: Grouped Bar Chart - 2024 vs 2025 Season Comparison
# =============================================================================

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
  pivot_longer(
    cols = c(total_distance_miles, total_hours, total_rides),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric,
      levels = c("total_distance_miles", "total_hours", "total_rides"),
      labels = c("Total Distance (miles)", "Total Hours", "Number of Rides")
    )
  )

# Create faceted bar chart
p1 <- ggplot(season_long, aes(x = season, y = value, fill = season)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", size = 0.3) +
  geom_text(aes(label = round(value, 1)),
    vjust = -0.5, size = 4, fontface = "bold"
  ) +
  facet_wrap(~metric, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("2024" = "#2E86AB", "2025" = "#E94F37")) +
  labs(
    title = "Cycling Season Comparison: 2024 vs 2025",
    subtitle = "March through October of each year",
    x = "Season",
    y = "Value",
    fill = "Season"
  ) +
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
  width = 12, height = 6, dpi = 300, bg = "white"
)

cat("Visualization 1 saved: outputs/figures/season_comparison_barplot.png\n")

# =============================================================================
# VISUALIZATION 2: Monthly Ride Frequency - Separate Charts for 2024 and 2025
# =============================================================================

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

# 2025 Season: March-October (months 3-10) - adjust if data starts later
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

# Process both seasons
data_2024 <- get_trend_data(monthly_2024)
data_2025 <- get_trend_data(monthly_2025)

# Month labels
month_labels <- c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
month_mapping <- setNames(month_labels, 3:10)

# Create 2024 visualization
p2_2024 <- ggplot() +
  geom_bar(
    data = data_2024$data,
    aes(x = factor(month_num, levels = 3:10), y = count, fill = ride_type),
    stat = "identity", position = "stack", width = 0.6
  ) +
  geom_line(
    data = data_2024$trends %>% filter(ride_type == "Indoor"),
    aes(x = as.numeric(factor(month_num, levels = 3:10)), y = trend_value, linetype = "Indoor"),
    color = "#2E86AB", size = 1, alpha = 0.8
  ) +
  geom_line(
    data = data_2024$trends %>% filter(ride_type == "Outdoor"),
    aes(x = as.numeric(factor(month_num, levels = 3:10)), y = trend_value, linetype = "Outdoor"),
    color = "#E94F37", size = 1, alpha = 0.8
  ) +
  geom_line(
    data = data_2024$trends %>% filter(ride_type == "Total"),
    aes(x = as.numeric(factor(month_num, levels = 3:10)), y = trend_value, linetype = "Total"),
    color = "black", size = 1.2, alpha = 0.6
  ) +
  scale_fill_manual(
    name = "Ride Type",
    values = c("Indoor" = "#2E86AB", "Outdoor" = "#E94F37")
  ) +
  scale_linetype_manual(
    name = "Trend Line",
    values = c("Indoor" = "dashed", "Outdoor" = "dotted", "Total" = "solid")
  ) +
  scale_x_discrete(labels = month_mapping) +
  labs(
    title = "2024 Season: Monthly Ride Frequency",
    subtitle = "Indoor vs Outdoor rides with trend lines",
    x = "Month",
    y = "Number of Rides"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  guides(
    fill = guide_legend(order = 1, nrow = 1),
    linetype = guide_legend(order = 2, nrow = 1)
  )

ggsave("outputs/figures/monthly_rides_2024.png", p2_2024,
  width = 10, height = 7, dpi = 300, bg = "white"
)

# Create 2025 visualization
month_range_2025 <- sort(unique(data_2025$data$month_num))
month_levels_2025 <- month_range_2025
month_labels_2025 <- month_mapping[as.character(month_range_2025)]

p2_2025 <- ggplot() +
  geom_bar(
    data = data_2025$data,
    aes(x = factor(month_num, levels = month_levels_2025), y = count, fill = ride_type),
    stat = "identity", position = "stack", width = 0.6
  ) +
  geom_line(
    data = data_2025$trends %>% filter(ride_type == "Indoor"),
    aes(x = as.numeric(factor(month_num, levels = month_levels_2025)), y = trend_value, linetype = "Indoor"),
    color = "#2E86AB", size = 1, alpha = 0.8
  ) +
  geom_line(
    data = data_2025$trends %>% filter(ride_type == "Outdoor"),
    aes(x = as.numeric(factor(month_num, levels = month_levels_2025)), y = trend_value, linetype = "Outdoor"),
    color = "#E94F37", size = 1, alpha = 0.8
  ) +
  geom_line(
    data = data_2025$trends %>% filter(ride_type == "Total"),
    aes(x = as.numeric(factor(month_num, levels = month_levels_2025)), y = trend_value, linetype = "Total"),
    color = "black", size = 1.2, alpha = 0.6
  ) +
  scale_fill_manual(
    name = "Ride Type",
    values = c("Indoor" = "#2E86AB", "Outdoor" = "#E94F37")
  ) +
  scale_linetype_manual(
    name = "Trend Line",
    values = c("Indoor" = "dashed", "Outdoor" = "dotted", "Total" = "solid")
  ) +
  scale_x_discrete(labels = month_labels_2025) +
  labs(
    title = "2025 Season: Monthly Ride Frequency",
    subtitle = "Indoor vs Outdoor rides with trend lines",
    x = "Month",
    y = "Number of Rides"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  guides(
    fill = guide_legend(order = 1, nrow = 1),
    linetype = guide_legend(order = 2, nrow = 1)
  )

ggsave("outputs/figures/monthly_rides_2025.png", p2_2025,
  width = 10, height = 7, dpi = 300, bg = "white"
)

cat("Visualization 2a saved: outputs/figures/monthly_rides_2024.png\n")
cat("Visualization 2b saved: outputs/figures/monthly_rides_2025.png\n")

# =============================================================================
# VISUALIZATION 3: Heart Rate and Power Zone Analysis (PLACEHOLDER)
# =============================================================================

# NOTE: Script 2 had incomplete zone analysis code
# Define heart rate zone functions for each season
get_hr_zone_2024 <- function(hr) {
  if (is.na(hr)) return(NA_character_)
  if (hr <= 114) return("Zone 1")
  if (hr <= 151) return("Zone 2")
  if (hr <= 169) return("Zone 3")
  if (hr <= 187) return("Zone 4")
  return("Zone 5")
}

get_hr_zone_2025 <- function(hr) {
  if (is.na(hr)) return(NA_character_)
  if (hr <= 117) return("Zone 1")
  if (hr <= 157) return("Zone 2")
  if (hr <= 177) return("Zone 3")
  if (hr <= 193) return("Zone 4")
  return("Zone 5")
}

# Power zone functions would go here (script was cut off)
# You'll need to complete this section based on your power zone definitions

cat("\nAll visualizations completed successfully!\n")