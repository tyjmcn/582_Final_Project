# Generate PDF Report with All Cycling Visualizations
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
cycling_data <- cycling_data %>%
  mutate(is_indoor = sub_sport %in% c("indoor_cycling", "virtual_activity"))

# Define cycling seasons
cycling_data <- cycling_data %>%
  mutate(season = case_when(
    year == 2024 & month >= 3 & month <= 10 ~ "2024",
    year == 2025 & month >= 3 & month <= 10 ~ "2025",
    TRUE ~ NA_character_
  ))

# Filter for in-season rides only
season_data <- cycling_data %>%
  filter(season %in% c("2024", "2025"))

# Convert distance from meters to miles
season_data <- season_data %>%
  mutate(distance_miles = total_distance / 1609.34,
         total_hours = total_elapsed_time / 3600)

# Define zone functions
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

# Apply zone classification
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

# Prepare data
hr_data <- season_data %>%
  filter(!is.na(avg_heart_rate)) %>%
  select(season, avg_heart_rate, hr_zone)

power_data <- season_data %>%
  filter(!is.na(avg_power)) %>%
  select(season, avg_power, power_zone)

# Calculate statistics
hr_stats <- hr_data %>%
  group_by(season) %>%
  summarise(
    mean_hr = mean(avg_heart_rate, na.rm = TRUE),
    sd_hr = sd(avg_heart_rate, na.rm = TRUE),
    .groups = "drop"
  )

power_stats <- power_data %>%
  group_by(season) %>%
  summarise(
    mean_power = mean(avg_power, na.rm = TRUE),
    sd_power = sd(avg_power, na.rm = TRUE),
    .groups = "drop"
  )

# Open PDF device
pdf("Rplots.pdf", width = 11, height = 8.5)

# ============================================================================
# Page 1: Heart Rate Boxplot
# ============================================================================

p1 <- ggplot(hr_data, aes(x = season, y = avg_heart_rate, fill = season)) +
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
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40", margin = margin(b = 20)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(fill = guide_legend(title = "Season:", nrow = 1)) +
  annotate("text", x = 0.6, y = Inf, vjust = 1.2, hjust = 0,
           label = "Box = IQR (25th-75th percentile)\nLine = Median\nWhiskers = 1.5×IQR\nPoints = Outliers",
           size = 4, color = "gray30", fontface = "italic", lineheight = 0.9)

print(p1)

# ============================================================================
# Page 2: Heart Rate Scatter
# ============================================================================

p2 <- ggplot(hr_data, aes(x = season, y = avg_heart_rate, color = hr_zone)) +
  geom_jitter(width = 0.25, alpha = 0.7, size = 3) +
  scale_color_manual(values = c("Zone 1" = "#00CC00", "Zone 2" = "#66CC00",
                                "Zone 3" = "#FFCC00", "Zone 4" = "#FF6600",
                                "Zone 5" = "#FF0000"),
                     name = "HR Zone",
                     labels = c("Zone 1 (≤114/117 bpm)",
                               "Zone 2 (115-151/118-155 bpm)",
                               "Zone 3 (152-169/156-174 bpm)",
                               "Zone 4 (170-187/175-193 bpm)",
                               "Zone 5 (≥188/194 bpm)")) +
  labs(title = "Individual Ride Heart Rates by Zone and Season",
       subtitle = "Each point represents one ride, colored by heart rate zone (2024/2025 zones)",
       x = "Season",
       y = "Average Heart Rate (bpm)") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40", margin = margin(b = 20)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4)))

print(p2)

# ============================================================================
# Page 3: Power Boxplot
# ============================================================================

p3 <- ggplot(power_data, aes(x = season, y = avg_power, fill = season)) +
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
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40", margin = margin(b = 20)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(fill = guide_legend(title = "Season:", nrow = 1)) +
  annotate("text", x = 0.6, y = Inf, vjust = 1.2, hjust = 0,
           label = "Box = IQR (25th-75th percentile)\nLine = Median\nWhiskers = 1.5×IQR\nPoints = Outliers",
           size = 4, color = "gray30", fontface = "italic", lineheight = 0.9)

print(p3)

# ============================================================================
# Page 4: Power Scatter
# ============================================================================

p4 <- ggplot(power_data, aes(x = season, y = avg_power, color = power_zone)) +
  geom_jitter(width = 0.25, alpha = 0.7, size = 3) +
  scale_color_manual(values = c("Zone 1" = "#00CC00", "Zone 2" = "#66CC00",
                                "Zone 3" = "#CCCC00", "Zone 4" = "#FFCC00",
                                "Zone 5" = "#FF9900", "Zone 6" = "#FF6600",
                                "Zone 7" = "#FF0000"),
                     name = "Power Zone",
                     labels = c("Zone 1 (≤169/179 W)",
                               "Zone 2 (170-231/180-257 W)",
                               "Zone 3 (232-277/258-293 W)",
                               "Zone 4 (278-323/294-341 W)",
                               "Zone 5 (324-370/342-390 W)",
                               "Zone 6 (371-462/391-488 W)",
                               "Zone 7 (≥463/489 W)")) +
  labs(title = "Individual Ride Power Output by Zone and Season",
       subtitle = "Each point represents one ride, colored by power zone (2024/2025 zones)",
       x = "Season",
       y = "Average Power (watts)") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40", margin = margin(b = 20)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4)))

print(p4)

# Close PDF device
dev.off()

cat("\nPDF report generated: Rplots.pdf\n")
cat("4 pages: HR Boxplot, HR Scatter, Power Boxplot, Power Scatter\n")
