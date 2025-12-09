# Cycling Training Data Analysis: Comparing 2024 and 2025 Seasons

## Executive Summary

In this document, I compare personal cycling data across two normalized training seasons: March-October 2024 and April-October 2025. Data was recorded via a Garmin cycling computer connected to heart rate monitors and power meters. The goal is to compare training volume, consistency, and intensity distributions within and across seasons, using a total of 507 activities.

From 2024 to 2025, training volume and frequency increased substantially: distance rose 81% (4,454 to 8,045 miles), training hours by 84% (230 to 422 hours), and ride count by 68% (189 to 318 rides).

Comparing biological data to physical data shows little measured change in physiological metrics despite the volume increase. The average heart rate decreased by 2% from 141.7 bpm to 138.9 bpm. The average power decreased by 0.2% from 199.6 watts to 199.2 watts. This suggests improved cardiovascular efficiency: a higher training load was achieved with lower relative stress. The general assumption is that off-season training combined with stronger structure and consistency in the 2025 season enabled this adaptation.

## Introduction

This analysis examines personal health data from hundreds of cycling activities across two training seasons. The data was recorded via a Garmin cycling computer connected to various heart rate monitors (at least two were frequently used with a minimal amount of other devices included) and a power meter, where again, multiple devices are used, but the stated margin of error between devices is low enough to ignore.

## Questions

This analysis addresses three specific research questions comparing the 2024 season (March-October 2024) to the 2025 season (April-October 2025):

1. **Training Volume Evolution**: How did total training volume differ between seasons in terms of distance, time, and ride frequency?
2. **Training Consistency**: What patterns emerge in monthly ride frequency, and how does consistency vary between seasons?
3. **Physiological Intensity**: How do heart rate and power distributions compare between seasons, and what do these patterns indicate about training adaptations?

The dataset consists of GPS cycling computer recordings from my Garmin Edge device, capturing detailed metrics for each ride, including distance, duration, heart rate, and power output. Each activity record includes timestamp data, location information, and performance metrics recorded at one-second intervals, then aggregated to ride-level statistics.

## Concerns

1. **Collection Methods**: The data was collected on a bike, outside of a trial or replicable environment. Many external factors may be at play during any moment of collection that cannot be accounted for in the final analysis.

2. **Lack of Daily Health Metrics**: In tandem with the above issue, no continuous data regarding any biological factors that may impact or inform recovery or non-recorded activities, athletic or otherwise, was easily available and has the potential to skew the results.

3. **Data Recording Devices**: Different heart rate or power meter devices are used for various activities. While industry standards aim for nearly identical measurements and similar standards of error, using multiple devices introduces potential inconsistency in measurement results.

4. **Fitness "Zone" Changes**: Across seasons, the zones used to quantify my data changed. This is expected and normal as my fitness improves, but it will moderately change what simply saying "Zone 2" means in 2024 vs. 2025. Power zones in 2024 were based on an indoor ramp test and estimated FTP. Power zones in 2025 were based on an outdoor 20-minute test. Heart rate zones across each season were calculated by using the maximal recorded heart rate and automatically applied via Garmin's calculations.

5. **Lack of Data**: Due to the convention in how data is exported or saved from virtual cycling programs, speed data is missing in approximately half of all the entries. This was attempted to be remedied, but consistent export proved too troublesome for the scope of the activity. Speed and speed-related calculations are thus not relevant to this analysis. If necessary, a script could be written to calculate the average speed of rides, despite lacking the granularity or context.

## Data Collection

Data was collected through a Garmin Edge 530 & 840 cycling computer across both training seasons. All outdoor rides utilized GPS tracking with an integrated power meter and heart rate monitor. Indoor rides used a smart trainer with controllable resistance and integrated power measurement. Data was automatically synchronized to the Garmin Connect cloud platform after each activity.

For this analysis, I exported the complete activity history from Garmin Connect covering January 2024 through October 2025. The export was performed on October 28, 2025, using Garmin Connect's bulk data export feature, which provides CSV-formatted activity summaries. The export included 714 total cycling activities containing metrics for distance (meters), moving time (seconds), average heart rate (bpm), average power (watts), activity type, and timestamp data. This data was then cleaned via a Python script and trimmed down to the 507 entries used for the seasonal analysis.

## Data Structures

The exported Garmin data is structured as a flat CSV file with one row per activity. Key variables include:

| Variable | Description |
|----------|-------------|
| start_time | Activity start timestamp (YYYY-MM-DD HH:MM:SS format) |
| sport | Activity type (filtered to "cycling" only for this analysis) |
| sub_sport | Specific activity subcategory (outdoor, indoor_cycling, virtual_activity) |
| total_distance | Distance in meters |
| total_elapsed_time | Total activity duration in seconds |
| avg_heart_rate | Average heart rate in beats per minute (bpm) |
| avg_power | Average power output in watts (W) |

### Additional Variables

- Season classification (2024/2025)
- Distance in miles (converted from meters)
- Duration in hours (converted from seconds)
- Training zone classifications for both heart rate and power based on physiologically determined thresholds

## Data Cleaning

Data cleaning involved several filtering and transformation steps:

1. The dataset was filtered to include only cycling activities, excluding running, swimming, and other sports recorded by the same device. Activities were then classified by season based on timestamp: the 2024 season includes March-October 2024, while the 2025 season includes April-October 2025. These date ranges represent competitive cycling seasons in Wisconsin, where weather permits outdoor training.

2. Indoor versus outdoor ride classification was determined using the sub_sport field, where "indoor_cycling" or "virtual_activity" designates indoor rides. Distance values were converted from meters to miles (dividing by 1609.34) and time from seconds to hours (dividing by 3600) for easier interpretation. Activities with missing heart rate or power data were retained for volume calculations but excluded from physiological intensity analyses, resulting in different sample sizes across visualizations.

3. Heart rate and power zones were calculated using season-specific thresholds based on physiological testing.

### Heart Rate Zones

| Zone | 2024 Thresholds | 2025 Thresholds |
|------|-----------------|-----------------|
| Zone 1 | ≤114 bpm | ≤117 bpm |
| Zone 2 | 115-151 bpm | 118-155 bpm |
| Zone 3 | 152-169 bpm | 156-174 bpm |
| Zone 4 | 170-187 bpm | 175-193 bpm |
| Zone 5 | ≥188 bpm | ≥194 bpm |

Power zones followed similar season-specific classifications based on functional threshold power testing.

## Data Structure Example

The data was provided to me in the form of a .csv file from Garmin. After cleaning, I opted to keep the file in the same format. Below is a display of the header and first row of the data.
```
filename,sport,sub_sport,ride_type,start_time,date,year,month,day_of_week,season,total_elapsed_time,total_timer_time,elapsed_minutes,moving_minutes,elapsed_hours,moving_hours,time_efficiency,total_distance,distance_km,distance_miles,avg_speed,max_speed,avg_speed_kmh,max_speed_kmh,avg_speed_mph,max_speed_mph,avg_heart_rate,max_heart_rate,avg_cadence,max_cadence,avg_power,max_power,total_ascent,total_descent,total_ascent_ft,total_descent_ft,total_calories,has_hr_data,has_power_data,has_speed_data,has_cadence_data,record_count
```

As a disclosure, this is a remarkably bad example of the data that was included, but for accuracy and convention, I have included the first row of data recorded. The data cleaning process eliminated results like this.

## Data Analysis

A statistical analysis was done using R. Required packages were tidyverse for data manipulation and ggplot2 for data visualization. For each season, metrics such as means, standard deviations, and medians were calculated for comparison and display.

### 2024 Season Metrics

| Metric | Value |
|--------|-------|
| Rides | 189 |
| Distance | 4,454 miles |
| Duration | 230 hours |
| Mean Heart Rate | 141.7 bpm (SD = 15.6) |
| Mean Power | 199.6W (SD = 35.6) |

### 2025 Season Metrics

| Metric | Value |
|--------|-------|
| Rides | 318 |
| Distance | 8,045 miles |
| Duration | 422 hours |
| Mean Heart Rate | 138.9 bpm (SD = 13.2) |
| Mean Power | 199.2W (SD = 41.7) |

Visualizations were created to illustrate season comparisons through bar charts (total volume metrics), line plots (monthly frequency trends), box plots (heart rate and power distributions), and scatter plots (individual ride intensities colored by training zone).

## Discussion and Conclusions

The analysis successfully answered all three research questions, revealing substantial training evolution between seasons. The 2025 season demonstrated 68-81% increases across all volume metrics while maintaining similar intensity levels, suggesting successful periodization and base-building. The decreased average heart rate at equivalent power outputs indicates improved cardiovascular efficiency.

Monthly consistency improved dramatically in 2025, with more evenly distributed training load rather than the pronounced mid-season peak observed in 2024.

## Future Improvements

Future analyses could incorporate race performance data to correlate training metrics with competitive outcomes. Analyzing intra-ride data (second-by-second recordings) rather than ride averages would provide deeper insight into intensity distribution within individual workouts. Integration with recovery metrics (sleep data, resting heart rate) would enable more comprehensive training load monitoring.

## AI Disclosure Statement

The use of AI tools was valuable in creating this data analysis. To begin, I used the project's guidelines and rubrics to assess the proposed prompt and data suitability for the project. After reviewing adherence to the project, I then mapped my goals and requirements in collaboration with AI to create a clear roadmap. As I approached the process of cleaning and analyzing data, my prior experience using R and Python was aided by the usage of AI tools in my IDE that auto-check and suggest code based on stated goals and conventions. With a solid fundamental understanding of Python and R, I was able to set up my environments and organize my data and code while using AI to correct errors, catch deficiencies and redundancy, as well as keep my code organized. Lastly, Grammarly's writing tools were used to address grammar and speech conventions for better presentation.