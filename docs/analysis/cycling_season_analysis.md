# Cycling Season Analysis: 2024 vs 2025

## Overview

This analysis compares cycling performance data from the 2024 and 2025 seasons (March-October) using Garmin activity exports.

## Visualizations

All figures are saved to `outputs/figures/`:

- `season_comparison_barplot.png` - Grouped bar chart comparing total distance, hours, and rides
- `monthly_rides_lineplot.png` - Line graph of rides per month with indoor rides marked as triangles
- `hr_power_combined_boxplot.png` - Combined heart rate and power box plots
- `heart_rate_boxplot.png` - Heart rate distribution by season
- `power_boxplot.png` - Power distribution by season

## Key Findings

### Season Volume Comparison

| Metric | 2024 Season | 2025 Season | Change |
|--------|-------------|-------------|--------|
| Total Distance | 4,454 miles | 8,045 miles | +81% |
| Total Hours | 274 hours | 511 hours | +87% |
| Number of Rides | 189 rides | 318 rides | +68% |

The 2025 season shows significant increases across all volume metrics, indicating a substantial increase in training load.

### Heart Rate Trends

| Season | Mean HR | Standard Deviation | Median HR | Sample Size |
|--------|---------|-------------------|-----------|-------------|
| 2024 | 142.2 bpm | 15.6 bpm | 144 bpm | 179 rides |
| 2025 | 138.9 bpm | 13.2 bpm | 139 bpm | 314 rides |

**Key Insight:** Average heart rate decreased by ~3 bpm in 2025 despite higher training volume. This suggests improved cardiovascular efficiency and aerobic fitness adaptation.

### Power Trends

| Season | Mean Power | Standard Deviation | Median Power | Sample Size |
|--------|------------|-------------------|--------------|-------------|
| 2024 | 200.0 W | 35.6 W | 205 W | 187 rides |
| 2025 | 199.2 W | 41.7 W | 206 W | 308 rides |

**Key Insight:** Power output remained consistent between seasons while heart rate decreased. Maintaining the same power at a lower heart rate indicates improved fitness and efficiency.

### Zone Profiles

Different zone thresholds were used for each season to reflect fitness changes:

**2024 Heart Rate Zones:**
- Zone 1: 0-114 bpm
- Zone 2: 115-151 bpm
- Zone 3: 152-169 bpm
- Zone 4: 170-187 bpm
- Zone 5: >188 bpm

**2025 Heart Rate Zones:**
- Zone 1: 0-117 bpm
- Zone 2: 118-155 bpm
- Zone 3: 156-174 bpm
- Zone 4: 175-193 bpm
- Zone 5: >194 bpm

**2024 Power Zones:**
- Zone 1: 0-169 W
- Zone 2: 170-231 W
- Zone 3: 232-277 W
- Zone 4: 278-323 W
- Zone 5: 324-370 W
- Zone 6: 371-462 W
- Zone 7: >463 W

**2025 Power Zones:**
- Zone 1: 0-179 W
- Zone 2: 180-257 W
- Zone 3: 258-293 W
- Zone 4: 294-341 W
- Zone 5: 342-390 W
- Zone 6: 391-488 W
- Zone 7: >489 W

The higher zone thresholds in 2025 reflect increased FTP (Functional Threshold Power) and improved fitness capacity.

## Conclusions

1. **Training Volume:** 2025 saw a dramatic increase in training volume (~80% more miles and hours)
2. **Cardiovascular Adaptation:** Lower average heart rate at similar power outputs demonstrates improved aerobic efficiency
3. **Fitness Progression:** Higher power zone thresholds in 2025 confirm increased FTP and overall cycling fitness
4. **Consistency:** Similar power output distribution suggests maintained training intensity despite increased volume

## Methods

Data was extracted from Garmin activity exports and analyzed using R with ggplot2. Indoor rides (virtual_activity and indoor_cycling sub_sports) were identified and marked separately in visualizations.

Analysis script: `scripts/analysis/cycling_visualizations.R`
