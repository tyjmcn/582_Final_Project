#!/usr/bin/env python3
"""
Garmin Activity Data Cleaning Script
Cleans raw Garmin export data for analysis.
"""

import pandas as pd
from pathlib import Path

# Paths
RAW_PATH = Path(__file__).parent.parent.parent / "data/raw/garmin_activities.csv"
CLEANED_PATH = Path(__file__).parent.parent.parent / "data/cleaned/garmin_cleaned.csv"


def load_raw_data():
    """Load raw Garmin CSV data."""
    df = pd.read_csv(RAW_PATH)
    print(f"Loaded {len(df)} activities from raw data")
    return df


def clean_timestamps(df):
    """Convert start_time to datetime and extract date components."""
    df["start_time"] = pd.to_datetime(df["start_time"])
    df["date"] = df["start_time"].dt.date
    df["year"] = df["start_time"].dt.year
    df["month"] = df["start_time"].dt.month
    df["day_of_week"] = df["start_time"].dt.day_name()
    return df


def add_season_labels(df):
    """
    Add season labels based on cycling season definitions:
    - 2024 Season: Mar-Oct 2024
    - 2025 Season: Apr-Oct 2025
    """
    def get_season(row):
        year, month = row["year"], row["month"]
        if year == 2024 and 3 <= month <= 10:
            return "2024 Season"
        elif year == 2025 and 4 <= month <= 10:
            return "2025 Season"
        else:
            return "Off-Season"

    df["season"] = df.apply(get_season, axis=1)
    return df


def convert_units(df):
    """Convert units to more useful formats."""
    # Distance: meters to kilometers
    df["distance_km"] = df["total_distance"] / 1000

    # Distance: meters to miles
    df["distance_miles"] = df["total_distance"] / 1609.34

    # Time: seconds to minutes
    df["elapsed_minutes"] = df["total_elapsed_time"] / 60
    df["moving_minutes"] = df["total_timer_time"] / 60

    # Time: seconds to hours
    df["elapsed_hours"] = df["total_elapsed_time"] / 3600
    df["moving_hours"] = df["total_timer_time"] / 3600

    # Speed: m/s to km/h
    df["avg_speed_kmh"] = df["avg_speed"] * 3.6
    df["max_speed_kmh"] = df["max_speed"] * 3.6

    # Speed: m/s to mph
    df["avg_speed_mph"] = df["avg_speed"] * 2.237
    df["max_speed_mph"] = df["max_speed"] * 2.237

    # Elevation: meters to feet
    df["total_ascent_ft"] = df["total_ascent"] * 3.281
    df["total_descent_ft"] = df["total_descent"] * 3.281

    return df


def filter_cycling_only(df):
    """Filter to cycling activities only."""
    cycling_df = df[df["sport"] == "cycling"].copy()
    print(f"Filtered to {len(cycling_df)} cycling activities")
    return cycling_df


def add_ride_type(df):
    """Categorize rides by type based on sub_sport."""
    ride_type_map = {
        "road": "Outdoor",
        "mountain": "Outdoor",
        "virtual_activity": "Indoor",
        "indoor_cycling": "Indoor",
        "generic": "Other"
    }
    df["ride_type"] = df["sub_sport"].map(ride_type_map).fillna("Other")
    return df


def calculate_derived_metrics(df):
    """Calculate additional useful metrics."""
    # Efficiency: moving time / elapsed time
    df["time_efficiency"] = df["total_timer_time"] / df["total_elapsed_time"]

    # Intensity Factor proxy (if power available)
    # Normalized power / FTP - would need FTP input

    # Training Stress Score approximation (simplified)
    # TSS = (duration_hours * NP * IF) / (FTP * 3600) * 100

    return df


def handle_missing_values(df):
    """
    Handle missing values appropriately.
    Note: We preserve NaN for missing sensor data rather than imputing.
    """
    # Flag rows with missing key metrics
    df["has_hr_data"] = df["avg_heart_rate"].notna()
    df["has_power_data"] = df["avg_power"].notna()
    df["has_speed_data"] = df["avg_speed"].notna()
    df["has_cadence_data"] = df["avg_cadence"].notna()

    return df


def select_and_order_columns(df):
    """Select and order columns for final output."""
    columns = [
        # Identifiers
        "filename", "sport", "sub_sport", "ride_type",
        # Timestamps
        "start_time", "date", "year", "month", "day_of_week", "season",
        # Duration (original + converted)
        "total_elapsed_time", "total_timer_time",
        "elapsed_minutes", "moving_minutes", "elapsed_hours", "moving_hours",
        "time_efficiency",
        # Distance (original + converted)
        "total_distance", "distance_km", "distance_miles",
        # Speed (original + converted)
        "avg_speed", "max_speed",
        "avg_speed_kmh", "max_speed_kmh", "avg_speed_mph", "max_speed_mph",
        # Heart Rate
        "avg_heart_rate", "max_heart_rate",
        # Cadence
        "avg_cadence", "max_cadence",
        # Power
        "avg_power", "max_power",
        # Elevation (original + converted)
        "total_ascent", "total_descent", "total_ascent_ft", "total_descent_ft",
        # Calories
        "total_calories",
        # Data quality flags
        "has_hr_data", "has_power_data", "has_speed_data", "has_cadence_data",
        # Metadata
        "record_count"
    ]

    # Only include columns that exist
    existing_cols = [c for c in columns if c in df.columns]
    return df[existing_cols]


def main():
    """Main cleaning pipeline."""
    print("=" * 50)
    print("Garmin Data Cleaning Pipeline")
    print("=" * 50)

    # Load data
    df = load_raw_data()

    # Apply cleaning steps
    df = clean_timestamps(df)
    df = add_season_labels(df)
    df = filter_cycling_only(df)
    df = add_ride_type(df)
    df = convert_units(df)
    df = calculate_derived_metrics(df)
    df = handle_missing_values(df)
    df = select_and_order_columns(df)

    # Save cleaned data
    CLEANED_PATH.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(CLEANED_PATH, index=False)
    print(f"\nSaved cleaned data to: {CLEANED_PATH}")

    # Print summary
    print("\n" + "=" * 50)
    print("Cleaning Summary")
    print("=" * 50)
    print(f"Total cycling activities: {len(df)}")
    print(f"\nBy ride type:")
    print(df["ride_type"].value_counts().to_string())
    print(f"\nBy season:")
    print(df["season"].value_counts().to_string())
    print(f"\nData completeness:")
    print(f"  With HR data: {df['has_hr_data'].sum()} ({df['has_hr_data'].mean()*100:.1f}%)")
    print(f"  With power data: {df['has_power_data'].sum()} ({df['has_power_data'].mean()*100:.1f}%)")
    print(f"  With speed data: {df['has_speed_data'].sum()} ({df['has_speed_data'].mean()*100:.1f}%)")


if __name__ == "__main__":
    main()
