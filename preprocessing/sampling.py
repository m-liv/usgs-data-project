import pandas as pd
import numpy as np

# =========================
# Configuration
# =========================
INPUT_CSV = "usgs_earthquakes.csv"
OUTPUT_CSV = "usgs_sampled2.csv"

LAT_COL = "latitude"   # change if your column names differ
LON_COL = "longitude"
MAG_COL = "mag"        # optional; only used if KEEP_LARGE_QUAKES is True

LAT_BIN = 2.0          # grid height in degrees
LON_BIN = 2.0          # grid width in degrees

MIN_PER_CELL = 3       # keep at least this many points per populated cell
MAX_PER_CELL = 50      # keep at most this many sampled points per cell
RANDOM_SEED = 42

KEEP_LARGE_QUAKES = False
LARGE_QUAKE_THRESHOLD = 5.0   # always keep quakes at or above this magnitude

# =========================
# Load data
# =========================
df = pd.read_csv(INPUT_CSV)

# Keep only rows with valid coordinates
df = df.dropna(subset=[LAT_COL, LON_COL]).copy()

# =========================
# Create grid cells
# =========================
df["lat_bin"] = np.floor(df[LAT_COL] / LAT_BIN) * LAT_BIN
df["lon_bin"] = np.floor(df[LON_COL] / LON_BIN) * LON_BIN

# =========================
# Density-aware sampling function
# =========================
def sample_cell(cell_df: pd.DataFrame) -> pd.DataFrame:
    if KEEP_LARGE_QUAKES and MAG_COL in cell_df.columns:
        big = cell_df[cell_df[MAG_COL] >= LARGE_QUAKE_THRESHOLD]
        rest = cell_df[cell_df[MAG_COL] < LARGE_QUAKE_THRESHOLD]
    else:
        big = cell_df.iloc[0:0]
        rest = cell_df

    n_rest = len(rest)

    # sqrt scaling: dense cells keep more rows, but not proportionally so
    sample_size = int(np.sqrt(n_rest))

    # clamp to configured range
    if n_rest > 0:
        sample_size = max(MIN_PER_CELL, min(sample_size, MAX_PER_CELL))
        sample_size = min(sample_size, n_rest)
        sampled_rest = rest.sample(n=sample_size, random_state=RANDOM_SEED)
    else:
        sampled_rest = rest

    # combine always-kept large quakes with sampled remainder
    sampled = pd.concat([big, sampled_rest], axis=0)

    # deduplicate in case something overlaps
    sampled = sampled.drop_duplicates()

    return sampled

# =========================
# Apply sampling by grid cell
# =========================
sampled_df = (
    df.groupby(["lat_bin", "lon_bin"], group_keys=False)
      .apply(sample_cell)
      .reset_index(drop=True)
)

# Remove helper columns
sampled_df = sampled_df.drop(columns=["lat_bin", "lon_bin"])

# =========================
# Save output
# =========================
sampled_df.to_csv(OUTPUT_CSV, index=False)

# =========================
# Summary
# =========================
print(f"Original rows: {len(df):,}")
print(f"Sampled rows:  {len(sampled_df):,}")
print(f"Saved to:      {OUTPUT_CSV}")
print(f"Reduction:     {100 * (1 - len(sampled_df) / len(df)):.2f}%")