import math
import time
from datetime import datetime, timedelta, timezone

import pandas as pd
import requests

BASE_URL = "https://earthquake.usgs.gov/fdsnws/event/1"
COUNT_URL = f"{BASE_URL}/count"
QUERY_URL = f"{BASE_URL}/query"

OUTPUT_FILE = "usgs_earthquakes.csv"

START_YEAR = 2016
START_MONTH = 3
END_YEAR = 2026
END_MONTH = 3

PAGE_SIZE = 20000
SLEEP = 0.2


def month_start(year, month):
    return datetime(year, month, 1, tzinfo=timezone.utc)


def next_month(dt):
    if dt.month == 12:
        return datetime(dt.year + 1, 1, 1, tzinfo=timezone.utc)
    return datetime(dt.year, dt.month + 1, 1, tzinfo=timezone.utc)


def iter_months():
    current = month_start(START_YEAR, START_MONTH)
    end = month_start(END_YEAR, END_MONTH)

    while current <= end:
        nxt = next_month(current)
        yield current, nxt - timedelta(milliseconds=1)
        current = nxt


def iso(dt):
    return dt.strftime("%Y-%m-%dT%H:%M:%S.%f")[:-3] + "Z"


def get_count(session, params):
    p = params.copy()
    p["format"] = "geojson"
    r = session.get(COUNT_URL, params=p)
    r.raise_for_status()
    return r.json()["count"]


def fetch_page(session, params, offset, limit):
    p = params.copy()
    p.update({"offset": offset, "limit": limit})
    r = session.get(QUERY_URL, params=p)
    r.raise_for_status()
    return pd.read_csv(pd.io.common.StringIO(r.text))


def main():
    session = requests.Session()
    all_dfs = []

    for start, end in iter_months():
        params = {
            "format": "csv",
            "starttime": iso(start),
            "endtime": iso(end),
            "eventtype": "earthquake",
            "orderby": "time-asc",
        }

        month_label = start.strftime("%Y-%m")
        count = get_count(session, params)

        pages = math.ceil(count / PAGE_SIZE) if count > 0 else 0
        print(f"{month_label}: {count} events ({pages} pages)")

        for i in range(pages):
            offset = 1 + i * PAGE_SIZE
            limit = min(PAGE_SIZE, count - i * PAGE_SIZE)

            for attempt in range(3):
                try:
                    df = fetch_page(session, params, offset, limit)
                    break
                except Exception as e:
                    if attempt == 2:
                        raise
                    time.sleep(2 ** attempt)

            all_dfs.append(df)

            print(f"  Page {i+1}/{pages} downloaded ({len(df)} rows)")
            time.sleep(SLEEP)

    print("\nCombining all data...")
    full_df = pd.concat(all_dfs, ignore_index=True)

    print(f"Total rows before deduplication: {len(full_df)}")

    # Deduplicate by ID
    # Keep the most recently updated version if duplicates exist
    if "updated" in full_df.columns:
        full_df = full_df.sort_values("updated").drop_duplicates(subset="id", keep="last")
    else:
        full_df = full_df.drop_duplicates(subset="id", keep="last")

    print(f"Total rows after deduplication: {len(full_df)}")

    # Optional: sort chronologically
    if "time" in full_df.columns:
        full_df = full_df.sort_values("time")

    full_df.to_csv(OUTPUT_FILE, index=False)
    print(f"\nSaved to {OUTPUT_FILE}")


if __name__ == "__main__":
    main()