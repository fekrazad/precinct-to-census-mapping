import pandas as pd
import os
import requests

# Paths
input_csv = "validation-data/voter-addresses-corrected.csv"
output_dir = "validation-data/geocoded"
os.makedirs(output_dir, exist_ok=True)

# Census Geocoder settings for 2020 addresses
benchmark = "Public_AR_Census2020"
vintage = "Census2020_Census2020"
api_url = "https://geocoding.geo.census.gov/geocoder/locations/addressbatch"

# Process in chunks of 10,000
chunksize = 10000

reader = pd.read_csv(input_csv, dtype=str, chunksize=chunksize)

for batch_num, chunk in enumerate(reader, start=1):
    output_path = os.path.join(output_dir, f"batch_{batch_num}.csv")
    if os.path.exists(output_path):
        print(f"Skipping batch {batch_num}, already processed.")
        continue

    # Prepare batch input file (pipe-delimited, no header)
    batch_file = os.path.join(output_dir, f"batch_{batch_num}_input.txt")
    chunk[["ncid", "addr", "res_city_desc", "state_cd", "zip_code"]].to_csv(
        batch_file, sep=",", index=False, header=False
    )

    # Send to Census Geographies batch geocoder
    params = {
        "benchmark": benchmark,
        "vintage": vintage,
        "format": "csv"
    }
    with open(batch_file, "rb") as f:
        files = {"addressFile": f}
        response = requests.post(api_url, params=params, files=files)
        response.raise_for_status()

    # Save response
    with open(output_path, "w", newline="") as out_f:
        out_f.write(response.text)

    print(f"Processed batch {batch_num}")
