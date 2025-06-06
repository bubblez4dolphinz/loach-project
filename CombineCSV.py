"""
.........................................................................................

Combine CSVs: 
Script to combine .csv files of the same result_type into a single file

This code is compatible with the file naming and structure determined in the Batch_QCPA 
macro scripts and SAM2 tracking used in this project and the QCPA framework more generally. 
The script defines the expected results table suffixes and 'spares' that don't fit the
pattern are shown to the user to decide how to proceed. 

Note: This code is written in python to be run in WSL, filepaths can be altered to be 
windows compatible for use in CommandLine and code can be adapted to be bash or shell scripts.

Author: Flora Timney
Date: May 2025

.......................................................................................
"""


import os
import pandas as pd

# --- Setup paths ---
base_dir = "/mnt/c/Flora-Loaches/QCPA/2_QCPA-results-550mm"
output_dir = os.path.join(base_dir, "QCPA-CombinedResults")
os.makedirs(output_dir, exist_ok=True)

# --- Recursively gather all .csv files ---
csv_files = []
for root, dirs, files in os.walk(base_dir):
    for file in files:
        if file.endswith(".csv"):
            csv_files.append({
                "path": os.path.join(root, file),
                "filename": file
            })

# --- Group files by pattern ---
matched = []
spares = []

known_types = ["Cluster Particle Analysis Summary Results", "Cluster Results", "Individual Particle Results", "Local Edge Intensity Analysis", "ROI Cluster Results", "Summary Results", "track"]  # Add more if needed

for f in csv_files:
    file = f["filename"]
    if "_" in file:
        parts = file.rsplit("_", 1)
        result_type = parts[1].replace(".csv", "")
        if result_type in known_types:
            matched.append({
                "path": f["path"],
                "name": parts[0],
                "result_type": result_type
            })
        else:
            spares.append(f)
    else:
        spares.append(f)

# --- Combine matched files by result_type ---
result_types = sorted(set(info["result_type"] for info in matched))

for result_type in result_types:
    subset = [f for f in matched if f["result_type"] == result_type]
    dfs = []
    for f in subset:
        df = pd.read_csv(f["path"])
        df.insert(0, "Name", f["name"])
        dfs.append(df)
    if dfs:
        combined = pd.concat(dfs, ignore_index=True)
        out_path = os.path.join(output_dir, f"comb-{result_type}.csv")
        combined.to_csv(out_path, index=False)
        print(f"✅ Saved: {out_path}")

# --- Interactive combine unmatched files ---
if spares:
    print(f"Found {len(spares)} CSV files that don't match known result types.")
    print("Example spare file:", spares[0]["filename"])
    combine_spares = input("Do you want to combine these spare files into one CSV? (y/n): ").strip().lower()
    while combine_spares not in ("y", "n"):
        combine_spares = input("Please enter 'y' or 'n': ").strip().lower()

    if combine_spares == "y":
        spare_output_name = input("Enter the output filename for the combined spare files (without .csv): ").strip()
        if not spare_output_name:
            spare_output_name = "Spare Results"

        dfs = []
        for f in spares:
            name = os.path.splitext(f["filename"])[0]
            df = pd.read_csv(f["path"])
            df.insert(0, "Name", name)
            dfs.append(df)

        combined = pd.concat(dfs, ignore_index=True)
        out_path = os.path.join(output_dir, f"{spare_output_name}.csv")
        combined.to_csv(out_path, index=False)
        print(f"✅ Saved combined spare files as: {out_path}")
    else:
        print("Skipped combining spare files.")
else:
    print("✅ No unmatched files to combine.")