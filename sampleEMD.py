import os
import shutil
from pathlib import Path

# Define the input and output base directories
input_base = Path("/mnt/c/Flora-Loaches/EMD/NoOverlay/Full")
output_base = Path("/mnt/c/Flora-Loaches/EMD/NoOverlay/Sampled")

# Make sure the output base directory exists
output_base.mkdir(parents=True, exist_ok=True)

# # Sample 5 frames
# Loop through subdirectories
# for subdir in input_base.iterdir():
#     if subdir.is_dir():
#         png_files = sorted(subdir.glob("*.png"))
#         total = len(png_files)
#         if total < 1:
#             continue  # Skip empty folders

#         # Percentile indexes: 0%, 25%, 50%, 75%, 100%
#         indices = [0,
#                    total * 25 // 100,
#                    total * 50 // 100,
#                    total * 75 // 100,
#                    total - 1]

#         # Use set() to avoid duplicate indices if total is small
#         selected_files = [png_files[i] for i in sorted(set(indices))]

#         # Create the corresponding output subdirectory
#         out_dir = output_base / subdir.name
#         out_dir.mkdir(parents=True, exist_ok=True)

#         # Copy the selected files
#         for f in selected_files:
#             shutil.copy2(f, out_dir / f.name)

#         print(f"Sampled {len(selected_files)} frames from {subdir.name} → {out_dir.name}")

        
# # Sample 3 frames
for subdir in input_base.iterdir():
    if subdir.is_dir():
        # Get all PNG files in this subdirectory
        png_files = sorted(subdir.glob("*.png"))
        if len(png_files) < 1:
            continue  # Skip if no PNG files

        # Determine first, middle, last
        first = png_files[0]
        last = png_files[-1]
        middle = png_files[len(png_files) // 2]

        selected_files = [first, middle, last]

        # Create the output directory
        out_dir = output_base / subdir.name
        out_dir.mkdir(parents=True, exist_ok=True)

        # Copy selected files
        for f in selected_files:
            shutil.copy2(f, out_dir / f.name)

        print(f"Sampled {subdir.name} → {out_dir.name}")
