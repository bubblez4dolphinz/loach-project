"""
.........................................................................................

Interactive Clip Extraction with ffmpeg: 
Once clip start and end frame, and the delay between Dorsal and Lateral footage have been 
defined in a csv, the Clips can be extracted as folders of frames either from the videos 
using ffmpeg or the specific frames can simply copied from pre-extracted full-clip folders
of frames. The script asks the user which view they are working from (defines input paths
and frame numbers), which method (as above, from video or folders of frames), and lastly
photo filetype (SAM2 requires jpeg). 

Note: This code is written in python to be run in WSL, filepaths can be altered to be 
windows compatible for use in CommandLine and code can be adapted to be bash or shell scripts.

Author: Flora Timney
Date: May 2025

.......................................................................................
"""

import pandas as pd
import os
import shutil
import subprocess
import re

# Load the CSV data
csv_path = '/mnt/c/Flora-Loaches/clips.csv'
data = pd.read_csv(csv_path)

# Ask user for settings
view = input("Select view (Dorsal/Lateral/Both): ").strip().capitalize()
while view not in ["Dorsal", "Lateral", "Both"]:
    view = input("Invalid choice. Select view (Dorsal/Lateral/Both): ").strip().capitalize()

method = input("Select extraction method (frames/videos): ").strip().lower()
while method not in ["frames", "videos"]:
    method = input("Invalid choice. Select extraction method (frames/videos): ").strip().lower()

image_format = "jpg"  # Default
if method == "videos":
    image_format = input("Select image format (jpg/png): ").strip().lower()
    while image_format not in ["jpg", "png"]:
        image_format = input("Invalid choice. Select image format (jpg/png): ").strip().lower()

def extract_clips(view):
    """Extracts frames or videos for a given view (Lateral or Dorsal)."""
    input_dir = f'/mnt/c/Flora-Loaches/sorted_videos/{view}'
    output_dir = f'/mnt/c/Flora-Loaches/Clips/{view}_clips/Frames'
    os.makedirs(output_dir, exist_ok=True)

    vid_col = "D_vidname" if view == "Dorsal" else "L_vidname"
    start_col = "D_start_frame" if view == "Dorsal" else "L_start_frame"
    end_col = "D_end_frame" if view == "Dorsal" else "L_end_frame"

    for _, row in data.iterrows():
        clip_name = f"{row['clip_name']}_{view[0]}"
        vid_name = row[vid_col]
        start_frame = int(row[start_col])
        end_frame = int(row[end_col])

        if method == "frames":
            # Find matching folder
            matching_folders = [f for f in os.listdir(input_dir) if vid_name in f]
            if not matching_folders:
                print(f"No folder found for {vid_name}")
                continue
            
            vid_folder = os.path.join(input_dir, matching_folders[0])
            clip_folder = os.path.join(output_dir, clip_name)
            os.makedirs(clip_folder, exist_ok=True)

            # Copy frames from existing folder
            for frame_num in range(start_frame, end_frame + 1):
                frame_filename = f"{frame_num}.jpg"  # Assuming source frames are JPG
                src_path = os.path.join(vid_folder, frame_filename)
                dst_path = os.path.join(clip_folder, frame_filename)
                if os.path.exists(src_path):
                    shutil.copy(src_path, dst_path)
                else:
                    print(f"Frame {frame_filename} not found in {vid_folder}")
        else:
            # Find matching video file
            matching_videos = [f for f in os.listdir(input_dir) if vid_name in f and re.search(r'\.mp4$', f, re.IGNORECASE)]
            if not matching_videos:
                print(f"No video found for {vid_name}")
                continue

            video_path = os.path.join(input_dir, matching_videos[0])
            clip_folder = os.path.join(output_dir, clip_name)
            os.makedirs(clip_folder, exist_ok=True)

            # Adjusted JPEG quality
            ffmpeg_command = [
                'ffmpeg', '-i', video_path,
                '-vf', f"select='between(n\\,{start_frame}\\,{end_frame})'",
                '-vsync', 'vfr',
                '-q:v', '1',  # Highest quality JPEG
                os.path.join(clip_folder, f'%05d.{image_format}')
            ]

            try:
                subprocess.run(ffmpeg_command, check=True)
                print(f"Extracted {clip_name} from {vid_name} into {clip_folder}")
            except subprocess.CalledProcessError as e:
                print(f"Failed to extract frames for {clip_name}: {e}")

# Run extraction for selected view(s)
if view == "Both":
    for v in ["Lateral", "Dorsal"]:
        print(f"\nProcessing {v} view...\n")
        extract_clips(v)
else:
    extract_clips(view)

print("Frame extraction completed.")
