## Rename and move files python script:

import os
import shutil

# Define the base path (WSL format)
base_dir = "/mnt/c/Flora-Loaches/EMD/NoOverlay/Sampled-3/Frames"

# Walk through subdirectories
for root, dirs, files in os.walk(base_dir):
    for dir_name in dirs:
        if dir_name.endswith("_frames"):
            dir_path = os.path.join(root, dir_name)
            clip_name = dir_name.replace("_frames", "")

            # Move all .png files to base_dir with renamed filenames
            for file in os.listdir(dir_path):
                if file.endswith(".png"):
                    new_filename = f"{clip_name}-{file}"
                    src_path = os.path.join(dir_path, file)
                    dst_path = os.path.join(base_dir, new_filename)
                    shutil.move(src_path, dst_path)
                    print(f"Moved: {src_path} -> {dst_path}")

            # Remove the empty directory
            shutil.rmtree(dir_path)
            print(f"Deleted folder: {dir_path}")
