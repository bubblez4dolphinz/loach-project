In this repo, I have uploaded the code associated with my MBiol project investigating Aposematism Under Motion. Raw photo and video data, as well as specialist/custom observer cone-catch models, and extensions of the tracking pipeline can be shared at the discretion of myself and my supervisors (Dr Robert Heathcote and Henry Cerbone).

The .ijm files are macro scripts for ImageJ allowing the user to batch pre-process and process images through the EMD and QCPA plugins. They need to be saved into the 'macros' folder in ImageJ on the user's computer. Replace the predator perceptual values and models with your own. 

The .ipynb files are jupyter notebooks to do the tracking for short clips, and to apply treatments to masks including the mean luminance used in this projected and the piloted stripey masks. 

The .py files are python scripts written to run in WSL to extract clips from videos (or folders of frames) using ffmpeg, to combine result types .csv files for different images adding a name column, and to sample 'x' many equally spaced frames from the full EMD output folder of frames.

The Analysis folder contains the R project I used for all my analysis and saving results and plots as figures. I established and maintained a renv() and print package lists to report which packages and their versions were used. 