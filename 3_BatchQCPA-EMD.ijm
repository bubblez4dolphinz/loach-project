/*
.................................................................................................................

RUN QCPA 2: 
Processing a folder full of frames from EMD output using QCPA plugin of ImageJ

1. Load in Multispectral Image
2. Load in ROIs for Scalebar and Crop-to-Rectangle
3. Crop to rectangle
4. Convert to Cone Catch - *hack* Camera-to-Camera calibration
5. Add luminance Channel - just taken from 'lw' channel
6. Run QCPA Framework with no acuity correction
7. Save Outputs

Note: In addition to MicaToolBox with QCPA, this script requires the custom Camera-to-Camera Cone-Catch Model (ref: Dr Robert Heathcote, Dr Cedric van den Berg)
Sony_A7_28_to_70mm_D65_to_Sony_A7_28_to_70mm_D65 (.class & .java)

''''''''''''''''''''''''''''''''
Author: Flora Timney
May 2025

''''''''''''''''''''''''''''''''
*/

// Prompt user to select source and output directories
inputDir = getDirectory("Select input directory with .mspec files");
outputDir = getDirectory("Select output directory for saving results");

setBatchMode(true);  // Speeds up batch processing
fileList = getFileList(inputDir);

for (i = 0; i < fileList.length; i++) {
    if (endsWith(fileList[i], ".mspec")) {
        mspecFile = inputDir + fileList[i];
        fullName = replace(fileList[i], ".mspec", "");

        if (startsWith(fullName, "bg_"))
            frameName = substring(fullName, 3); // "bg_" is 3 characters
        else if (startsWith(fullName, "both_"))
            frameName = substring(fullName, 5); // "both_" is 5 characters
        else
            frameName = fullName;

        lastDash = lastIndexOf(fullName, "-");
        if (lastDash > -1)
            clipName = substring(fullName, 0, lastDash);
        else
            clipName = fullName;

        subDir = outputDir + fullName + "/"; // was "clipName before"

        // Skip if subdirectory exists and is not empty
        if (File.exists(subDir)) {
            subList = getFileList(subDir);
            if (subList.length > 0) {
                print("Skipping " + fullName + ": already processed.");
                continue;
            }
        }

        // Load corresponding ROI set
        roiZip = inputDir + frameName + "-RoiSet.zip";

        // Create output subdirectory
        File.makeDirectory(subDir);

        // --- Load mspec and ROIs ---
        run("Load Multispectral Image", "select=[" + mspecFile + "] image=[Linear Normalised Reflectance Stack]");
        roiManager("reset");
        if (File.exists(roiZip)) {
            roiManager("Open", roiZip);
        } else {
            print("ROI zip not found for " + frameName + ". Skipping.");
            close("*");
            continue;
        }

        // --- Crop to Rectangle to speed up processing ---
        roiManager("Select", 1);
        run("Crop");

        // --- Run Cone Catch and QCPA Framework ---
        run("Convert to Cone Catch", "model=[Sony A7 28 to 70mm D65 to Sony A7 28 to 70mm D65] desaturate desaturation=0.010 remove replace=0.001");
        run("Create Luminance Channel", "lw");
        run("Run QCPA Framework", "acuity=None rnl_ranked_filter clustering=[RNL Cluster] visual=[Human 0.05] luminance=0.100 particle local iterations=5 radius=5 falloff=3");

        // --- Save all open image windows as .tif and .png ---
        imgCount = nImages;
        for (j = 1; j <= imgCount; j++) {
            selectImage(j);
            imgTitle = getTitle();
            saveAs("Tiff", subDir + imgTitle + ".tif");
            saveAs("PNG", subDir + imgTitle + ".png");
        }

        // --- Save result tables if open ---
        tableNames = newArray(
            "Individual Particle Results",
            "Cluster Particle Analysis Summary Results",
            "Summary Results",
            "ROI Cluster Results",
            "Cluster Results",
            "Local Edge Intensity Analysis"
        );
        for (j = 0; j < tableNames.length; j++) {
            if (isOpen(tableNames[j])) {
                selectWindow(tableNames[j]);
                saveAs("Results", subDir + fullName + "_" + tableNames[j] + ".csv");
                run("Close");
            }
        }

        // --- Cleanup ---
        close("*");
        roiManager("reset");
    }
}

setBatchMode(false);
