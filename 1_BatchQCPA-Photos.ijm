/*
.................................................................................................................

RUN QCPA 1: 
Processing a folder full of calibrated images using QCPA plugin of ImageJ

1. Load in Multispectral Image
2. Load in ROIs for Scalebar and FishOutline
3. Convert to Cone Catch - pike cichlid model, includes adding luminance channel
4. Run QCPA Framework with no acuity correction
5. Save Outputs

Note: In addition to MicaToolBox with QCPA, this script requires the custom Cone-catch model for a pike cichlid (ref: Dr Robert Heathcote)
Sony_A7_28_to_70mm_D65_to_Pike_Cichlid_Crenicichla_frenata_D65 (.class & .java)

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
        baseName = replace(fileList[i], ".mspec", "");
        
        // Define output subdirectory
        subDir = outputDir + baseName + "/";
        
        // Skip if subdirectory exists and is not empty
        if (File.exists(subDir)) {
            subList = getFileList(subDir);
            if (subList.length > 0) {
                print("Skipping " + baseName + ": already processed.");
                continue;
            }
        }

        // Load corresponding ROI set
        roiZip = inputDir + baseName + "-RoiSet.zip";

        // Create output subdirectory
        File.makeDirectory(subDir);

        // --- Load mspec and ROIs ---
        run("Load Multispectral Image", "select=[" + mspecFile + "] image=[Linear Normalised Reflectance Stack]");
        roiManager("reset");
        if (File.exists(roiZip)) {
            roiManager("Open", roiZip);
        } else {
            print("ROI zip not found for " + baseName + ". Skipping.");
            close("*");
            continue;
        }

        // --- Run Cone Catch and QCPA Framework ---
        run("Convert to Cone Catch", "model=[Sony A7 28 to 70mm D65 to Pike Cichlid Crenicichla frenata D65] desaturate desaturation=0.010 remove replace=0.001");
        run("Run QCPA Framework", "acuity=Gaussian rnl_ranked_filter clustering=[RNL Cluster] visual=[Human 0.05] luminance=0.100 particle local acuity=[Cycles per degree] acuity_0=8.080 method=[Viewing distance] distance=100 rescale=5 roi=FishOutline");

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
                saveAs("Results", subDir + baseName + "_" + tableNames[j] + ".csv");
                run("Close");
            }
        }

        // --- Cleanup ---
        close("*");
        roiManager("reset");
    }
}

setBatchMode(false);
