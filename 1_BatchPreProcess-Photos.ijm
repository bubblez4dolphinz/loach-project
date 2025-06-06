/*
.................................................................................................................

PRE-PROCESS 2: 
Preparing a folder full of calibrated for use in QCPA plugin of ImageJ
1. Generate Multispectral Image
2. Create and Save ROIs for Scalebar and FishOutline

Note: This script requires the MicaToolBox with QCPA.

''''''''''''''''''''''''''''''''
Author: Flora Timney
May 2025

''''''''''''''''''''''''''''''''
*/


while (true) {
    // Run Generate Multispectral Image and let user select input manually
    run("Generate Multispectral Image", 
        "camera=Visible grey=Same standard=7 alignment=None image_0=[Linear Normalised Reflectance Stack]");

    // Wait for image to open
    wait(500); // Give ImageJ a moment to catch up
    imgTitle = getTitle(); // Get the title of the generated image
    imgDir = getDirectory("image"); // Get the directory of the image

    // Clean up imgTitle to avoid issues with invalid file name characters
    imgTitle = replace(imgTitle, " ", "_");  // Replace spaces with underscores (optional)
    imgTitle = replace(imgTitle, ":", "_");  // Replace colons (useful for Windows paths)

    // Make sure the directory path ends with a separator (for cross-platform compatibility)
    if (!endsWith(imgDir, File.separator)) {
        imgDir = imgDir + File.separator;
    }

    // Prompt user to draw scale bar
    setTool("line");
    waitForUser("Draw Scale Bar", "Draw a scale bar using the line tool and add it to the ROI Manager.");

    // Prompt user to draw fish outline
    setTool("polygon");
    waitForUser("Draw Fish Outline", "Draw a polygon around the fish and add it to the ROI Manager.");

    // Rename second ROI
    roiManager("Show All");
    roiManager("Deselect");
    n = roiManager("count");

    if (n == 2) {
        roiManager("Select", 1); // Select second ROI (the fish outline)
        roiManager("Rename", "FishOutline"); // Rename it
        roiManager("Select", newArray(0, 1)); // Select both ROIs
        print("ROI set ready for saving. Please save it manually.");
    } else {
        showMessage("Error", "Expected 2 ROIs, but found " + n + ". Skipping save.");
    }

    // Wait for user to close the image before next loop
    waitForUser("Close Image", "Close the current image window, then click OK to continue.");
    roiManager("Reset"); // Reset ROI Manager for the next round
}


