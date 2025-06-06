/*
.................................................................................................................

PRE-PROCESS 2: 
Preparing a folder full of frames from EMD output for use in QCPA plugin of ImageJ
1. Convert from 8-bit to RGBColour Image
2. Generate Multispectral Image
3. Create and Save ROIs for Scalebar and Crop-to-Rectangle

Note: This script requires the MicaToolBox with QCPA.

''''''''''''''''''''''''''''''''
Author: Flora Timney
May 2025

''''''''''''''''''''''''''''''''
*/

// === Set your frames folder here ===
dir = getDirectory("Select directory");
setBatchMode(true);  // Speeds up batch processing
fileList = getFileList(dir);

for (i = 0; i < fileList.length; i++) {
    filename = fileList[i];

    // Skip files that are not .png OR already have 'RGB' in the name
    if (endsWith(filename, ".png") && indexOf(filename, "RGB") == -1) {
        
        // Derive fullName and frameName
        fullName = filename.replace(".png", "");
        if (startsWith(fullName, "bg_"))
            frameName = substring(fullName, 3);  // "bg_" is 3 characters
        else if (startsWith(fullName, "both_"))
            frameName = substring(fullName, 5);  // "both_" is 5 characters
        else
            frameName = fullName;

        // Check file paths
        mspecPath = dir + fullName + ".mspec";
        roiZipPath = dir + frameName + "-RoiSet.zip";

        // Skip if .mspec already exists
        if (File.exists(mspecPath)) {
            print("Skipping " + fullName + ": .mspec already exists.");
            continue;
        }

        // Step 1: Open image and convert to RGB
        open(dir + filename);
        run("RGB Color");

        // Step 2: Save RGB version
        saveAs("PNG", dir + fullName + "RGB.png");
        close();  

        // === Log image being processed ===
        print("Processing: " + fullName);

        // Step 3: Generate Multispectral Image
        run("Generate Multispectral Image", "camera=Visible image=[Linear Image] grey=[Same photo] estimate standard=68 alignment=None image_0=[Linear Normalised Reflectance Stack] image_1=" + fullName);

        // Step 4: Check if ROI set already exists
        if (File.exists(roiZipPath)) {
            print("RoiSet already exists for " + frameName + ", skipping ROI drawing.");
        } else {
            // Prompt user to draw scale bar
            roiManager("Reset");
            setTool("line");
            waitForUser("Draw Scale Bar", "Draw a scale bar using the line tool and add it to the ROI Manager.");

            // Prompt user to draw fish rectangle
            setTool("roundrect");
            waitForUser("Draw rectangle to crop to", "Draw a rectangle around the fish and add it to the ROI Manager.");

            // Check if 2 ROIs were added
            nRois = roiManager("count");
            if (nRois != 2) {
                waitForUser("Missing ROIs", "You must have exactly 2 ROIs: scale bar and crop rectangle.\nPlease add them now, then click OK to continue.");
            }

            // Rename the crop ROI (assumes it's the last one added)
            roiManager("Deselect");       	
            roiManager("Select", 1);
            roiManager("Rename", "CropRect");

            // Step 5: Save ROI set
            roiManager("Save", roiZipPath);
        }

        // Step 6: Close image
        roiManager("Reset");
        close("*");        
    }
}
