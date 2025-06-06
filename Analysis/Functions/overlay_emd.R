## ---------------------------
##
## Script name: OverlayEMD.r
##
## Purpose of script: 
##      To run calculations on perceived motion direction
##
## ---------------------------
##
## Notes:
##   The user must have loaded in EMD overlay stats, and lateral tracks
## ---------------------------

# Helper to ensure required packages are installed and loaded
require_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# Ensure required packages
require_package("dplyr")
require_package("stringr")
require_package("tidyr")
require_package("rlang")
require_package("ggplot2")
require_package("circular")

analyze_motion <- function(data, clipname) {
  required_columns <- c("Left", "Right", "Up", "Down", "Frame")
  if (!all(required_columns %in% colnames(data))) {
    warning(paste("Skipping", clipname, "- Missing required columns."))
    return(NULL)
  }
  
  data <- na.omit(data)
  if (nrow(data) == 0) {
    warning(paste("Skipping", clipname, "- No valid data after removing NAs."))
    return(NULL)
  }
  
  calculate_direction <- function(left, right, up, down) {
    x_component <- right - left
    y_component <- up - down
    angle_deg <- (atan2(y_component, x_component) * 180/pi + 360 + 90) %% 360
    # note: rotates it so 0 degrees is purely up
    magnitude <- sqrt(x_component^2 + y_component^2)
    return(c(angle_deg = angle_deg, magnitude = magnitude))
  }
  
  n_frames <- nrow(data)
  angles_degrees <- numeric(n_frames)
  magnitudes <- numeric(n_frames)
  
  for (i in 1:n_frames) {
    result <- calculate_direction(data$Left[i], data$Right[i], data$Up[i], data$Down[i])
    angles_degrees[i] <- result["angle_deg"]
    magnitudes[i] <- result["magnitude"]
  }
  
  angles_circular <- circular(angles_degrees, units = "degrees", template = "none", modulo = "2pi", rotation = "clock")
  mean_direction <- mean.circular(angles_circular, units = "degrees")
  mean_magnitude <- mean(magnitudes)
  
  frame_results <- data.frame(
    ClipName = clipname,
    Frame = data$Frame,
    motion_direction = angles_degrees,
    motion_magnitude = magnitudes
  )
  
  return(list(
    ClipName = clipname,
    frame_results = frame_results,
    angles_circular = angles_circular,
    mean_direction = mean_direction,
    mean_magnitude = mean_magnitude
  ))
}

# Function to plot motion for each clip
plot_motion <- function(clipname,
                        frame_data   = EMDOverlay_frames,
                        summary_data = EMDOverlay_clipsummary,
                        output_dir   = ".",
                        save_circular  = TRUE,
                        save_direction = TRUE,
                        save_magnitude = TRUE) {
  
  library(dplyr)
  library(ggplot2)
  library(circular)
  
  # Subset frame data
  frame_df <- frame_data %>%
    filter(ClipName == clipname) %>%
    mutate(Source = case_when(
      grepl("^bg_", FullClipName)   ~ "bg",
      grepl("^both_", FullClipName) ~ "both",
      TRUE                          ~ NA_character_
    ))
  
  if (nrow(frame_df) == 0) {
    warning(paste("No data found for clip", clipname))
    return(NULL)
  }
  
  # Get summary rows for bg and both
  summary_bg   <- summary_data %>% filter(FullClipName == paste0("bg_", clipname, "_L") | FullClipName == paste0("bg_", clipname, "_D"))
  summary_both <- summary_data %>% filter(FullClipName == paste0("both_", clipname, "_L") | FullClipName == paste0("both_", clipname, "_D"))
  
  # Check if summaries exist
  if (nrow(summary_bg) == 0 | nrow(summary_both) == 0) {
    warning("Missing bg or both summary rows for: ", clipname)
    return(NULL)
  }
  
  # Extract mean and real values
  bg_mean_dir <- summary_bg$mean_direction[1]
  bg_mean_mag <- summary_bg$mean_magnitude[1]
  
  both_mean_dir <- summary_both$mean_direction[1]
  both_mean_mag <- summary_both$mean_magnitude[1]
  
  real_dir <- summary_bg$real_direction[1]  # same for both
  
  # Circular plot
  angles_circ <- circular(frame_df$motion_direction,
                          units = "degrees", template = "geographics")
  
  circular_plot <- function() {
    par(mar = c(4, 1, 4, 1))  
    
    # Plot the motion directions
    plot.circular(
      circular(frame_df$motion_direction, units = "degrees", template = "geographics"),
      stack = TRUE, bins = 36,
      main = paste("Motion Directions –", clipname),
      col = alpha(ifelse(frame_df$Source == "bg", "grey70", "black"), 0.3),
      zero = pi/2, rotation = "clock"
    )
    
    # Arrows for mean directions
    arrows.circular(circular(bg_mean_dir, units = "degrees", template = "geographics"),
                    length = 0.2, col = "grey70", lwd = 2)
    
    arrows.circular(circular(both_mean_dir, units = "degrees", template = "geographics"),
                    length = 0.2, col = "black", lwd = 2)
    
    arrows.circular(circular(real_dir, units = "degrees", template = "geographics"),
                    length = 0.15, col = "blue", lwd = 2)
    
    # Caption
    caption <- paste0("Mean perceived direction values: bg (grey) ", round(bg_mean_dir, 1), "°, ",
                      "both (black) ", round(both_mean_dir, 1), "°; ",
                      "Real direction (blue) ", round(real_dir, 1), "°")
    
    mtext(caption, side = 1, line = 2, cex = 0.7)  # side=1 = bottom, line=4.5 = position, cex = size
  }
  
  if (save_circular) {
    png(file.path(output_dir, paste0(clipname, "_circular_plot.png")))
    circular_plot()
    dev.off()
  }
  
  # Add relative direction and real_dir offset
  center_dir <- median(c(bg_mean_dir, both_mean_dir))
  
  # Add wrapped relative direction
  frame_df <- frame_df %>%
    mutate(
      rel_dir = ((motion_direction - center_dir + 180) %% 360) - 180,
      real_rel = ((real_dir - center_dir + 180) %% 360) - 180,
      bg_mean_rel = ((bg_mean_dir - center_dir + 180) %% 360) - 180,
      both_mean_rel = ((both_mean_dir - center_dir + 180) %% 360) - 180
    )
  
  # Direction plot
  p_dir <- ggplot(frame_df, aes(x = Frame, y = rel_dir, color = Source)) +
    geom_line() +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = frame_df$bg_mean_rel[1], color = "grey70", linetype = "dashed") +
    geom_hline(yintercept = frame_df$both_mean_rel[1], color = "black", linetype = "dashed") +
    geom_hline(yintercept = frame_df$real_rel[1], color = "blue", linetype = "dotted") +
    scale_color_manual(values = c(bg = "grey70", both = "black")) +
    labs(
      title = paste("Relative Motion Direction –", clipname),
      subtitle = paste("Centered on ",
                       round(center_dir, 1), "°"),
      x = "Frame", y = "Direction (±180° around center)",
      caption = paste0("Mean perceived direction values: bg (grey) ", round(bg_mean_dir, 1), "°, ",
                       "both (black) ", round(both_mean_dir, 1), "°; ",
                       "Real direction (blue) ", round(real_dir, 1), "°")
    ) +
    theme_minimal() +
    theme(
      plot.caption = element_text(size = 7)) 
    
  
  if (save_direction) {
    ggsave(file.path(output_dir, paste0(clipname, "_direction_plot.png")),
           p_dir, width = 6, height = 4)
  }
  
  # Magnitude plot
  p_mag <- ggplot(frame_df, aes(x = Frame, y = motion_magnitude, color = Source)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = both_mean_mag, color = "black", linetype = "dashed") +
    geom_hline(yintercept = bg_mean_mag, color = "grey70", linetype = "dashed") +
    scale_color_manual(values = c(bg = "grey70", both = "black")) +
    labs(
      title = paste("Motion Magnitude –", clipname),
      x = "Frame", y = "Magnitude",
      caption = paste0("Mean magnitude values: bg (grey) ", round(bg_mean_mag, 5), "°, ",
                       "both (black) ", round(both_mean_mag, 5),"°")
    ) +
    theme_minimal() +
    theme(
      plot.caption = element_text(size = 7)) 
  
  if (save_magnitude) {
    ggsave(file.path(output_dir, paste0(clipname, "_magnitude_plot.png")),
           p_mag, width = 6, height = 4)
  }
  
  return(list(
    circular_plot  = circular_plot,
    direction_plot = p_dir,
    magnitude_plot = p_mag
  ))
}


# Function to combine the plots for the clip into a combined figure
combine_motion_plots_by_clipname <- function(clipname, input_dir = "Plots", output_dir = "CombinedPlots") {
  library(magick)
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Define file paths
  circular_file  <- file.path(input_dir, paste0(clipname, "_circular_plot.png"))
  direction_file <- file.path(input_dir, paste0(clipname, "_direction_plot.png"))
  magnitude_file <- file.path(input_dir, paste0(clipname, "_magnitude_plot.png"))
  
  # Check all files exist
  if (!all(file.exists(c(circular_file, direction_file, magnitude_file)))) {
    warning("Missing one or more plot files for: ", clipname)
    return(NULL)
  }
  
  # Read and optionally scale all images to same height
  circ_plot <- image_read(circular_file)  |> image_scale("500x")
  dir_plot  <- image_read(direction_file) |> image_scale("800x")
  mag_plot  <- image_read(magnitude_file) |> image_scale("800x")

  # Combine in one row: circular | direction | magnitude
  combined <- image_append(c(circ_plot, dir_plot, mag_plot), stack = FALSE)
  
  # Save
  output_file <- file.path(output_dir, paste0(clipname, "_combined_plot.png"))
  image_write(combined, path = output_file)
  
  message("✅ Combined plot saved to: ", output_file)
}

# Function to run rayleigh test
run_rayleigh <- function(data, label) {
  result <- rayleigh.test(circular(data, units = "degrees"))
  tibble(
    Test = "Rayleigh",
    Direction = label,
    p_value = result$p.value,
    mean_angle = mean(circular(data, units = "degrees")),
    n = length(data)
  )
}