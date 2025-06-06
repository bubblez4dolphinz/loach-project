## ---------------------------
##
## Script name: modelling.r
##
## Purpose of script: 
##      To check model assumptions and save output figures
##
## ---------------------------
##
## Notes:
##   The user defines the list of models
## ---------------------------


require_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# Ensure required packages
require_package("DHARMa")
require_package("performance")
require_package("ggplot2")


glmmTMB_to_gt <- function(model, model_name) {
  tidy(model, conf.int = FALSE) %>%
    filter(effect == "fixed") %>%
    select(term, estimate, std.error, statistic, p.value) %>%
    mutate(
      p_stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE            ~ ""
      )
    ) %>%
    gt(rowname_col = "term") %>%
    tab_header(title = model_name) %>%
    fmt_number(
      columns  = c("estimate", "std.error", "statistic", "p.value"),
      decimals = 3
    ) %>%
    cols_label(p_stars = "")
}


drop1_to_gt <- function(model, model_name, out_dir = NULL, file_name = NULL) {
  # Run drop1 with Chi-squared test
  drop_res <- drop1(model, test = "Chi") %>% as.data.frame()
  
  # Add term names (rownames to column)
  drop_df <- drop_res %>%
    tibble::rownames_to_column("term") %>%
    filter(term != "<none>") %>%  # remove "<none>" row
    mutate(
      p_stars = case_when(
        `Pr(>Chi)` < 0.001 ~ "***",
        `Pr(>Chi)` < 0.01  ~ "**",
        `Pr(>Chi)` < 0.05  ~ "*",
        TRUE               ~ ""
      )
    )
  
  # Format into a gt table
  gt_tbl <- drop_df %>%
    gt(rowname_col = "term") %>%
    tab_header(title = paste(model_name, "– drop1")) %>%
    fmt_number(
      columns = c("AIC", "LRT", "Pr(>Chi)"),
      decimals = 3
    ) %>%
    cols_label(
      Df        = "df",
      AIC       = "AIC",
      LRT       = "Chi-sq",
      `Pr(>Chi)` = "p-value",
      p_stars   = ""
    )
  
  # Save if requested
  if (!is.null(out_dir) && !is.null(file_name)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(out_dir, file_name)
    gtsave(gt_tbl, out_path)
  }
  
  return(gt_tbl)
}

combined_model_summary_gt <- function(model, model_name, out_dir = NULL, file_name = NULL) {
  # 1. Fixed effects from tidy()
  fixed_df <- tidy(model, conf.int = FALSE) %>%
    filter(effect == "fixed") %>%
    select(term, estimate, std.error, statistic, p.value) %>%
    mutate(
      p_stars_coef = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE            ~ ""
      )
    )
  
  gt_fixed <- fixed_df %>%
    gt(rowname_col = "term") %>%
    tab_header(title = paste(model_name, "- Fixed Effects Summary")) %>%
    fmt_number(
      columns = c("estimate", "std.error", "statistic", "p.value"),
      decimals = 3
    ) %>%
    cols_label(
      estimate     = "Estimate",
      std.error    = "SE",
      statistic    = "z",
      p.value      = "p (coef)",
      p_stars_coef = ""
    )
  
  # 2. Term-deletion results from drop1()
  drop_df <- drop1(model, test = "Chi") %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    filter(term != "<none>") %>%
    mutate(
      p_stars_drop = case_when(
        `Pr(>Chi)` < 0.001 ~ "***",
        `Pr(>Chi)` < 0.01  ~ "**",
        `Pr(>Chi)` < 0.05  ~ "*",
        TRUE               ~ ""
      )
    )
  
  gt_drop1 <- drop_df %>%
    gt(rowname_col = "term") %>%
    tab_header(title = paste(model_name, "- Drop1 Summary")) %>%
    fmt_number(
      columns = c("AIC", "LRT", "Pr(>Chi)"),
      decimals = 3
    ) %>%
    cols_label(
      Df           = "df",
      AIC          = "AIC",
      LRT          = "Chi-sq",
      `Pr(>Chi)`   = "p (drop1)",
      p_stars_drop = ""
    )
  
  if (!is.null(out_dir) && !is.null(file_name)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Temporary files for individual tables
    fixed_file <- tempfile(fileext = ".png")
    drop1_file <- tempfile(fileext = ".png")
    
    gtsave(gt_fixed, fixed_file)
    gtsave(gt_drop1, drop1_file)
    
    # Read images with magick
    img_fixed <- image_read(fixed_file)
    img_drop1 <- image_read(drop1_file)
    
    # Resize images to the same height for neatness (optional)
    height <- max(image_info(img_fixed)$height, image_info(img_drop1)$height)
    img_fixed <- image_scale(img_fixed, paste0("x", height))
    img_drop1 <- image_scale(img_drop1, paste0("x", height))
    
    # Combine side-by-side
    combined <- image_append(c(img_fixed, img_drop1))
    
    # Save combined image
    image_write(combined, path = file.path(out_dir, file_name), format = "png")
    
    # Clean up temporary files
    file.remove(fixed_file, drop1_file)
  }
}


# helper to get a nice evenly‐spaced sequence
nseq <- function(x, len = length(x)) {
  seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length = len)
}

make_contvar_plot <- function(model, model_name, data,
                              out_dir = "Figures/Plots/Kinematics",
                              n_points = nrow(data),
                              width = 5, height = 5) {
  
  # 1. Extract response var name (not used for labeling now)
  response <- as.character(formula(model)[[2]])
  
  # 2. Build prediction grid: one curve per Species, fixed Treatment = "C"
  grid <- expand.grid(
    mean_dist2pred = nseq(data$mean_dist2pred, len = n_points),
    Species        = unique(data$Species),
    Treatment      = "C",
    TrialID        = NA_character_,
    stringsAsFactors = FALSE
  )
  
  # 3. Predict with se.fit
  pr <- predict(model, newdata = grid, se.fit = TRUE, type = "response")
  grid <- bind_cols(grid,
                    fit = pr$fit,
                    se  = pr$se.fit) %>%
    mutate(
      Upper = fit + 1.96 * se,
      Lower = fit - 1.96 * se
    )
  
  # 4. Build the ggplot
  p <- ggplot(data, aes(x = mean_dist2pred, y = !!sym(response), colour = Species)) +
    geom_point() +
    geom_ribbon(data = grid,
                aes(x = mean_dist2pred, ymin = Lower, ymax = Upper, fill = Species),
                alpha = 0.2, inherit.aes = FALSE) +
    geom_line(data = grid,
              aes(x = mean_dist2pred, y = fit, colour = Species),
              inherit.aes = FALSE) +
    scale_fill_manual(values = species_colours, labels = species_labels) +
    scale_colour_manual(values = species_colours, labels = species_labels) +
    theme_minimal() +
    theme(
      legend.position      = c(0.95, 0.95),
      legend.justification = c("right", "top"),
      legend.background    = element_rect(fill = alpha("white", 0.7), color = NA),
      legend.key           = element_rect(fill = "transparent", color = NA),
      legend.text          = element_text(face = "italic")
    ) +
    labs(
      title = paste("(A) Continuous Variable Plot -", model_name),
      y     = model_name,
      x     = "Mean Distance to Predator"
    )
  
  # 5. Ensure output dir exists
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 6. Save plot
  out_file1 <- file.path(out_dir, paste0(model_name, "_ContVar_plot.svg"))
  ggsave(out_file1, plot = p, width = width, height = height)
  out_file2 <- file.path(out_dir, paste0(model_name, "_ContVar_plot.png"))
  ggsave(out_file2, plot = p, width = width, height = height)
  
  invisible(p)
}


make_inter_plot <- function(model, model_name, data,
                            out_dir = "Figures/Plots/Kinematics",
                            width = 5, height = 5) {
  # 1. Pull the left-hand side of the formula (response variable)
  response <- as.character(formula(model)[[2]])
  
  # 2. Build the interaction plot
  p <- ggline(
    data     = data,
    x        = "Treatment",
    y        = response,
    color    = "Species",
    palette  = species_colours,
    add      = c("mean_se", "dotplot")
  ) +
    scale_color_manual(values = species_colours, labels = species_labels) +
    scale_x_discrete(labels = c("C" = "Control", "P" = "Predator")) +
    labs(
      title = paste("(B) Interaction Plot -", model_name),
      y     = model_name
    ) +
    theme_minimal() +
    theme(
      legend.position      = c(0.95, 0.95),
      legend.justification = c("right", "top"),
      legend.background    = element_rect(fill = alpha("white", 0.7), color = NA),
      legend.key           = element_rect(fill = "transparent", color = NA),
      legend.text          = element_text(face = "italic")
    )
  
  # 3. Ensure the output directory exists
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 4. Save
  out_path1 <- file.path(out_dir, paste0(model_name, "_Inter_plot.svg"))
  ggsave(out_path1, plot = p, width = width, height = height)
  out_path2 <- file.path(out_dir, paste0(model_name, "_Inter_plot.png"))
  ggsave(out_path2, plot = p, width = width, height = height)
  
  invisible(p)
}

save_model_diagnostics <- function(model, model_name,
                                   out_dir = "Figures/ModelDiagnostics",
                                   filetype = c("svg", "png")) {
  filetype <- match.arg(filetype)
  
  # Ensure output directory exists
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --- Plot 1: check_model ---
  file1 <- file.path(out_dir, paste0(model_name, "_check_model.", filetype))
  if (filetype == "svg") {
    svg(file = file1, width = 10, height = 6)
  } else if (filetype == "png") {
    png(file = file1, width = 1200, height = 1000, res = 150)
  }
  plot(check_model(model))
  dev.off()
  
  # --- Plot 2: simulateResiduals ---
  file2 <- file.path(out_dir, paste0(model_name, "_simulateResiduals.", filetype))
  if (filetype == "svg") {
    svg(file = file2, width = 10, height = 6)
  } else if (filetype == "png") {
    png(file = file2, width = 1000, height = 600, res = 150)
  }
  plot(simulateResiduals(model))
  dev.off()
}

