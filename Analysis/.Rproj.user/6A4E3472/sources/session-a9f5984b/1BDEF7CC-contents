## ---------------------------
##
## Script name: calculations.r
##
## Purpose of script: 
##      To make calculations in dataframes neater in scripts
##
## ---------------------------
##
## Notes:
##   The user must have loaded in trajectories, meta_data and predator positions
## ---------------------------

characteriseTrajectory <- function(trj, pred_pos, traj_name = NULL, save_dir = NULL) {
  derivs <- TrajDerivatives(trj)
  
  # Speed and acceleration
  speed <- derivs$speed
  acc <- derivs$acceleration
  
  # Distance to predator
  coords <- trj[, c("x", "y")]
  dist2pred <- sqrt((coords$x - pred_pos[1])^2 + (coords$y - pred_pos[2])^2)
  
  # Make all vectors the same length
  n <- min(length(speed), length(acc), nrow(coords))
  
  # Trim to common length
  speed <- speed[1:n]
  acc <- acc[1:n]
  coords <- coords[1:n, ]
  dist2pred <- dist2pred[1:n]
  
  # Summary stats
  median_speed <- median(speed, na.rm = TRUE)
  mean_speed <- mean(speed, na.rm = TRUE)
  sd_speed <- sd(speed, na.rm = TRUE)
  cv_speed <- sd_speed / mean_speed
  max_acc <- max(abs(acc), na.rm = TRUE)
  mean_dist2pred <- mean(dist2pred, na.rm = TRUE)
  
  # Frame-level tibble
  raw_data <- tibble::tibble(
    frame = seq_len(n),
    x = coords$x,
    y = coords$y,
    speed = speed,
    acceleration = acc,
    dist_to_pred = dist2pred
  )
  
  # Optional saving
  if (!is.null(save_dir) && !is.null(traj_name)) {
    dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(raw_data, file = file.path(save_dir, paste0(traj_name, "_kinematics.csv")))
  }
  
  # Return both summary and frame-by-frame data
  list(
    summary = tibble(
      median_speed = median_speed,
      cv_speed = cv_speed,
      max_acc = max_acc,
      mean_dist2pred = mean_dist2pred
    ),
    frame_data = raw_data
  )
}

calc_PC1 <- function(df, formula_file, pc1_colname = "PC1") {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    install.packages("stringr")
  }
  library(stringr)
  
  # Load formula
  pca_formula_full <- readRDS(formula_file)
  
  # Extract formula RHS text (assume single-line)
  formula_text <- deparse(pca_formula_full)[2]
  
  # Extract coefficients and variable names from RHS (e.g. "3.0722 * `CAA:Sc`")
  matches <- str_match_all(formula_text, "([0-9\\.]+) \\* `([^`]+)`")[[1]]
  
  coefs <- as.numeric(matches[, 2])
  vars <- matches[, 3]
  
  # For each variable in formula, check if "med_" + var exists in df columns,
  # if yes, use that instead
  vars_to_use <- sapply(vars, function(v) {
    med_var <- paste0("med_", v)
    if (med_var %in% colnames(df)) {
      med_var
    } else {
      v
    }
  }, USE.NAMES = FALSE)
  
  # Add zero columns for any variables (or med_ vars) missing from df
  missing_vars <- setdiff(vars_to_use, colnames(df))
  if(length(missing_vars) > 0) {
    df[missing_vars] <- 0
  }
  
  # Subset df to columns in coef_vec order
  df_sub <- df[, vars_to_use, drop = FALSE]
  
  # Calculate PC1 scores (matrix multiplication)
  pc1_scores <- as.numeric(as.matrix(df_sub) %*% coefs)
  
  # Add PC1 scores column to df
  df[[pc1_colname]] <- pc1_scores
  
  return(df)
}

variance_partition <- function(var, group_vars = c("Species", "test"), data) {
  library(dplyr)
  
  df <- data %>%
    mutate(Group = interaction(!!!syms(group_vars), sep = "-")) %>%  # dynamic interaction
    select(Group, all_of(var)) %>%
    rename(Response = all_of(var)) %>%
    filter(!is.na(Response))
  
  grand_mean <- mean(df$Response, na.rm = TRUE)
  
  group_stats <- df %>%
    group_by(Group) %>%
    summarise(
      n = sum(!is.na(Response)),
      group_mean = mean(Response, na.rm = TRUE),
      group_var = ifelse(n > 1, var(Response, na.rm = TRUE), 0)
    )
  
  between_var <- sum(group_stats$n * (group_stats$group_mean - grand_mean)^2) / (sum(group_stats$n) - 1)
  
  within_var <- sum((group_stats$n - 1) * group_stats$group_var) / (sum(group_stats$n) - nrow(group_stats))
  
  total_var <- var(df$Response, na.rm = TRUE)
  
  data.frame(
    Response = var,
    Between_Group_Var = between_var,
    Within_Group_Var = within_var,
    Total_Var = total_var,
    Prop_Between = between_var / total_var,
    Prop_Within = within_var / total_var
  )
}


## Trying to make functions to run anova, emeans, partition of variance and make plots
## aplication: run_anova_extended(data = compare_distance, response_var = "caa_asp", group_vars = c("species", "test"), label = "distance test")
# Function 1: separate tables for anova, emeans and parition of variance results, plot doesn't work
run_anova_extended1 <- function(data, response_var, group_vars = c("Species", "test"), label = NULL) {
  library(broom)
  library(dplyr)
  library(ggplot2)
  library(effectsize)
  library(emmeans)
  
  # species_colours should be defined globally
  if (!exists("species_colours")) stop("Please define species_colours before running the function.")
  
  # Build formula string with backticks around response_var to handle special chars
  formula_str <- paste0("`", response_var, "` ~ ", paste(group_vars, collapse = " * "))
  response_sym <- rlang::sym(response_var)
  formula <- as.formula(formula_str)
  
  # Run ANOVA
  fit <- aov(formula, data = data)
  tidy_anova <- broom::tidy(fit)
  
  # Add eta squared effect sizes (overall, no CI)
  eta2_tbl <- effectsize::eta_squared(fit, partial = FALSE) %>%
    as.data.frame() %>%
    mutate(
      eta2 = round(Eta2, 3),
      eta2_effect = case_when(
        eta2 < 0.01 ~ "negligible",
        eta2 < 0.06 ~ "small",
        eta2 < 0.14 ~ "medium",
        TRUE        ~ "large"
      )
    ) %>%
    rename(term = Parameter) %>%
    select(term, eta2, eta2_effect)
  
  # Join eta2 to tidy anova results, add significance stars (without renaming p.value)
  tidy_anova <- tidy_anova %>%
    left_join(eta2_tbl, by = "term") %>%
    mutate(
      Response = response_var,
      Dataset = label,
      p_stars = case_when(
        is.na(p.value) ~ "",
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE            ~ ""
      )
    ) %>%
    rename(
      Term = term,
      Sum_Sq = sumsq,
      Mean_Sq = meansq,
      DF = df,
      P_value = p.value,
      signif = p_stars,
      Eta2 = eta2,
      Effect_size = eta2_effect
    ) %>%
    select(Dataset, Response, Term, Sum_Sq, Mean_Sq, DF, P_value, signif, Eta2, Effect_size)
  
  # Extract interaction p-value & star
  interaction_term <- paste(group_vars, collapse = ":")
  interaction_p <- tidy_anova %>% filter(Term == interaction_term) %>% pull(P_value)
  interaction_star <- tidy_anova %>% filter(Term == interaction_term) %>% pull(signif)
  if (length(interaction_star) == 0) interaction_star <- ""
  
  # Compute variance partition for group_vars
  var_part <- variance_partition(response_var, group_vars = group_vars, data = data) %>%
    # Separate Group into individual factors for plotting
    mutate(
      Species = sub("-.*", "", Group),
      test = sub(".*-", "", Group)
    )
  
  # For estimated marginal means for plotting
  emm <- emmeans(fit, specs = group_vars)
  emm_df <- as.data.frame(emm)
  
  # Make sure Species and test are factors and levels match data
  emm_df$Species <- factor(emm_df[[group_vars[1]]], levels = levels(data[[group_vars[1]]]))
  emm_df$test <- factor(emm_df[[group_vars[2]]], levels = levels(data[[group_vars[2]]]))
  
  # Base plot with raw points (jitter), emmeans + error bars, variance labels
  p <- ggplot(data, aes(x = .data[[group_vars[2]]], y = !!response_sym, color = .data[[group_vars[1]]])) +
    geom_jitter(width = 0.1, alpha = 0.6, size = 2) +
    geom_point(data = emm_df, aes(x = .data[[group_vars[2]]], y = emmean, color = .data[[group_vars[1]]]),
               size = 3, position = position_dodge(0.2)) +
    geom_line(data = emm_df, aes(x = .data[[group_vars[2]]], y = emmean, group = .data[[group_vars[1]]], color = .data[[group_vars[1]]]),
              position = position_dodge(0.2)) +
    geom_errorbar(data = emm_df,
                  aes(x = .data[[group_vars[2]]], ymin = emmean - SE, ymax = emmean + SE, color = .data[[group_vars[1]]]),
                  width = 0.1, position = position_dodge(0.2)) +
    geom_text(data = var_part, 
              aes(x = test, y = max(data[[!!response_sym]], na.rm = TRUE) * 1.05,
                  label = paste0("Var: ", round(Prop_Between * 100, 1), "%"),
                  color = Species),
              size = 3, vjust = 0, show.legend = FALSE) +
    scale_color_manual(values = species_colours) +
    labs(
      title = paste0(response_var, " - Interaction Plot "),
      subtitle = paste0("Interaction p = ", signif(interaction_p, 3), interaction_star),
      x = group_vars[2],
      y = response_var,
      color = group_vars[1]
    ) +
    theme_minimal() +
    theme(legend.position = "top")
  
  # Return results as a list
  return(list(
    tidy_anova = tidy_anova,
    variation_partition = var_part,
    emeans_res = emm_df,
    interaction_plot = p
  ))
}

run_anova_extended2 <- function(data, response_var, group_vars = c("Species", "test"), label = NULL) {
  library(broom)
  library(dplyr)
  library(ggplot2)
  library(effectsize)
  library(emmeans)
  library(tidyr)    # for separate()
  library(rlang)
  
  if (!exists("species_colours")) stop("Please define species_colours before running the function.")
  
  # Build the model formula safely
  formula <- as.formula(paste0("`", response_var, "` ~ ", paste(group_vars, collapse = " * ")))
  
  # Run ANOVA
  fit <- aov(formula, data = data)
  anova_tbl <- broom::tidy(fit)
  
  # Compute overall eta-squared
  eta2_tbl <- effectsize::eta_squared(fit, partial = FALSE) %>%
    as.data.frame() %>%
    mutate(
      eta2 = round(Eta2, 3),
      eta2_effect = case_when(
        eta2 < 0.01 ~ "negligible",
        eta2 < 0.06 ~ "small",
        eta2 < 0.14 ~ "medium",
        TRUE        ~ "large"
      )
    ) %>%
    rename(term = Parameter) %>%
    select(term, eta2, eta2_effect)
  
  # Join ANOVA table with eta2 & add stars
  tidy_anova <- anova_tbl %>%
    left_join(eta2_tbl, by = "term") %>%
    mutate(
      Dataset = label,
      Response = response_var,
      signif = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE            ~ ""
      )
    ) %>%
    rename(
      Term = term,
      Sum_Sq = sumsq,
      Mean_Sq = meansq,
      DF = df,
      P_value = p.value,
      Eta2 = eta2,
      Effect_size = eta2_effect
    ) %>%
    select(Dataset, Response, Term, Sum_Sq, Mean_Sq, DF, P_value, signif, Eta2, Effect_size)
  
  # Interaction stats
  interaction_term <- paste(group_vars, collapse = ":")
  inter_row <- tidy_anova %>% filter(Term == interaction_term)
  interaction_p <- if (nrow(inter_row)) inter_row$P_value else NA
  interaction_star <- if (nrow(inter_row)) inter_row$signif else ""
  
  # Variance partition
  var_part <- variance_partition(response_var, group_vars = group_vars, data = data) %>%
    separate(Group, into = group_vars, sep = "-", remove = FALSE)
  
  # Estimated marginal means
  emm <- emmeans(fit, specs = group_vars)
  emm_df <- as.data.frame(emm)
  
  # Compute y-axis max for annotation
  max_y <- max(data[[response_var]], na.rm = TRUE) * 1.05
  
  # Build interaction plot using .data pronoun (handles weird names)
  p <- ggplot(data, aes(x = .data[[group_vars[2]]], y = .data[[response_var]], color = .data[[group_vars[1]]])) +
    geom_jitter(width = 0.1, alpha = 0.6, size = 2) +
    geom_point(data = emm_df,
               aes(x = .data[[group_vars[2]]], y = emmean, color = .data[[group_vars[1]]]),
               size = 3, position = position_dodge(0.2)) +
    geom_line(data = emm_df,
              aes(x = .data[[group_vars[2]]], y = emmean, group = .data[[group_vars[1]]], color = .data[[group_vars[1]]]),
              position = position_dodge(0.2)) +
    geom_errorbar(data = emm_df,
                  aes(x = .data[[group_vars[2]]], ymin = emmean - SE, ymax = emmean + SE, color = .data[[group_vars[1]]]),
                  width = 0.1, position = position_dodge(0.2)) +
    geom_text(data = var_part,
              aes(x = .data[[group_vars[2]]], y = max_y,
                  label = paste0("Var: ", round(Prop_Between * 100, 1), "%"),
                  color = .data[[group_vars[1]]]),
              vjust = 0, size = 3, show.legend = FALSE) +
    scale_color_manual(values = species_colours) +
    labs(
      title = paste0(response_var, " - Interaction Plot"),
      subtitle = paste0("Interaction p = ", signif(interaction_p, 3), interaction_star),
      x = group_vars[2],
      y = response_var,
      color = group_vars[1]
    ) +
    theme_minimal() +
    theme(legend.position = "top")
  
  return(list(
    tidy_anova = tidy_anova,
    variation_partition = var_part,
    emm = emm_df,
    interaction_plot = p
  ))
}
