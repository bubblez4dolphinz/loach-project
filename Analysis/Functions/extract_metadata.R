## ---------------------------
##
## Script name: extract_metadata.r
##
## Purpose of script: 
##      To extract the metadata from the names
##
## ---------------------------
##
## Notes:
##   The user defines the name structure and which columns to keep
## ---------------------------


extract_metadata <- function(df, source_col, structure, keep = NULL, sep = "_", composite_vars = NULL, insert_after = NULL) {
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
  
  # Convert column name to symbol
  source_sym <- rlang::ensym(source_col)
  
  # Temporary column for splitting
  tmp_col <- "._tmp_split_"
  
  # Separate based on separator
  df_split <- df %>%
    dplyr::mutate(!!tmp_col := !!source_sym) %>%
    tidyr::separate(!!tmp_col, into = structure, sep = sep, remove = FALSE, fill = "right")
  
  # Add any user-defined composite variables
  if (!is.null(composite_vars)) {
    for (new_col in names(composite_vars)) {
      components <- composite_vars[[new_col]]
      missing_components <- components[!components %in% names(df_split)]
      if (length(missing_components) > 0) {
        stop(paste0("Missing components for composite variable '", new_col, "': ", paste(missing_components, collapse = ", ")))
      }
      df_split[[new_col]] <- apply(df_split[, components, drop = FALSE], 1, paste, collapse = "_")
    }
  }
  
  # Select only requested columns if specified
  if (!is.null(keep)) {
    # Keep all original columns plus requested new columns
    keep_cols <- c(names(df), keep)
    df_split <- df_split %>% dplyr::select(dplyr::any_of(keep_cols))
  }
  
  # Relocate new columns after insert_after column if specified
  if (!is.null(insert_after)) {
    if (!insert_after %in% names(df_split)) {
      stop("`insert_after` column not found in dataframe.")
    }
    
    # Determine which columns were newly added
    original_names <- names(df)
    new_cols <- setdiff(names(df_split), original_names)
    
    df_split <- df_split %>%
      dplyr::relocate(dplyr::all_of(new_cols), .after = insert_after)
  }
  
  return(df_split)
}
