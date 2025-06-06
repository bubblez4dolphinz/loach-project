## ---------------------------
##
## Script name: clean_dfs.r
##
## Purpose of script: 
##      To clean the different types of dataframe that I have
##
## ---------------------------
##
## Notes:
##   The user defines the list and type of dataframes
## ---------------------------

clean_qcpa_df <- function(df_list) {
  for (df_name in names(df_list)) {
    df <- df_list[[df_name]]
    
    # Remove rows where ...2 == 2, only if ...2 exists
    if ("...2" %in% names(df)) {
      df <- df[df[["...2"]] != 2, , drop = FALSE]
    }
    
    # Define all columns to remove
    cols_to_remove <- c("...2", "Image", "Transform", "Col thresh", "Lum thresh")
    
    # Remove only those that exist in the current dataframe
    cols_to_remove <- intersect(cols_to_remove, names(df))
    df <- df[, !names(df) %in% cols_to_remove, drop = FALSE]
    
    # Assign cleaned df back to global environment
    assign(df_name, df, envir = .GlobalEnv)
  }
}



merge_qcpa_leia <- function(qcpa_leia_list, suffixes = c("_100", "_550", "_clips")) {
  merged_dfs <- list()
  
  for (suf in suffixes) {
    qcpa_name <- paste0("QCPAres", suf)
    leia_name <- paste0("LEIAres", suf)
    
    if (exists(qcpa_name, envir = .GlobalEnv) && exists(leia_name, envir = .GlobalEnv)) {
      qcpa_df <- get(qcpa_name, envir = .GlobalEnv)
      leia_df <- get(leia_name, envir = .GlobalEnv)
      
      # Merge by "Name"
      merged <- dplyr::full_join(qcpa_df, leia_df, by = "Name")
      
      # Rename 'Name' column conditionally by suffix
      if ("Name" %in% names(merged)) {
        if (suf %in% c("_100", "_550")) {
          names(merged)[names(merged) == "Name"] <- "TrialName"
        } else if (suf == "_clips") {
          names(merged)[names(merged) == "Name"] <- "FrameName"
        }
      }
      
      # Save into environment and list
      merged_name <- paste0("Merged", suf)
      assign(merged_name, merged, envir = .GlobalEnv)
      merged_dfs[[merged_name]] <- merged
    }
  }
  
  return(merged_dfs)
}


