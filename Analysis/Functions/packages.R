## ---------------------------
##
## Script name: packages.r
##
## Purpose of script: 
##      1. To check if the packages required are installed and loaded
##      2. To list the package versions and references for packages
##
## ---------------------------
##
## Notes:
##   This means only required and missing packages are installed
## ---------------------------

check_and_load_packages <- function(packages) {
  # Check if each package is already installed, and if not, install it
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      renv::install(package)
    }
    library(package, character.only = TRUE) # Loads every package
  }
}



save_package_info <- function(file = "package-versions.txt") {
  # Helper to install and load required packages
  require_package <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  
  # Ensure required packages are available
  require_package("grateful")
  require_package("devtools")
  
  # Write session info and citations
  sink(file)
  cat("=== SESSION INFO ===\n\n")
  print(sessionInfo())
  
  cat("\n\n=== PACKAGE CITATIONS ===\n\n")
  tryCatch(
    {
      cite_packages()
    },
    error = function(e) {
      message("cite_packages() failed: ", e$message)
    }
  )
  sink()
  
  message("Package information saved to: ", file)
}
