# Scripts/utils.R

# Function to install and load required packages
install_and_load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      message("Installing package: ", pkg)
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    } else {
      library(pkg, character.only = TRUE)
    }
  }
}

# List of required packages
required_packages <- c(
  "dplyr", "tidyverse", "lmerTest", "lme4", "broom", "ggplot2", "ggpubr", 
  "gghalves", "PupillometryR", "performance", "gridExtra", "e1071", "moments",
  "rstudioapi", "nortest", "xtable", "reshape2", "tidyr", "kableExtra"
)

# Install and load packages
install_and_load_packages(required_packages)

# Function to format p-values
format_p_value <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(sprintf("p = %.3f", p))
  }
}

# Function to calculate correlation statistics
calculate_stats <- function(df, x, y) {
  cor_test <- cor.test(df[[x]], df[[y]], method = "pearson")
  r <- cor_test$estimate
  p <- cor_test$p.value
  model <- lm(as.formula(paste(y, "~", x)), data = df)
  R2 <- summary(model)$r.squared
  list(r = r, p = p, R2 = R2)
}

# Function to convert education levels to years
convert_to_years <- function(x) {
  num <- as.numeric(sub("Bac\\+", "", x))
  return(num + 12)
}

# Function to set up paths
setup_paths <- function() {
  # Get the directory of the current script
  path_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
  
  # Set paths for data and figures
  data_dir <- file.path(dirname(path_directory), "Data")
  figures_dir <- file.path(dirname(path_directory), "Figures")
  
  # Create figures directory if it doesn't exist
  if (!dir.exists(figures_dir)) {
    dir.create(figures_dir)
  }
  
  list(path_directory = path_directory, data_dir = data_dir, figures_dir = figures_dir)
}
