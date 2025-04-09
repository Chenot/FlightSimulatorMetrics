# Scripts/data_loading.R

# Load utility functions (this will also load required packages)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("utils.R")

# Set up paths
paths <- setup_paths()
path_directory <- paths$path_directory
data_dir <- paths$data_dir
figures_dir <- paths$figures_dir

# Define the names of the files to look for
file_names <- c("df_final.csv", "simu_perf_processed.csv", "app_questionnaire.csv")

# Initialize list to hold data frames
data_frames <- list()

# Loop through the file names and load them if they exist
for (file_name in file_names) {
  full_path <- file.path(data_dir, file_name)
  
  # Check if the file exists and read it
  if (file.exists(full_path)) {
    data_frames[[file_name]] <- read.csv(full_path)
    message("File loaded: ", full_path)
  } else {
    stop("File not found: ", full_path)
  }
}

# Assign loaded data frames to variables
df_final <- data_frames[["df_final.csv"]]
df_simu_perf <- data_frames[["simu_perf_processed.csv"]]
df_questionnaire <- data_frames[["app_questionnaire.csv"]]
