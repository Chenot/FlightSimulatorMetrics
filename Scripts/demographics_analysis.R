# Scripts/demographics_analysis.R

################################################
######### 1) Prepare data for analyses #########
################################################
library(rstudioapi)

# Load data and libraries
setwd(dirname(getActiveDocumentContext()$path))
source("data_loading.R")

################################################
######## 2) Generate table & paragraph  ########
################################################

### Generate table

# Use the convert_to_years function from utils.R
df_final$Education <- sapply(df_final$NiveauEtudes, convert_to_years)

# Calculate mean and standard deviation for numerical variables
numerical_summary <- df_final %>% 
  summarise(
    Age_Mean = mean(Age, na.rm = TRUE),
    Age_SD = sd(Age, na.rm = TRUE),
    Education_Mean = mean(Education, na.rm = TRUE),
    Education_SD = sd(Education, na.rm = TRUE),
    HeuresVol_Mean = mean(HeuresVol, na.rm = TRUE),
    HeuresVol_SD = sd(HeuresVol, na.rm = TRUE),
    HeuresSimu_Mean = mean(HeuresSimu, na.rm = TRUE),
    HeuresSimu_SD = sd(HeuresSimu, na.rm = TRUE)
  )

# Display the summary for numerical variables
print(numerical_summary)

# Count frequencies for categorical variables
categorical_summary <- df_final %>% 
  summarise(
    Lateralite_Counts = list(table(Lateralite)),
    Genre_Counts = list(table(Genre))
  )

# Extract necessary values
age_mean <- numerical_summary$Age_Mean
education_mean <- numerical_summary$Education_Mean
heures_vol_mean <- numerical_summary$HeuresVol_Mean
heures_vol_min <- min(df_final$HeuresVol, na.rm = TRUE)
heures_vol_max <- max(df_final$HeuresVol, na.rm = TRUE)

# Extract counts for gender and handedness
genre_counts <- categorical_summary$Genre_Counts[[1]]
lateralite_counts <- categorical_summary$Lateralite_Counts[[1]]

# Extract number of males and number of right-handed participants
num_males <- genre_counts["H"]
num_right_handed <- lateralite_counts["Droitier"]

# Total number of participants
total_participants <- nrow(df_final)

### Generate summary paragraph

# Format the paragraph
summary_paragraph <- paste(
  total_participants, " participants (", num_males, " men, ", num_right_handed, " right-handed) were included in the study. ",
  "All participants either held a PPL (Private Pilot License) or were in the process of obtaining one. ",
  "There were no professional pilots in the sample. ",
  "Mean age was ", sprintf("%.1f", age_mean), " years old while mean education level was ", 
  sprintf("%.1f", education_mean), " years. ",
  "Mean flight hours was ", sprintf("%.1f", heures_vol_mean), 
  " [", sprintf("%.0f", heures_vol_min), "-", sprintf("%.0f", heures_vol_max), "].",
  sep = ""
)

# Print the paragraph
print(summary_paragraph)
