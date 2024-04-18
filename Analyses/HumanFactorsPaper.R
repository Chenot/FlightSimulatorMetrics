# Load required packages
library(dplyr)
library(tidyverse)
library(lmerTest)
library(lme4)
library(ggplot2)
library(ggpubr)
library(gghalves)
library(PupillometryR)
library(performance)
library(gridExtra)
library(e1071)
library(rstudioapi)
library(nortest)


##################################################
################ FILE LOADING ####################
##################################################
# Get the directory where the script is located
path_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
message("Script directory: ", path_directory)
figures_dir <- file.path(path_directory, "Figures")

# Define the names of the files to look for
file_names <- c("df_final.csv", "simu_perf_processed.csv")

# Initialize list to hold data frames
data_frames <- list()

# Loop through the file names and load them if they exist
for (file_name in file_names) {
  full_path <- file.path(path_directory, file_name)
  
  # Check if the file exists and read it
  if (file.exists(full_path)) {
    data <- read.csv(full_path)
    data_frames[[file_name]] <- data
    message("File loaded: ", file_name)
  } else {
    message("File not found: ", full_path)
  }
}

# If the files were loaded, assign them to variables
if (!is.null(data_frames[["df_final.csv"]])) {
  df_final <- data_frames[["df_final.csv"]]
}

if (!is.null(data_frames[["simu_perf_processed.csv"]])) {
  df_simu_perf <- data_frames[["simu_perf_processed.csv"]]
}


##############################################################
################ DATA ANAYSIS - DEMOGRAPHICS #################
##############################################################
# Function to extract numeric value from "Bac+X" and convert it to years of education
convert_to_years <- function(x) {
  num <- as.numeric(sub("Bac\\+", "", x)) # Extract number part and convert to numeric
  return(num + 12) # Add 12 years for "Bac"
}

# Apply the function to the NiveauEtudes column and calculate mean and sd
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

# Extract the necessary values
age_mean <- numerical_summary$Age_Mean
education_mean <- numerical_summary$Education_Mean
heures_vol_mean <- numerical_summary$HeuresVol_Mean

# Extract counts for gender and handedness
genre_counts <- categorical_summary$Genre_Counts[[1]]
lateralite_counts <- categorical_summary$Lateralite_Counts[[1]]

# Extract number of males and number of right-handed participants
num_males <- genre_counts["H"]
num_right_handed <- lateralite_counts["Droitier"]

# Total number of participants (assuming row count of df_final is the total)
total_participants <- nrow(df_final)

# Format the paragraph
summary_paragraph <- paste(
  "Thirty participants (", num_males, " men, ", num_right_handed, " right-handed) were included in the study. ",
  "All participants either held a PPL (Private Pilot License) or were in the process of obtaining one. ",
  "There were no professional pilots in the sample. ",
  "Mean age was ", sprintf("%.1f", age_mean), " years old while mean education level was ", 
  sprintf("%.1f", education_mean), " years. ",
  "Mean flights hours was ", sprintf("%.1f", heures_vol_mean), ".",
  sep=""
)

# Print the improved paragraph
print(summary_paragraph)




##############################################################
################ DATA ANAYSIS - WORKLOAD #####################
##############################################################
# Create a new column 'simu_all_scenarios' which is the mean of the four scenarios
df_final$simu_all_scenarios <- rowMeans(df_final[,c("X3_VFR.low_simu", "X3_VFR.high_simu", "X3_IFR.low_simu", "X3_IFR.high_simu")], na.rm = TRUE)

#### SCENARIO DIFFICULTY AND SUBJECTIVE WORKLOAD (NASA-TLX)
# Create a new data frame with the mapping
scenario_mapping <- data.frame(
  Scenario = c("VFR-low", "VFR-high", "IFR-low", "IFR-high"),
  flight_rule = c("VFR", "VFR", "IFR", "IFR"),
  difficulty = c("Low", "High", "Low", "High")
)

# Merge the mapping data frame with your original data frame
df_simu_perf <- merge(df_simu_perf, scenario_mapping, by = "Scenario")

# Change the levels of the "difficulty" column
df_simu_perf$difficulty <- factor(df_simu_perf$difficulty, levels = c("Low", "High"))
df_simu_perf$flight_rule <- factor(df_simu_perf$flight_rule, levels = c("VFR", "IFR"))

# Rename scenarios
df_simu_perf$Scenario <- factor(df_simu_perf$Scenario, levels = c("VFR-low", "VFR-high", "IFR-low", "IFR-high"))
levels(df_simu_perf$Scenario) <- c("VFR-Low", "VFR-High", "IFR-Low", "IFR-High")
scenario_colors <- c("VFR-Low" = "green", "VFR-High" = "orange", "IFR-Low" = "yellow", "IFR-High" = "red")

# Extract mean and standard deviation from df_simu_perf
scenario_summary <- df_simu_perf %>%
  group_by(Scenario) %>%
  summarise(
    Mean = mean(TLX_raw, na.rm = TRUE),
    SD = sd(TLX_raw, na.rm = TRUE)
  ) %>%
  arrange(Scenario)

# Fit a mixed-effects model with the interaction between difficulty and flight_rule
mixed_model_interaction <- lmer(TLX_raw ~ difficulty * flight_rule + (1 | Participant), data = df_simu_perf)
summary(mixed_model_interaction)
model_summary <- summary(mixed_model_interaction)
fixed_effects <- as.data.frame(model_summary$coefficients)

# Create a formatted paragraph
paragraph <- paste(
  "The NASA-TLX mean scores differed across scenarios. For VFR-Low and VFR-High, the mean scores were ", 
  sprintf("%.1f (± %.1f)", scenario_summary$Mean[scenario_summary$Scenario == "VFR-Low"], scenario_summary$SD[scenario_summary$Scenario == "VFR-Low"]),
  " and ", 
  sprintf("%.1f (± %.1f)", scenario_summary$Mean[scenario_summary$Scenario == "VFR-High"], scenario_summary$SD[scenario_summary$Scenario == "VFR-High"]),
  " respectively. For IFR-Low and IFR-High, the mean scores were ",
  sprintf("%.1f (± %.1f)", scenario_summary$Mean[scenario_summary$Scenario == "IFR-Low"], scenario_summary$SD[scenario_summary$Scenario == "IFR-Low"]),
  " and ", 
  sprintf("%.1f (± %.1f)", scenario_summary$Mean[scenario_summary$Scenario == "IFR-High"], scenario_summary$SD[scenario_summary$Scenario == "IFR-High"]),
  " respectively. The Mixed-Model revealed a significant difference in NASA-TLX scores, with higher values for High vs Low conditions (p = ",
  sprintf("%.5f", fixed_effects["difficultyHigh", "Pr(>|t|)"]),
  "), and for IFR vs VFR conditions (p = ",
  sprintf("%.5f", fixed_effects["flight_ruleIFR", "Pr(>|t|)"]),
  "). No significant interaction was found (p = ",
  sprintf("%.5f", fixed_effects["difficultyHigh:flight_ruleIFR", "Pr(>|t|)"]),
  "). Results are highlighted in Figure X and Table X.",
  sep = ""
)

# Print the paragraph
print(paragraph)

# Define a function to format the p-values
format_p_value <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(sprintf("p = %.3f", p))
  }
}

# Extract p-values
p_flight_rule <- fixed_effects["flight_ruleIFR", "Pr(>|t|)"]
p_difficulty <- fixed_effects["difficultyHigh", "Pr(>|t|)"]
p_interaction <- fixed_effects["difficultyHigh:flight_ruleIFR", "Pr(>|t|)"]

# Format p-values
p_flight_rule_formatted <- format_p_value(p_flight_rule)
p_difficulty_formatted <- format_p_value(p_difficulty)
p_interaction_formatted <- format_p_value(p_interaction)


# Create the raincloud plot
plot_tlx_scenario <- ggplot(df_simu_perf, aes(x = Scenario, y = TLX_raw, fill = Scenario)) +
  geom_flat_violin(position = position_nudge(x = 0.12, y = 0), adjust = 1, alpha = 0.5) + # Half-violin plot (density plot)
  geom_boxplot(width = 0.2, position = position_nudge(x = -0.2), outlier.shape = NA, alpha = 0.5) + # Box plot
  geom_point(position = position_jitter(width = 0.08), size = 2, shape = 21, color = "black", alpha = 0.7) + # Points (jitter)
  theme_pubr() + # Adjust theme
  scale_fill_manual(values = scenario_colors) + # Fill colors for half-violin
  labs(title = "A. Subjective Workload", x = "Scenario", y = "TLX Score") +
  annotate("text", x = 0.5, y = 99, label = paste("Flight Rule:", p_flight_rule_formatted),
           hjust = 0, vjust = 1.1, size = 4) +
  annotate("text", x = 0.5, y = 95, label = paste("Difficulty:", p_difficulty_formatted),
           hjust = 0, vjust = 1.3, size = 4) +
  annotate("text", x = 0.5, y = 91, label = paste("Interaction:", p_interaction_formatted),
           hjust = 0, vjust = 1.5, size = 4) +
  theme(legend.position = "none")

# Save the plot
plot_file_name <- "TLX_scenarios.pdf" # Create the plot file name
full_plot_path <- file.path(figures_dir, plot_file_name)  # Full path of the plot file
ggsave(full_plot_path, plot = plot_tlx_scenario, device = "pdf", width = 6, height = 4, units = "in")


#### SCENARIO DIFFICULTY AND OBJECTIVE WORKLOAD (ODDBALL)
df_simu_perf$oddball_percentage[df_simu_perf$Participant == "P013" & df_simu_perf$Scenario == "VFR-High"] <- NA #Change the incorrect value

# Calculate Mean and SD
oddball_descriptives <- df_simu_perf %>%
  group_by(Scenario) %>%
  summarise(
    Mean = mean(oddball_percentage, na.rm = TRUE),
    SD = sd(oddball_percentage, na.rm = TRUE)
  ) %>%
  arrange(Scenario)

# 3. Fit Mixed-Effects Model and Extract Values
mixed_model_interaction_oddball <- lmer(oddball_percentage ~ difficulty * flight_rule + (1 | Participant), data = df_simu_perf)
model_summary_oddball <- summary(mixed_model_interaction_oddball)
fixed_effects_oddball <- as.data.frame(model_summary_oddball$coefficients)

# 4. Create a LaTeX-Friendly Paragraph
paragraph_oddball <- paste(
  "The Oddball percentage miss rate differed across scenarios. For VFR-Low and VFR-High, the mean miss rate was ", 
  sprintf("%.1f (± %.1f)\\%%", oddball_descriptives$Mean[oddball_descriptives$Scenario == "VFR-Low"], oddball_descriptives$SD[oddball_descriptives$Scenario == "VFR-Low"]),
  " and ", 
  sprintf("%.1f (± %.1f)\\%%", oddball_descriptives$Mean[oddball_descriptives$Scenario == "VFR-High"], oddball_descriptives$SD[oddball_descriptives$Scenario == "VFR-High"]),
  " respectively. For IFR-Low and IFR-High, the mean miss rate was ",
  sprintf("%.1f (± %.1f)\\%%", oddball_descriptives$Mean[oddball_descriptives$Scenario == "IFR-Low"], oddball_descriptives$SD[oddball_descriptives$Scenario == "IFR-Low"]),
  " and ", 
  sprintf("%.1f (± %.1f)\\%%", oddball_descriptives$Mean[oddball_descriptives$Scenario == "IFR-High"], oddball_descriptives$SD[oddball_descriptives$Scenario == "IFR-High"]),
  " respectively. The Mixed-Model revealed a significant difference in Oddball score, with higher miss rates for High vs Low conditions (p = ",
  sprintf("%.5f", fixed_effects_oddball["difficultyHigh", "Pr(>|t|)"]),
  "), and for IFR vs VFR conditions (p = ",
  sprintf("%.5f", fixed_effects_oddball["flight_ruleIFR", "Pr(>|t|)"]),
  "). A significant interaction was found (p = ",
  sprintf("%.5f", fixed_effects_oddball["difficultyHigh:flight_ruleIFR", "Pr(>|t|)"]),
  "). Results are highlighted in Figure X and Table X.",  # Replace X with actual figure and table numbers
  sep = ""
)

# Print the paragraph
print(paragraph_oddball)

# Extract p-values
p_flight_rule <- fixed_effects_oddball["flight_ruleIFR", "Pr(>|t|)"]
p_difficulty <- fixed_effects_oddball["difficultyHigh", "Pr(>|t|)"]
p_interaction <- fixed_effects_oddball["difficultyHigh:flight_ruleIFR", "Pr(>|t|)"]

# Format p-values
p_flight_rule_formatted <- format_p_value(p_flight_rule)
p_difficulty_formatted <- format_p_value(p_difficulty)
p_interaction_formatted <- format_p_value(p_interaction)


# Create the plot for oddball_percentage
plot_oddball_scenario <- ggplot(df_simu_perf, aes(x = Scenario, y = oddball_percentage, fill = Scenario)) +
  geom_flat_violin(position = position_nudge(x = 0.12, y = 0), adjust = 1, alpha = 0.5) + # Half-violin plot (density plot)
  geom_boxplot(width = 0.2, position = position_nudge(x = -0.2), outlier.shape = NA, alpha = 0.5) + # Box plot
  geom_point(position = position_jitter(width = 0.08), size = 2, shape = 21, color = "black", alpha = 0.7) + # Points (jitter)
  theme_pubr() + # Adjust theme
  scale_fill_manual(values = scenario_colors) + # Fill colors for half-violin
  labs(title = "B. Objective Workload", x = "Scenario", y = "Oddball Score (miss rate percentage)") +
  annotate("text", x = 0.5, y = 79, label = paste("Flight Rule:", p_flight_rule_formatted),
           hjust = 0, vjust = 1.1, size = 4) +
  annotate("text", x = 0.5, y = 75, label = paste("Difficulty:", p_difficulty_formatted),
           hjust = 0, vjust = 1.3, size = 4) +
  annotate("text", x = 0.5, y = 71, label = paste("Interaction:", p_interaction_formatted),
           hjust = 0, vjust = 1.5, size = 4) +
  theme(legend.position = "none")

# Save the plot
plot_file_name_oddball <- "Oddball_Scenarios.pdf"  # File name
full_plot_path_oddball <- file.path(figures_dir, plot_file_name_oddball)  # Full path
ggsave(full_plot_path_oddball, plot = plot_oddball_scenario, device = "pdf", width = 6, height = 4, units = "in")


# Combien Plots
combined_plot <- grid.arrange(plot_tlx_scenario, plot_oddball_scenario, ncol = 2)

# Save the combined plot
combined_plot_path <- file.path(figures_dir, "workload_Scenarios.pdf")
ggsave(combined_plot_path, plot = combined_plot, device = "pdf", width = 11, height = 5, units = "in")



#### SUBJECTIVE (TLX) AND OBJECTIVE WORKLOAD (ODDBALL)
model <- lmer(oddball_percentage ~ TLX_raw + (1 | Participant), data = df_simu_perf)
summary(model)
model_r2 <- r2(model) # Calculate R²
print(model_r2) # The output will have both marginal and conditional R²

# Extract fixed effect estimates for TLX_raw and intercept
tlx_raw_coef <- fixef(model)["TLX_raw"]
intercept <- fixef(model)["(Intercept)"]

# Format the regression equation as a string
regression_eq <- sprintf("y = %.2fx%.2f", tlx_raw_coef, intercept)

# Calculate predicted values for a range of TLX_raw
tlx_raw_range <- range(df_simu_perf$TLX_raw, na.rm = TRUE)
predicted_oddball <- data.frame(
  TLX_raw = seq(from = tlx_raw_range[1], to = tlx_raw_range[2], length.out = 100),
  Predicted_oddball = (tlx_raw_coef * seq(from = tlx_raw_range[1], to = tlx_raw_range[2], length.out = 100)) + fixef(model)["(Intercept)"]
)

#Draw plot
plot_oddball_TLX <- ggplot(df_simu_perf, aes(x = TLX_raw, y = oddball_percentage)) +
  geom_point(aes(fill = Scenario), shape = 21, color = "black", size = 4) +
  geom_line(data = predicted_oddball, aes(x = TLX_raw, y = Predicted_oddball), color = "black") +
  scale_fill_manual(values = scenario_colors) +
  labs(x = "TLX Score", y = "Oddball Score (miss rate percentage)") +
  theme_pubr() +
  theme(legend.position = c(0.01, 1), legend.justification = c(0, 1),legend.background = element_rect(color = "black", fill = "white", size = .4)) + # Position legend at top left
  annotate("text", x = 85, y = 0, na.rm = TRUE, label = regression_eq, hjust = 0, vjust = 0, size = 4)

# Save the plot
plot_file_name_workload <- "plot_oddball_TLX.pdf"  # File name
full_plot_path_workload <- file.path(figures_dir, plot_file_name_workload)  # Full path
ggsave(full_plot_path_workload, plot = plot_oddball_TLX, device = "pdf", width = 6, height = 4, units = "in")

# Fit the model using lmerTest for p-value calculation
summary_model <- summary(model)

# Extract necessary information
number_of_obs <- length(summary_model$residuals)
number_of_participants <- length(levels(df_simu_perf$Participant))
tlx_raw_coef <- fixef(model)["TLX_raw"]
intercept <- fixef(model)["(Intercept)"]
std_error_tlx_raw <- summary_model$coefficients["TLX_raw", "Std. Error"]
t_value_tlx_raw <- summary_model$coefficients["TLX_raw", "t value"]
p_value_tlx_raw <- summary_model$coefficients["TLX_raw", "Pr(>|t|)"]
p_value_formatted <- ifelse(p_value_tlx_raw < 0.001, "< 0.001", sprintf("%.5f", p_value_tlx_raw))

# Create the report paragraph
report_paragraph <- paste(
  "A linear mixed-effects model was fitted to understand the relationship between the Oddball miss rate percentage and the raw NASA Task Load Index (TLX) scores, including ",
  number_of_participants, " participants and a total of ", number_of_obs, " observations. ",
  "The fixed effect of TLX_raw on the Oddball miss rate was significant, with an estimate of ", 
  sprintf("%.5f", tlx_raw_coef), " (SE = ", sprintf("%.5f", std_error_tlx_raw), ", t = ", 
  sprintf("%.3f", t_value_tlx_raw), ", p = ", p_value_formatted, "). ",
  "This indicates that higher TLX scores are associated with an increased Oddball miss rate. ",
  "The intercept of the model was ", sprintf("%.5f", intercept), ".",
  sep = ""
)

# Print the report paragraph
print(report_paragraph)


#### SUBJECTIVE (TLX) AND OBJECTIVE WORKLOAD (ODDBALL)
model <- lmer(TLX_raw ~ oddball_percentage + (1 | Participant), data = df_simu_perf)
summary(model)

# Calculate R²
model_r2 <- r2(model)
print(model_r2) # The output will have both marginal and conditional R²

# Extract fixed effect estimates for oddball_percentage and intercept
oddball_coef <- fixef(model)["oddball_percentage"]
intercept <- fixef(model)["(Intercept)"]

# Format the regression equation as a string
regression_eq <- sprintf("y = %.2fx + %.2f", oddball_coef, intercept)

# Calculate predicted values for a range of oddball_percentage
oddball_range <- range(df_simu_perf$oddball_percentage, na.rm = TRUE)
predicted_tlx <- data.frame(
  Oddball_percentage = seq(from = oddball_range[1], to = oddball_range[2], length.out = 100),
  Predicted_TLX = (oddball_coef * seq(from = oddball_range[1], to = oddball_range[2], length.out = 100)) + intercept
)

# Draw plot
plot_tlx_oddball <- ggplot(df_simu_perf, aes(x = oddball_percentage, y = TLX_raw)) +
  geom_point(aes(fill = Scenario), shape = 21, color = "black", size = 4) +
  geom_line(data = predicted_tlx, aes(x = Oddball_percentage, y = Predicted_TLX), color = "black") +
  scale_fill_manual(values = scenario_colors) +
  labs(x = "Oddball Score (miss rate percentage)", y = "TLX Score") +
  theme_pubr() +
  theme(legend.position = c(1, 0.01), legend.justification = c(1, 0), legend.background = element_rect(color = "black", fill = "white", size = .4)) +
  annotate("text", x = 55, y = 95, label = regression_eq, hjust = 0, vjust = 0, size = 4)

# Save the plot
plot_file_name_tlx <- "plot_tlx_oddball.pdf"
full_plot_path_tlx <- file.path(figures_dir, plot_file_name_tlx)
ggsave(full_plot_path_tlx, plot = plot_tlx_oddball, device = "pdf", width = 6, height = 4, units = "in")

# Fit the model using lmerTest for p-value calculation
summary_model <- summary(model)

# Extract necessary information
number_of_obs <- length(summary_model$residuals)
number_of_participants <- length(levels(df_simu_perf$Participant))
oddball_coef <- fixef(model)["oddball_percentage"]
intercept <- fixef(model)["(Intercept)"]
std_error_oddball <- summary_model$coefficients["oddball_percentage", "Std. Error"]
t_value_oddball <- summary_model$coefficients["oddball_percentage", "t value"]
p_value_oddball <- summary_model$coefficients["oddball_percentage", "Pr(>|t|)"]
p_value_formatted <- ifelse(p_value_oddball < 0.001, "< 0.001", sprintf("%.5f", p_value_oddball))

# Create the report paragraph
report_paragraph <- paste(
  "A linear mixed-effects model was fitted to understand the relationship between the raw NASA Task Load Index (TLX) scores and the Oddball miss rate percentage, including ",
  number_of_participants, " participants and a total of ", number_of_obs, " observations. ",
  "The fixed effect of oddball_percentage on TLX scores was significant, with an estimate of ", 
  sprintf("%.5f", oddball_coef), " (SE = ", sprintf("%.5f", std_error_oddball), ", t = ", 
  sprintf("%.3f", t_value_oddball), ", p = ", p_value_formatted, "). ",
  "This indicates that higher Oddball miss rates are associated with increased TLX scores. ",
  "The intercept of the model was ", sprintf("%.5f", intercept), ".",
  sep = ""
)

# Print the report paragraph
print(report_paragraph)



#############################################################
############### Composite score & Workload ##################
#############################################################
# Plot for TLX
p_tlx <- df_simu_perf %>%
  ggplot(aes(x = TLX_raw, y = simu_global_perf, fill = Scenario)) +
  geom_point(colour = "black", shape = 21, size = 4) +  # Black border with fill color
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  stat_cor(method = "pearson", aes(group = NULL), label.y = 1.3) +  # Add correlation info
  scale_fill_manual(values = scenario_colors) +
  facet_wrap(~Scenario, scales = "free") +
  labs(x = "TLX Score",
       y = "Composite Score") +
  theme_pubr() +
  theme(legend.position = "none")  # Remove legend


# Plot for Oddball
p_oddball <- df_simu_perf %>%
  ggplot(aes(x = oddball_percentage, y = simu_global_perf, fill = Scenario)) +
  geom_point(colour = "black", shape = 21, size = 4) +  # Black border with fill color
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  stat_cor(method = "pearson", aes(group = NULL), label.y = 1.5) +  # Add correlation info
  scale_fill_manual(values = scenario_colors) +
  facet_wrap(~Scenario, scales = "free") +
  labs(x = "Oddball Score (miss rate percentage)",
       y = "Simulator Performance") +
  theme_pubr() +
  theme(legend.position = "none")  # Remove legend

# Save the plots
plot_file_name_composite_oddball <- "plot_composite_oddball.pdf"
full_plot_path_composite_oddball <- file.path(figures_dir, plot_file_name_composite_oddball)
ggsave(full_plot_path_composite_oddball, plot = p_oddball, device = "pdf", width = 7, height = 6, units = "in")
plot_file_name_composite_TLX <- "plot_composite_TLX.pdf"
full_plot_path_composite_TLX <- file.path(figures_dir, plot_file_name_composite_TLX)
ggsave(full_plot_path_composite_TLX, plot = p_tlx, device = "pdf", width = 7, height = 6, units = "in")


# Function to calculate correlation and return a formatted string
get_correlation_report <- function(data, x_var, y_var, scenario) {
  cor_result <- cor.test(data[[x_var]], data[[y_var]], method = "pearson")
  sprintf("In the %s scenario, the Pearson correlation between %s and %s was r = %.2f (p = %.3f).", 
          scenario, x_var, y_var, cor_result$estimate, cor_result$p.value)
}

# Calculate and store the correlation results
results <- lapply(unique(df_simu_perf$Scenario), function(scenario) {
  data_subset <- df_simu_perf %>% filter(Scenario == scenario)
  report_tlx <- get_correlation_report(data_subset, "TLX_raw", "simu_global_perf", scenario)
  report_oddball <- get_correlation_report(data_subset, "oddball_percentage", "simu_global_perf", scenario)
  list(tlx = report_tlx, oddball = report_oddball)
})

# Construct the report paragraph
report_paragraphs <- lapply(results, function(res) {
  paste(res$tlx, res$oddball, sep = " ")
})

# Combine all paragraphs into one text
final_report <- paste(unlist(report_paragraphs), collapse = " ")
print(final_report)




#####################################################################
################ SUPPLEMENTARY MATERIAL - COMPOSITE SCORE ###########
#####################################################################

## Composite score psychometrics
# Calculate psychometric analysis for simu_global_perf across scenarios
psychometric_analysis <- df_simu_perf %>%
  group_by(Scenario) %>%
  summarise(
    mean = mean(simu_global_perf, na.rm = TRUE),
    sd = sd(simu_global_perf, na.rm = TRUE),
    skewness = skewness(simu_global_perf, na.rm = TRUE),
    kurtosis = kurtosis(simu_global_perf, na.rm = TRUE),
    lilliefors_p_value = lillie.test(simu_global_perf[!is.na(simu_global_perf)])$p.value
  )

# View the resulting table
print(psychometric_analysis)

# Creating the density plot
density_plot <- ggplot(df_simu_perf, aes(x = simu_global_perf, fill = Scenario)) +
  geom_density(alpha = 0.5) +
  geom_rug() +
  scale_fill_manual(values = scenario_colors) +
  facet_wrap(~ Scenario, scales = "free") +
  theme_pubr() +
  labs(x = "Composite Score", y = "Density")

# Save the plot
plot_file_name_tlx <- "plot_density_composite_score.pdf"
full_plot_path_tlx <- file.path(figures_dir, plot_file_name_tlx)
ggsave(full_plot_path_tlx, plot = density_plot, device = "pdf", width = 8, height = 6, units = "in")



#####################################################################
################ SUPPLEMENTARY MATERIAL #############################
#####################################################################
# ## Composite score psychometrics - relationship with flight hours
# # Rename the 'Participant_Powerbrain' column in df_final to 'Participant' for consistency
# df_final$Participant <- df_final$Participant_Powerbrain
# 
# # Select only the necessary columns from df_final
# df_final_subset <- df_final[, c('Participant', 'HeuresVol', 'HeuresSimu')]
# 
# # Merge the dataframes
# df_simu_perf_merged <- merge(df_simu_perf, df_final_subset, by = "Participant")
# 
# 
# # Plot for Flight Hours
# p_flight_hours <- df_simu_perf_merged %>%
#   ggplot(aes(x = HeuresVol, y = simu_global_perf, fill = Scenario)) +
#   geom_point(colour = "black", shape = 21, size = 4) +  # Black border with fill color
#   geom_smooth(method = "lm", se = FALSE, color = "black") +
#   stat_cor(method = "pearson", aes(group = NULL), label.x = 50, label.y = 1.5) +  # Add correlation info
#   scale_fill_manual(values = scenario_colors) +
#   facet_wrap(~Scenario, scales = "free") +
#   labs(title = "Composite Score & Flight Hours",
#        x = "Flight Hours",
#        y = "Composite Score") +
#   theme_pubr() +
#   theme(legend.position = "none")  # Remove legend
# 
# p_flight_hours_all <- df_simu_perf_merged %>%
#   ggplot(aes(x = HeuresVol, y = simu_global_perf, fill = Scenario)) +
#   geom_point(colour = "black", shape = 21, size = 4) +  # Black border with fill color
#   geom_smooth(method = "lm", se = FALSE, color = "black") +
#   stat_cor(method = "pearson", aes(group = NULL), label.x = 50, label.y = 1.5) +  # Add correlation info
#   scale_fill_manual(values = scenario_colors) +
#   labs(title = "Composite Score & Flight Hours",
#        x = "Flight Hours",
#        y = "Composite Score") +
#   theme_pubr() +
#   theme(legend.position = "none")  # Remove legend
# 
# 
# # Plot for Flight Simulator Hours
# p_flight_simulator_hours <- df_simu_perf_merged %>%
#   ggplot(aes(x = HeuresSimu, y = simu_global_perf, fill = Scenario)) +
#   geom_point(colour = "black", shape = 21, size = 4) +  # Black border with fill color
#   geom_smooth(method = "lm", se = FALSE, color = "black") +
#   stat_cor(method = "pearson", aes(group = NULL), label.x = 2000, label.y = 1.5) +  # Add correlation info
#   scale_fill_manual(values = scenario_colors) +
#   facet_wrap(~Scenario, scales = "free") +
#   labs(title = "Composite Score & Flight Simulator Hours",
#        x = "Flight Simulator Hours",
#        y = "Composite Score") +
#   theme_pubr() +
#   theme(legend.position = "none")  # Remove legend
# 
# 
# #############################################################
# ## Workload and flight hours
# # Plot for Flight Hours vs Oddball Percentage
# p_flight_hours_oddball <- df_simu_perf_merged %>%
#   ggplot(aes(x = HeuresVol, y = oddball_percentage, fill = Scenario)) +
#   geom_point(colour = "black", shape = 21, size = 4) +  # Black border with fill color
#   geom_smooth(method = "lm", se = FALSE, color = "black") +
#   stat_cor(method = "pearson", aes(group = NULL), label.x = 50, label.y = 50) +  # Add correlation info
#   scale_fill_manual(values = scenario_colors) +
#   facet_wrap(~Scenario, scales = "free") +
#   labs(title = "Flight Hours & Oddball Performance",
#        x = "Flight Hours",
#        y = "Oddball Score (miss rate percentage)") +
#   theme_pubr() +
#   theme(legend.position = "none")  # Remove legend
# 
# # Plot for Flight Hours vs TLX Raw Score
# p_flight_hours_tlx <- df_simu_perf_merged %>%
#   ggplot(aes(x = HeuresVol, y = TLX_raw, fill = Scenario)) +
#   geom_point(colour = "black", shape = 21, size = 4) +  # Black border with fill color
#   geom_smooth(method = "lm", se = FALSE, color = "black") +
#   stat_cor(method = "pearson", aes(group = NULL), label.x = 50, label.y = 80) +  # Add correlation info
#   scale_fill_manual(values = scenario_colors) +
#   facet_wrap(~Scenario, scales = "free") +
#   labs(title = "Flight Hours & Subjective Workload (TLX Score)",
#        x = "Flight Hours",
#        y = "TLX Raw Score") +
#   theme_pubr() +
#   theme(legend.position = "none")  # Remove legend
# 
# 
# 
# 
# # Fit the multiple regression model
# model <- lmer(simu_global_perf ~ HeuresVol + TLX_raw + oddball_percentage + (1 | Participant), data = df_simu_perf_merged)
# 
# # Summary of the model to check coefficients and statistics
# summary(model)
# 
# # Diagnostics for assumptions checking
# plot(model)
# 

