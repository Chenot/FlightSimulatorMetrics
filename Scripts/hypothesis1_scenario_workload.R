# Scripts/hypothesis1_scenario_workload.R

# Hypothesis 1: Scenario difficulty affects subjective and objective workload

################################################
######### 1) Prepare data for analyses #########
################################################

library(rstudioapi)

# Load data and libraries
setwd(dirname(getActiveDocumentContext()$path))
source("data_loading.R")

# Correct the incorrect value for participant P013 in scenario VFR-High
df_simu_perf$oddball_percentage[df_simu_perf$Participant == "P013" &
                                  df_simu_perf$Scenario == "VFR-High"] <- NA

# Mapping scenarios to difficulty and flight rules
scenario_mapping <- data.frame(
  Scenario = c("VFR-low", "VFR-high", "IFR-low", "IFR-high"),
  flight_rule = c("VFR", "VFR", "IFR", "IFR"),
  difficulty = c("Low", "High", "Low", "High")
)

# Merge mapping with performance data
df_simu_perf <- merge(df_simu_perf, scenario_mapping, by = "Scenario")

# Convert 'difficulty' and 'flight_rule' to factors with specified levels
df_simu_perf$difficulty <- factor(df_simu_perf$difficulty, levels = c("Low", "High"))
df_simu_perf$flight_rule <- factor(df_simu_perf$flight_rule, levels = c("VFR", "IFR"))

# Rename scenarios and define colors for plotting
df_simu_perf$Scenario <- factor(df_simu_perf$Scenario,
                                levels = c("VFR-low", "VFR-high", "IFR-low", "IFR-high"))
levels(df_simu_perf$Scenario) <- c("VFR-Low", "VFR-High", "IFR-Low", "IFR-High")
scenario_colors <- c("VFR-Low" = "green",
                     "VFR-High" = "orange",
                     "IFR-Low" = "yellow",
                     "IFR-High" = "red")

##############################################################
### 2) Hypothesis 1.a: Subjective workload & scenarios #######
##############################################################

### Statistical analyses (descriptive, model, etc.)

# Calculate descriptive statistics
scenario_summary <- df_simu_perf %>%
  group_by(Scenario) %>%
  summarise(
    Mean = mean(TLX_raw, na.rm = TRUE),
    SD = sd(TLX_raw, na.rm = TRUE)
  ) %>%
  arrange(Scenario)

# Fit mixed-effects model with interaction between difficulty and flight_rule
mixed_model_subjective <- lmer(TLX_raw ~ difficulty * flight_rule + (1 | Participant),
                               data = df_simu_perf)
model_summary_subjective <- summary(mixed_model_subjective)
fixed_effects_subjective <- as.data.frame(model_summary_subjective$coefficients)

# Get confidence intervals for fixed effects
conf_int_subjective <- confint(mixed_model_subjective, parm = "beta_", level = 0.95)

# Prepare results table (optional for reporting)
results_table_subjective <- data.frame(
  Variable = c("(Intercept)", "Difficulty (High)", "Flight Rule (IFR)", "Difficulty × Flight Rule"),
  Estimate = fixed_effects_subjective$Estimate,
  Std_Error = fixed_effects_subjective$`Std. Error`,
  CI_95 = paste0("[", round(conf_int_subjective[, 1], 2), ", ", round(conf_int_subjective[, 2], 2), "]"),
  df = fixed_effects_subjective$df,
  t = fixed_effects_subjective$`t value`,
  p = ifelse(fixed_effects_subjective$`Pr(>|t|)` < 0.001,
             "< 0.001",
             round(fixed_effects_subjective$`Pr(>|t|)`, 3))
)

# Print results table
print(results_table_subjective)

### Create plot

# Extract and format p-values for annotations
p_flight_rule <- fixed_effects_subjective["flight_ruleIFR", "Pr(>|t|)"]
p_difficulty <- fixed_effects_subjective["difficultyHigh", "Pr(>|t|)"]
p_interaction <- fixed_effects_subjective["difficultyHigh:flight_ruleIFR", "Pr(>|t|)"]

p_flight_rule_formatted <- format_p_value(p_flight_rule)
p_difficulty_formatted <- format_p_value(p_difficulty)
p_interaction_formatted <- format_p_value(p_interaction)

# Create the raincloud plot for subjective workload
plot_tlx_scenario <- ggplot(df_simu_perf, aes(x = Scenario, y = TLX_raw, fill = Scenario)) +
  geom_flat_violin(position = position_nudge(x = 0.12), adjust = 1, alpha = 0.5) +
  geom_boxplot(width = 0.2,
               position = position_nudge(x = -0.2),
               outlier.shape = NA,
               alpha = 0.5) +
  geom_point(position = position_jitter(width = 0.08),
             size = 2,
             shape = 21,
             color = "black",
             alpha = 0.7) +
  theme_pubr() +
  scale_fill_manual(values = scenario_colors) +
  labs(title = "A. Subjective Workload",
       x = "Scenario",
       y = "NASA-TLX Score") +
  annotate("text",
           x = 0.5,
           y = 99,
           label = paste("Flight Rule:", p_flight_rule_formatted),
           hjust = 0,
           vjust = 1.1,
           size = 4) +
  annotate("text",
           x = 0.5,
           y = 95,
           label = paste("Difficulty:", p_difficulty_formatted),
           hjust = 0,
           vjust = 1.3,
           size = 4) +
  annotate("text",
           x = 0.5,
           y = 91,
           label = paste("Interaction:", p_interaction_formatted),
           hjust = 0,
           vjust = 1.5,
           size = 4) +
  theme(legend.position = "none")

# Save the plot
plot_file_name <- "TLX_scenarios.pdf"
full_plot_path <- file.path(figures_dir, plot_file_name)
ggsave(full_plot_path,
       plot = plot_tlx_scenario,
       device = "pdf",
       width = 6,
       height = 4,
       units = "in")

# Document the plot creation
if (file.exists(full_plot_path)) {
  message("Plot successfully saved: ", full_plot_path)
} else {
  stop("Plot saving failed: ", full_plot_path)
}

### Generate summary paragraph

# Create a formatted paragraph summarizing the results
paragraph_subjective <- paste(
  "The NASA-TLX mean scores differed across scenarios. For VFR-Low and VFR-High, the mean scores were ",
  sprintf("%.1f (± %.1f)", scenario_summary$Mean[scenario_summary$Scenario == "VFR-Low"],
          scenario_summary$SD[scenario_summary$Scenario == "VFR-Low"]),
  " and ",
  sprintf("%.1f (± %.1f)", scenario_summary$Mean[scenario_summary$Scenario == "VFR-High"],
          scenario_summary$SD[scenario_summary$Scenario == "VFR-High"]),
  " respectively. For IFR-Low and IFR-High, the mean scores were ",
  sprintf("%.1f (± %.1f)", scenario_summary$Mean[scenario_summary$Scenario == "IFR-Low"],
          scenario_summary$SD[scenario_summary$Scenario == "IFR-Low"]),
  " and ",
  sprintf("%.1f (± %.1f)", scenario_summary$Mean[scenario_summary$Scenario == "IFR-High"],
          scenario_summary$SD[scenario_summary$Scenario == "IFR-High"]),
  " respectively. The mixed-effects model revealed a significant difference in NASA-TLX scores, with higher values for High vs. Low conditions (",
  format_p_value(fixed_effects_subjective["difficultyHigh", "Pr(>|t|)"]),
  "), and for IFR vs. VFR conditions (",
  format_p_value(fixed_effects_subjective["flight_ruleIFR", "Pr(>|t|)"]),
  "). No significant interaction was found (",
  format_p_value(fixed_effects_subjective["difficultyHigh:flight_ruleIFR", "Pr(>|t|)"]),
  ").",
  sep = ""
)

# Print the summary paragraph
print(paragraph_subjective)

##############################################################
### 3) Hypothesis 1.b: Objective workload & scenarios ########
##############################################################

### Statistical analyses (descriptive, model, etc.)

# Calculate descriptive statistics for Oddball miss rates
oddball_summary <- df_simu_perf %>%
  group_by(Scenario) %>%
  summarise(
    Mean = mean(oddball_percentage, na.rm = TRUE),
    SD = sd(oddball_percentage, na.rm = TRUE)
  ) %>%
  arrange(Scenario)

# Fit mixed-effects model for Oddball miss rates
mixed_model_objective <- lmer(oddball_percentage ~ difficulty * flight_rule + (1 | Participant),
                              data = df_simu_perf)
model_summary_objective <- summary(mixed_model_objective)
fixed_effects_objective <- as.data.frame(model_summary_objective$coefficients)

# Get confidence intervals
conf_int_objective <- confint(mixed_model_objective, parm = "beta_", level = 0.95)

# Prepare results table (optional for reporting)
results_table_objective <- data.frame(
  Variable = c("(Intercept)", "Difficulty (High)", "Flight Rule (IFR)", "Difficulty × Flight Rule"),
  Estimate = fixed_effects_objective$Estimate,
  Std_Error = fixed_effects_objective$`Std. Error`,
  CI_95 = paste0("[", round(conf_int_objective[, 1], 2), ", ", round(conf_int_objective[, 2], 2), "]"),
  df = fixed_effects_objective$df,
  t = fixed_effects_objective$`t value`,
  p = ifelse(fixed_effects_objective$`Pr(>|t|)` < 0.001,
             "< 0.001",
             round(fixed_effects_objective$`Pr(>|t|)`, 3))
)

# Print results table
print(results_table_objective)

### Create plot

# Extract and format p-values for annotations
p_flight_rule_obj <- fixed_effects_objective["flight_ruleIFR", "Pr(>|t|)"]
p_difficulty_obj <- fixed_effects_objective["difficultyHigh", "Pr(>|t|)"]
p_interaction_obj <- fixed_effects_objective["difficultyHigh:flight_ruleIFR", "Pr(>|t|)"]

p_flight_rule_formatted_obj <- format_p_value(p_flight_rule_obj)
p_difficulty_formatted_obj <- format_p_value(p_difficulty_obj)
p_interaction_formatted_obj <- format_p_value(p_interaction_obj)

# Create the raincloud plot for objective workload
plot_oddball_scenario <- ggplot(df_simu_perf, aes(x = Scenario, y = oddball_percentage, fill = Scenario)) +
  geom_flat_violin(position = position_nudge(x = 0.12), adjust = 1, alpha = 0.5) +
  geom_boxplot(width = 0.2,
               position = position_nudge(x = -0.2),
               outlier.shape = NA,
               alpha = 0.5) +
  geom_point(position = position_jitter(width = 0.08),
             size = 2,
             shape = 21,
             color = "black",
             alpha = 0.7) +
  theme_pubr() +
  scale_fill_manual(values = scenario_colors) +
  labs(title = "B. Objective Workload",
       x = "Scenario",
       y = "Oddball Score (miss percentage)") +
  annotate("text",
           x = 0.5,
           y = max(df_simu_perf$oddball_percentage, na.rm = TRUE) * 0.95,
           label = paste("Flight Rule:", p_flight_rule_formatted_obj),
           hjust = 0,
           vjust = 1.1,
           size = 4) +
  annotate("text",
           x = 0.5,
           y = max(df_simu_perf$oddball_percentage, na.rm = TRUE) * 0.90,
           label = paste("Difficulty:", p_difficulty_formatted_obj),
           hjust = 0,
           vjust = 1.3,
           size = 4) +
  annotate("text",
           x = 0.5,
           y = max(df_simu_perf$oddball_percentage, na.rm = TRUE) * 0.85,
           label = paste("Interaction:", p_interaction_formatted_obj),
           hjust = 0,
           vjust = 1.5,
           size = 4) +
  theme(legend.position = "none")

# Save the plot
plot_file_name_oddball <- "Oddball_Scenarios.pdf"
full_plot_path_oddball <- file.path(figures_dir, plot_file_name_oddball)
ggsave(full_plot_path_oddball,
       plot = plot_oddball_scenario,
       device = "pdf",
       width = 6,
       height = 4,
       units = "in")

# Document the plot creation
if (file.exists(full_plot_path_oddball)) {
  message("Plot successfully saved: ", full_plot_path_oddball)
} else {
  stop("Plot saving failed: ", full_plot_path_oddball)
}

### Generate summary paragraph

# Create a formatted paragraph summarizing the results
paragraph_objective <- paste(
  "The Oddball miss rates differed across scenarios. For VFR-Low and VFR-High, the mean miss rates were ",
  sprintf("%.1f%% (± %.1f%%)", oddball_summary$Mean[oddball_summary$Scenario == "VFR-Low"],
          oddball_summary$SD[oddball_summary$Scenario == "VFR-Low"]),
  " and ",
  sprintf("%.1f%% (± %.1f%%)", oddball_summary$Mean[oddball_summary$Scenario == "VFR-High"],
          oddball_summary$SD[oddball_summary$Scenario == "VFR-High"]),
  " respectively. For IFR-Low and IFR-High, the mean miss rates were ",
  sprintf("%.1f%% (± %.1f%%)", oddball_summary$Mean[oddball_summary$Scenario == "IFR-Low"],
          oddball_summary$SD[oddball_summary$Scenario == "IFR-Low"]),
  " and ",
  sprintf("%.1f%% (± %.1f%%)", oddball_summary$Mean[oddball_summary$Scenario == "IFR-High"],
          oddball_summary$SD[oddball_summary$Scenario == "IFR-High"]),
  " respectively. The mixed-effects model revealed a significant difference in Oddball miss rates, with higher miss rates for High vs. Low conditions (",
  format_p_value(fixed_effects_objective["difficultyHigh", "Pr(>|t|)"]),
  "), and for IFR vs. VFR conditions (",
  format_p_value(fixed_effects_objective["flight_ruleIFR", "Pr(>|t|)"]),
  "). A significant interaction was found (",
  format_p_value(fixed_effects_objective["difficultyHigh:flight_ruleIFR", "Pr(>|t|)"]),
  ").",
  sep = ""
)

# Print the summary paragraph
print(paragraph_objective)

##############################################################
### Save combined plot #######################################
##############################################################

# Combine the two plots into one figure
combined_plot <- grid.arrange(plot_tlx_scenario, plot_oddball_scenario, ncol = 2)

# Save the combined plot
combined_plot_path <- file.path(figures_dir, "workload_Scenarios.pdf")
ggsave(combined_plot_path,
       plot = combined_plot,
       device = "pdf",
       width = 12,
       height = 5,
       units = "in")

# Document the combined plot creation
if (file.exists(combined_plot_path)) {
  message("Combined plot successfully saved: ", combined_plot_path)
} else {
  stop("Combined plot saving failed: ", combined_plot_path)
}
