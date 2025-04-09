# Scripts/hypothesis2_subjective_objective_workload.R

# Hypothesis 2: Relationship between subjective (NASA-TLX) and objective (Oddball) workload measures

#######################################################
############### 1) Prepare data for analyses ##########
#######################################################

library(rstudioapi)

# Load data and libraries
setwd(dirname(getActiveDocumentContext()$path))
source("data_loading.R")

# Correct the incorrect value for participant P013 in scenario VFR-High
df_simu_perf$oddball_percentage[df_simu_perf$Participant == "P013" &
                                  df_simu_perf$Scenario == "VFR-High"] <- NA

# Ensure Scenario factor levels and colors are set
df_simu_perf$Scenario <- factor(df_simu_perf$Scenario,
                                levels = c("VFR-low", "VFR-high", "IFR-low", "IFR-high"))
levels(df_simu_perf$Scenario) <- c("VFR-Low", "VFR-High", "IFR-Low", "IFR-High")
scenario_colors <- c("VFR-Low" = "green",
                     "VFR-High" = "orange",
                     "IFR-Low" = "yellow",
                     "IFR-High" = "red")

###############################################################
### 2) Hypothesis 2: Subjective workload & Objective workload #
###############################################################

### Statistical analyses (descriptive, model, etc.)

# Fit a linear mixed-effects model
model <- lmer(TLX_raw ~ oddball_percentage + (1 | Participant), data = df_simu_perf)
summary_model <- summary(model)

# Calculate R-squared
model_r2 <- r2(model)
print(model_r2)

# Extract fixed effect estimates
oddball_coef <- fixef(model)["oddball_percentage"]
intercept <- fixef(model)["(Intercept)"]

# Extract model statistics
number_of_obs <- length(summary_model$residuals)
number_of_participants <- length(unique(df_simu_perf$Participant))
std_error_oddball <- summary_model$coefficients["oddball_percentage", "Std. Error"]
t_value_oddball <- summary_model$coefficients["oddball_percentage", "t value"]
p_value_oddball <- summary_model$coefficients["oddball_percentage", "Pr(>|t|)"]
p_value_formatted <- format_p_value(p_value_oddball)

### Create plot

# Format the regression equation
regression_eq <- sprintf("y = %.2fx + %.2f", oddball_coef, intercept)

# Prepare data for plotting
oddball_range <- range(df_simu_perf$oddball_percentage, na.rm = TRUE)
predicted_tlx <- data.frame(
  oddball_percentage = seq(from = oddball_range[1], to = oddball_range[2], length.out = 100)
)
predicted_tlx$Predicted_TLX <- predict(model, newdata = predicted_tlx, re.form = NA)

# Create the plot
plot_tlx_oddball <- ggplot(df_simu_perf, aes(x = oddball_percentage, y = TLX_raw)) +
  geom_point(aes(fill = Scenario), shape = 21, color = "black", size = 4) +
  geom_line(data = predicted_tlx, aes(x = oddball_percentage, y = Predicted_TLX), color = "black") +
  scale_fill_manual(values = scenario_colors) +
  labs(x = "Oddball Score (miss percentage)", y = "NASA-TLX Score") +
  theme_pubr() +
  theme(
    legend.position = c(0.99, 0.01),
    legend.justification = c(1, 0),
    legend.background = element_rect(color = "black", fill = "white", size = 0.4)
  ) +
  annotate("text",
           x = max(df_simu_perf$oddball_percentage, na.rm = TRUE),
           y = min(df_simu_perf$TLX_raw, na.rm = TRUE),
           label = regression_eq,
           hjust = 1,
           vjust = 0,
           size = 4)

# Save the plot
plot_file_name_tlx <- "plot_tlx_oddball.pdf"
full_plot_path_tlx <- file.path(figures_dir, plot_file_name_tlx)
ggsave(full_plot_path_tlx,
       plot = plot_tlx_oddball,
       device = "pdf",
       width = 6,
       height = 4,
       units = "in")

# Document the plot creation
if (file.exists(full_plot_path_tlx)) {
  message("Plot successfully saved: ", full_plot_path_tlx)
} else {
  stop("Plot saving failed: ", full_plot_path_tlx)
}

### Generate summary paragraph

# Create the report paragraph
report_paragraph <- paste(
  "A linear mixed-effects model was fitted to understand the relationship between the raw NASA-TLX scores and the Oddball miss percentage, including ",
  number_of_participants, " participants and a total of ", number_of_obs, " observations. ",
  "The fixed effect of Oddball miss rate on NASA-TLX scores was significant, with an estimate of ",
  sprintf("%.3f", oddball_coef), " (SE = ", sprintf("%.3f", std_error_oddball), ", t = ",
  sprintf("%.2f", t_value_oddball), ", ", p_value_formatted, "). ",
  "This indicates that higher Oddball miss rates are associated with increased NASA-TLX scores. ",
  "The intercept of the model was ", sprintf("%.3f", intercept), ".",
  sep = ""
)

# Print the report paragraph
print(report_paragraph)
