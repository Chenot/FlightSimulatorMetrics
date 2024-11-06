# Scripts/hypothesis3_composite_score_workload.R

# Hypothesis 3: Relationship between composite performance scores and workload measures

#######################################################
############### 1) Prepare data for analyses ##########
#######################################################

# Load data and utilities
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

# Define the order of the scenarios
ordered_scenarios <- c("VFR-Low", "VFR-High", "IFR-Low", "IFR-High")

##############################################################
### 2) Hypothesis 3.a: Composite score & subjective workload #
##############################################################

### Statistical analyses (descriptive, model, etc.)

# Initialize a list to store results for summary paragraph
results_tlx <- list()

for (scenario in ordered_scenarios) {
  df_scenario <- df_simu_perf %>% filter(Scenario == scenario)
  
  # Calculate correlation and linear model
  stats <- calculate_stats(df_scenario, "TLX_raw", "simu_global_perf")
  
  # Store results for summary paragraph
  results_tlx[[scenario]] <- list(
    r = stats$r,
    R2 = stats$R2,
    p_value = stats$p
  )
}

### Create plot

plots_tlx <- list()
for (scenario in ordered_scenarios) {
  df_scenario <- df_simu_perf %>% filter(Scenario == scenario)
  
  # Get stats for annotation
  stats <- results_tlx[[scenario]]
  label_text <- sprintf("r = %.2f, R² = %.3f, %s", stats$r, stats$R2, format_p_value(stats$p_value))
  
  # Determine the y position for the annotation
  y_max <- max(df_scenario$simu_global_perf, na.rm = TRUE)
  y_position <- y_max + 0.1 * (max(df_simu_perf$simu_global_perf, na.rm = TRUE) -
                                 min(df_simu_perf$simu_global_perf, na.rm = TRUE))
  
  p <- ggplot(df_scenario, aes(x = TLX_raw, y = simu_global_perf, fill = Scenario)) +
    geom_point(color = "black", shape = 21, size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    scale_fill_manual(values = scenario_colors) +
    labs(title = scenario, x = "NASA-TLX Score", y = "Composite Score") +
    theme_pubr() +
    theme(legend.position = "none") +
    annotate("text",
             x = min(df_scenario$TLX_raw, na.rm = TRUE),
             y = y_position,
             label = label_text,
             hjust = 0,
             vjust = 1,
             size = 4)
  
  plots_tlx[[scenario]] <- p
}

# Combine the plots
p_tlx <- grid.arrange(grobs = plots_tlx, ncol = 2)

# Save the plot
plot_file_name_composite_TLX <- "plot_composite_TLX.pdf"
full_plot_path_composite_TLX <- file.path(figures_dir, plot_file_name_composite_TLX)
ggsave(full_plot_path_composite_TLX,
       plot = p_tlx,
       device = "pdf",
       width = 12,
       height = 8,
       units = "in")

# Document the plot creation
if (file.exists(full_plot_path_composite_TLX)) {
  message("Plot successfully saved: ", full_plot_path_composite_TLX)
} else {
  stop("Plot saving failed: ", full_plot_path_composite_TLX)
}

### Generate summary paragraph

# Create summary paragraph for subjective workload
paragraph_tlx <- lapply(ordered_scenarios, function(scenario) {
  stats <- results_tlx[[scenario]]
  sprintf(
    "In the %s scenario, the Pearson correlation between NASA-TLX scores and composite performance scores was r = %.2f (R² = %.3f, %s).",
    scenario,
    stats$r,
    stats$R2,
    format_p_value(stats$p_value)
  )
})

# Print the summary paragraph
print(paste(paragraph_tlx, collapse = " "))

##############################################################
### 3) Hypothesis 3.b: Composite score & objective workload ##
##############################################################

### Statistical analyses (descriptive, model, etc.)

# Initialize a list to store results for summary paragraph
results_oddball <- list()

for (scenario in ordered_scenarios) {
  df_scenario <- df_simu_perf %>% filter(Scenario == scenario)
  
  # Calculate correlation and linear model
  stats <- calculate_stats(df_scenario, "oddball_percentage", "simu_global_perf")
  
  # Store results for summary paragraph
  results_oddball[[scenario]] <- list(
    r = stats$r,
    R2 = stats$R2,
    p_value = stats$p
  )
}

### Create plot

plots_oddball <- list()
for (scenario in ordered_scenarios) {
  df_scenario <- df_simu_perf %>% filter(Scenario == scenario)
  
  # Get stats for annotation
  stats <- results_oddball[[scenario]]
  label_text <- sprintf("r = %.2f, R² = %.3f, %s", stats$r, stats$R2, format_p_value(stats$p_value))
  
  # Determine the y position for the annotation
  y_max <- max(df_scenario$simu_global_perf, na.rm = TRUE)
  y_position <- y_max + 0.1 * (max(df_simu_perf$simu_global_perf, na.rm = TRUE) -
                                 min(df_simu_perf$simu_global_perf, na.rm = TRUE))
  
  p <- ggplot(df_scenario, aes(x = oddball_percentage, y = simu_global_perf, fill = Scenario)) +
    geom_point(color = "black", shape = 21, size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    scale_fill_manual(values = scenario_colors) +
    labs(title = scenario, x = "Oddball Score (miss percentage)", y = "Composite Score") +
    theme_pubr() +
    theme(legend.position = "none") +
    annotate("text",
             x = min(df_scenario$oddball_percentage, na.rm = TRUE),
             y = y_position,
             label = label_text,
             hjust = 0,
             vjust = 1,
             size = 4)
  
  plots_oddball[[scenario]] <- p
}

# Combine the plots
p_oddball <- grid.arrange(grobs = plots_oddball, ncol = 2)

# Save the plot
plot_file_name_composite_oddball <- "plot_composite_oddball.pdf"
full_plot_path_composite_oddball <- file.path(figures_dir, plot_file_name_composite_oddball)
ggsave(full_plot_path_composite_oddball,
       plot = p_oddball,
       device = "pdf",
       width = 12,
       height = 8,
       units = "in")

# Document the plot creation
if (file.exists(full_plot_path_composite_oddball)) {
  message("Plot successfully saved: ", full_plot_path_composite_oddball)
} else {
  stop("Plot saving failed: ", full_plot_path_composite_oddball)
}

### Generate summary paragraph

# Create summary paragraph for objective workload
paragraph_oddball <- lapply(ordered_scenarios, function(scenario) {
  stats <- results_oddball[[scenario]]
  sprintf(
    "In the %s scenario, the Pearson correlation between Oddball miss rates and composite performance scores was r = %.2f (R² = %.3f, %s).",
    scenario,
    stats$r,
    stats$R2,
    format_p_value(stats$p_value)
  )
})

# Print the summary paragraph
print(paste(paragraph_oddball, collapse = " "))
