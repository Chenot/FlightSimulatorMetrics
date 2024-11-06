# Scripts/composite_score_psychometrics.R

# Composite Score Psychometric Analysis

#######################################################
############### 1) Prepare data for analyses ##########
#######################################################

# Load data and utilities
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

#######################################################
####### 2) Descriptive analyses & print graph #########
#######################################################

## Descriptive statistics and psychometric analysis

# Calculate psychometric properties for 'simu_global_perf' across scenarios
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

## Create the density plot

density_plot <- ggplot(df_simu_perf, aes(x = simu_global_perf, fill = Scenario)) +
  geom_density(alpha = 0.5) +
  geom_rug() +
  scale_fill_manual(values = scenario_colors) +
  facet_wrap(~ Scenario, scales = "free") +
  theme_pubr() +
  labs(x = "Composite Score", y = "Density")

# Save the plot
plot_file_name <- "plot_density_composite_score.pdf"
full_plot_path <- file.path(figures_dir, plot_file_name)
ggsave(full_plot_path, plot = density_plot, device = "pdf", width = 8, height = 6, units = "in")

# Document the plot creation
if (file.exists(full_plot_path)) {
  message("Density plot successfully saved: ", full_plot_path)
} else {
  stop("Plot saving failed: ", full_plot_path)
}