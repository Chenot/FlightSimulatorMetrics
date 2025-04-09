# Scripts/composite_score_psychometrics.R

# Composite Score Psychometric Analysis

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


#######################################################
####### 3) Flight experience, workload & performance ###
#######################################################

# Add flight hours to df_simu_perf
df_simu_perf <- df_simu_perf %>% 
  left_join(df_final %>% 
              select(Participant = Identifiant, HeuresVol),
            by = "Participant")

### Create plots for workload measures

# Initialize lists for plots
plots_tlx_hours <- list()
plots_oddball_hours <- list()

for (scenario in levels(df_simu_perf$Scenario)) {
  df_scenario <- df_simu_perf %>% filter(Scenario == scenario)
  
  # TLX correlation and plot
  tlx_cor <- cor.test(df_scenario$TLX_raw, df_scenario$HeuresVol)
  tlx_label <- sprintf("r = %.2f [%.2f, %.2f], %s",
                      tlx_cor$estimate,
                      tlx_cor$conf.int[1],
                      tlx_cor$conf.int[2],
                      format_p_value(tlx_cor$p.value))
  
  # Calculate y-axis limits based on data
  y_min <- min(df_scenario$TLX_raw, na.rm = TRUE)
  y_max <- max(df_scenario$TLX_raw, na.rm = TRUE)
  y_range <- y_max - y_min
  text_y_pos <- y_max + 0.1 * y_range
  
  p_tlx <- ggplot(df_scenario, aes(x = HeuresVol, y = TLX_raw, fill = Scenario)) +
    geom_point(color = "black", shape = 21, size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "black", fill = scenario_colors[scenario]) +
    scale_fill_manual(values = scenario_colors) +
    labs(title = scenario,
         x = "Flight Hours",
         y = "NASA-TLX Score") +
    theme_pubr() +
    theme(legend.position = "none") +
    annotate("text",
             x = min(df_scenario$HeuresVol, na.rm = TRUE),
             y = text_y_pos,
             label = tlx_label,
             hjust = 0,
             vjust = 1,
             size = 4) +
    ylim(y_min - 0.1 * y_range, y_max + 0.2 * y_range)  # Add padding for text
  plots_tlx_hours[[scenario]] <- p_tlx
  
  # Oddball correlation and plot
  oddball_cor <- cor.test(df_scenario$oddball_percentage, df_scenario$HeuresVol)
  oddball_label <- sprintf("r = %.2f [%.2f, %.2f], %s",
                          oddball_cor$estimate,
                          oddball_cor$conf.int[1],
                          oddball_cor$conf.int[2],
                          format_p_value(oddball_cor$p.value))
  
  # Calculate y-axis limits based on data
  y_min <- min(df_scenario$oddball_percentage, na.rm = TRUE)
  y_max <- max(df_scenario$oddball_percentage, na.rm = TRUE)
  y_range <- y_max - y_min
  text_y_pos <- y_max + 0.1 * y_range
  
  p_oddball <- ggplot(df_scenario, aes(x = HeuresVol, y = oddball_percentage, fill = Scenario)) +
    geom_point(color = "black", shape = 21, size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "black", fill = scenario_colors[scenario]) +
    scale_fill_manual(values = scenario_colors) +
    labs(title = scenario,
         x = "Flight Hours",
         y = "Oddball Score (miss percentage)") +
    theme_pubr() +
    theme(legend.position = "none") +
    annotate("text",
             x = min(df_scenario$HeuresVol, na.rm = TRUE),
             y = text_y_pos,
             label = oddball_label,
             hjust = 0,
             vjust = 1,
             size = 4) +
    ylim(y_min - 0.1 * y_range, y_max + 0.2 * y_range)  # Add padding for text
  plots_oddball_hours[[scenario]] <- p_oddball
}

### Create plots for composite score
plots_composite_hours <- list()

for (scenario in levels(df_simu_perf$Scenario)) {
  df_scenario <- df_simu_perf %>% filter(Scenario == scenario)
  
  # Composite score correlation and plot
  composite_cor <- cor.test(df_scenario$simu_global_perf, df_scenario$HeuresVol)
  composite_label <- sprintf("r = %.2f [%.2f, %.2f], %s",
                           composite_cor$estimate,
                           composite_cor$conf.int[1],
                           composite_cor$conf.int[2],
                           format_p_value(composite_cor$p.value))
  
  # Calculate y-axis limits based on data
  y_min <- min(df_scenario$simu_global_perf, na.rm = TRUE)
  y_max <- max(df_scenario$simu_global_perf, na.rm = TRUE)
  y_range <- y_max - y_min
  text_y_pos <- y_max + 0.1 * y_range
  
  p_composite <- ggplot(df_scenario, aes(x = HeuresVol, y = simu_global_perf, fill = Scenario)) +
    geom_point(color = "black", shape = 21, size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "black", fill = scenario_colors[scenario]) +
    scale_fill_manual(values = scenario_colors) +
    labs(title = scenario,
         x = "Flight Hours",
         y = "Composite Score") +
    theme_pubr() +
    theme(legend.position = "none") +
    annotate("text",
             x = min(df_scenario$HeuresVol, na.rm = TRUE),
             y = text_y_pos,
             label = composite_label,
             hjust = 0,
             vjust = 1,
             size = 4) +
    ylim(y_min - 0.1 * y_range, y_max + 0.2 * y_range)  # Add padding for text
  plots_composite_hours[[scenario]] <- p_composite
}

# Create ordered list of plots for each measure to ensure correct grid position
ordered_scenarios <- c("VFR-Low", "VFR-High", "IFR-Low", "IFR-High")

# Arrange TLX plots in 2x2 grid
p_tlx_hours <- grid.arrange(
  grobs = plots_tlx_hours[ordered_scenarios],
  ncol = 2,
  nrow = 2,
  #top = "Correlations between Flight Hours and NASA-TLX Scores"
)

# Save TLX plots
ggsave(
  file.path(figures_dir, "plot_tlx_hours.pdf"),
  plot = p_tlx_hours,
  device = "pdf",
  width = 10,
  height = 8,
  units = "in"
)

# Arrange Oddball plots in 2x2 grid
p_oddball_hours <- grid.arrange(
  grobs = plots_oddball_hours[ordered_scenarios],
  ncol = 2,
  nrow = 2,
  #top = "Correlations between Flight Hours and Oddball Scores"
)

# Save Oddball plots
ggsave(
  file.path(figures_dir, "plot_oddball_hours.pdf"),
  plot = p_oddball_hours,
  device = "pdf",
  width = 10,
  height = 8,
  units = "in"
)

# Arrange Composite score plots in 2x2 grid
p_composite_hours <- grid.arrange(
  grobs = plots_composite_hours[ordered_scenarios],
  ncol = 2,
  nrow = 2,
  #top = "Correlations between Flight Hours and Composite Scores"
)

# Save Composite score plots
ggsave(
  file.path(figures_dir, "plot_composite_hours.pdf"),
  plot = p_composite_hours,
  device = "pdf",
  width = 10,
  height = 8,
  units = "in"
)
