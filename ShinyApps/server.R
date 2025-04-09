# Load necessary packages
library(shiny)
library(ggplot2)
library(ggpubr)
library(plotly)

# Define server logic
server <- function(input, output, session) {
  # Load data
  df_simu_perf <- read.csv("simu_perf_processed.csv", sep = ',')
  df_filtered <- read.csv("all_pilots_resampled.csv", sep = ',')
  
  # Use updateSelectInput to populate the choices for selectInput widgets
  updateSelectInput(session, "participant", choices = unique(df_filtered$Participant))
  updateSelectInput(session, "scenario", choices = unique(df_filtered$Scenario))
  
  # 3D Flight Path Plot
  output$flightPath <- renderPlotly({
    filtered_data <- subset(df_filtered, Participant == input$participant & Scenario == input$scenario)
    
    # Define the custom color order
    custom_colors <- c(
      "start" = "black",
      "turn 1" = "black",
      "crosswind" = "blue",
      "turn 2" = "black",
      "downwind" = "green",
      "turn 3" = "black",
      "base leg" = "orange",
      "turn 4" = "black",
      "approach" = "red"
    )
    
    # 3D plot for the flight trajectory with specific colors
    p <- plot_ly(filtered_data, x = ~Longitude, y = ~Latitude, z = ~Altitude, color = ~phase, colors = custom_colors, type = "scatter3d", mode = "lines", showlegend = FALSE) %>%
      layout(
        title = "3D Flight Path",
        scene = list(
          aspectmode = 'data', 
          xaxis = list(visible = FALSE), 
          yaxis = list(visible = FALSE), 
          zaxis = list(visible = FALSE)
        )
      ) # Hide axes and add title
    
    # Add 3D plot for the ground trajectory (dashed line)
    p <- add_trace(p, x = ~Longitude, y = ~Latitude, z = rep(155.448, nrow(filtered_data)), type = "scatter3d", mode = "lines", line = list(color = 'black', dash = 'dash'))
    
    p
  })
  
  # Speed and Altitude Plots with ggpubr theme
  output$speedPlot <- renderPlot({
    filtered_data <- subset(df_filtered, Participant == input$participant & Scenario == input$scenario)
    
    # Define the custom color order
    custom_colors <- c(
      "start" = "black",
      "turn 1" = "black",
      "crosswind" = "blue",
      "turn 2" = "black",
      "downwind" = "green",
      "turn 3" = "black",
      "base leg" = "orange",
      "turn 4" = "black",
      "approach" = "red"
    )
    
    ggplot(filtered_data, aes(x = Time, y = Speed, color = phase)) + 
      geom_line() + 
      scale_color_manual(values = custom_colors, breaks = names(custom_colors)) +
      labs(title = "Speed Profile", x = "Time", y = "Speed") +
      theme_pubr() +
      theme(legend.position = "none") # Optionally remove the legend
  })
  
  output$altitudePlot <- renderPlot({
    filtered_data <- subset(df_filtered, Participant == input$participant & Scenario == input$scenario)
    
    # Define the custom color order
    custom_colors <- c(
      "start" = "black",
      "turn 1" = "black",
      "crosswind" = "blue",
      "turn 2" = "black",
      "downwind" = "green",
      "turn 3" = "black",
      "base leg" = "orange",
      "turn 4" = "black",
      "approach" = "red"
    )
    
    ggplot(filtered_data, aes(x = Time, y = Altitude, color = phase)) + 
      geom_line() + 
      scale_color_manual(values = custom_colors, breaks = names(custom_colors)) +
      labs(title = "Altitude Profile", x = "Time", y = "Altitude") +
      theme_pubr() +
      theme(legend.position = "none") # Optionally remove the legend
  })
  
  
  
  # Function to calculate percentile
  calculate_percentile <- function(score, df, var) {
    percentile <- pnorm(score, mean(df[[var]], na.rm = TRUE), sd(df[[var]], na.rm = TRUE)) * 100
    return(percentile)
  }
  
  output$SimuScoresPlot <- renderPlot({
    # Filter both by Participant and Scenario
    selected_data <- subset(df_simu_perf, Participant == input$participant & Scenario == input$scenario)
    score_vars <- c("simu_global_perf")
    
    # Also, when calculating the percentile, make sure to filter df_simu_perf by the selected scenario
    filtered_df_simu_perf <- subset(df_simu_perf, Scenario == input$scenario) # Filter by Scenario
    
    plots <- lapply(score_vars, function(var) {
      selected_score <- selected_data[[var]][1]
      percentile <- calculate_percentile(selected_score, filtered_df_simu_perf, var)
      plot_title <- paste("Score superior to", round(percentile, 2), "% of sample")
      ggplot(filtered_df_simu_perf, aes_string(x = var)) + 
        geom_density(fill = "grey", alpha = 0.3) + 
        geom_vline(xintercept = selected_score, color = "red") + 
        geom_rug(aes(y = 0), sides = "b") +
        labs(title = plot_title, x = "Composite Score") +
        theme_pubr() +
        theme(aspect.ratio = 1) # Ensuring square plots
    })
    ggarrange(plotlist = plots, ncol = 1, nrow = 1, common.legend = TRUE, legend = "bottom")
  })
  
  
  output$densityPlots <- renderPlot({
    selected_data <- subset(df_simu_perf, Participant == input$participant & Scenario == input$scenario)
    score_vars <- c("ldg", "app", "alt", "spd", "time")
    x_labels <- c("Landing Metric", "Approach Metric", "Altitude Metric", "Speed Metric", "Time Metric")
    
    # Filter df_simu_perf by Scenario for calculating percentiles
    filtered_df_simu_perf <- subset(df_simu_perf, Scenario == input$scenario) # Filter by Scenario
    
    plots <- lapply(seq_along(score_vars), function(i) {
      var <- score_vars[i]
      selected_score <- selected_data[[var]][1]
      percentile <- calculate_percentile(selected_score, filtered_df_simu_perf, var)
      plot_title <- paste("Score superior to", round(percentile, 2), "% of sample")
      ggplot(filtered_df_simu_perf, aes_string(x = var)) + 
        geom_density(fill = "grey", alpha = 0.3) + 
        geom_vline(xintercept = selected_score, color = "red") + 
        geom_rug(aes(y = 0), sides = "b") +
        labs(title = plot_title, x = x_labels[i]) +
        theme_pubr() +
        theme(aspect.ratio = 1) # Ensuring square plots
    })
    
    ggarrange(plotlist = plots, ncol = 5, nrow = 1, common.legend = TRUE, legend = "bottom")
  })
  
  # Workload Scores Plot with fixed aspect ratio
  output$workloadScoresPlot <- renderPlot({
    selected_data <- subset(df_simu_perf, Participant == input$participant & Scenario == input$scenario)
    global_score_vars <- c("oddball_percentage", "TLX_raw")
    x_labels <- c("Objective workload (Oddball miss percentage)", "Subjective Workload (TLX score)")
    
    # Filter df_simu_perf by Scenario for calculating percentiles
    filtered_df_simu_perf <- subset(df_simu_perf, Scenario == input$scenario) # Filter by Scenario
    
    plots <- lapply(seq_along(global_score_vars), function(i) {
      var <- global_score_vars[i]
      selected_score <- selected_data[[var]][1]
      percentile <- calculate_percentile(selected_score, filtered_df_simu_perf, var)
      plot_title <- paste("Score superior to", round(percentile, 2), "% of sample")
      ggplot(filtered_df_simu_perf, aes_string(x = var)) + 
        geom_density(fill = "grey", alpha = 0.3) + 
        geom_vline(xintercept = selected_score, color = "red") + 
        geom_rug(aes(y = 0), sides = "b") +
        labs(title = plot_title, x = x_labels[i]) +
        theme_pubr() +
        theme(aspect.ratio = 1) # Ensuring square plots
    })
    ggarrange(plotlist = plots, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
  })
}