library(shiny)
library(plotly) # Required for plotlyOutput

# Define UI for application
ui <- fluidPage(
  titlePanel("Airfield Patterns Performance Visualization"),
  
  # Participant and Scenario Selection
  div(
    style = "margin-bottom: 20px;",
    fluidRow(
      column(4, offset = 2,
             selectInput("participant", "Select Participant", choices = NULL) # Choices will be updated server-side
      ),
      column(4,
             selectInput("scenario", "Select Scenario", choices = NULL) # Choices will be updated server-side
      )
    )
  ),
  
  # Section 1: Flight Data
  h3("Flight Data", style = "text-align: center; margin-top: 40px; font-weight: bold;"),
  
  # Legend for Phases
  div(
    style = "text-align: center; margin-bottom: 20px;",
    tags$div(
      style = "padding: 20px; font-size: 18px; display: inline-block;",
      tags$span(style = "color: black;", "Legend: "),
      tags$span(style = "color: blue;", "Crosswind "),
      tags$span(style = "color: green;", "Downwind "),
      tags$span(style = "color: orange;", "Base leg "),
      tags$span(style = "color: red;", "Approach")
    )
  ),
  
  fluidRow(
    column(12, plotlyOutput("flightPath")),
    column(6, plotOutput("speedPlot")),
    column(6, plotOutput("altitudePlot"))
  ),
  
  # Section 2: Simulator Performance
  h3("Simulator Performance", style = "text-align: center; margin-top: 50px;font-weight: bold;"),
  fluidRow(
    column(12, plotOutput("SimuScoresPlot")),
    column(12, plotOutput("densityPlots"))
  ),
  
  # Section 3: Workload
  h3("Workload", style = "text-align: center; margin-top: 50px;font-weight: bold;"),
  fluidRow(
    column(12, plotOutput("workloadScoresPlot"))
  )
)
