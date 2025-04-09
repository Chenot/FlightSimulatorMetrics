# Shiny App
This is a Shiny web application that helps to visualize Pilot Performance in a human factors experiment.
The web link is available here: https://powerbrain-simulator.shinyapps.io/shiny_app/
Publication (in review) : Chenot et al., 2024. Assessing and Visualizing Pilot Performance in Airfield Patterns: A Composite Score Approach

## Usage and Dependencies
To run the app locally, you'll need a r install (version 4.2.2) and r studio (build 561), with the following packages:
- shiny
- rsconnect
- ggplot2
- ggpubr
- plotly

To run locally or online, you can create the following script in the app folder:
##########################################################
library(shiny)
library(rsconnect)
rsconnect::setAccountInfo(name='YOUR-NAME',
                          token='YOUR-TOKEN',
                          secret='YOUR-SECRET') # Connection with shinyapps website
setwd("PATH//OF//THE//APP")
runApp() # Run locally
deployApp(account = "YOUR-ACCOUNT") # Run online
##########################################################

## Data
The app uses the following data files:
- `all_pilots_resampled.csv`: contains flight data ("Time","Latitude","Longitude","Altitude","Speed") per phase (e.g. base leg, final, etc), per participant and per condition, resampled at 1 Hz.
- `simu_perf_processed.csv`: contains performance metrics ("Scenario","ldg","app","alt","spd","time","oddball","oddball_RT","oddball_RT_log","oddball_percentage","oddball_percentage_log","TLX","TLX_raw","TLX_weighted","simu_global_perf","workload_global") per participant and per condition.

## License
This project is licensed under the MIT License - see the LICENSE.txt file for details.

