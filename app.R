# Main application file
# Load necessary libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(plotly)
library(sf)
library(tidyr)
library(scales)
library(viridis)
library(RColorBrewer)
library(shinyWidgets)
library(shinycssloaders)
library(igraph)
library(tidygraph)
library(ggraph)

# Source all component files
source("global.R")
source("R/data_processing.R")
source("R/visualizations.R")
source("R/network_analysis.R")
source("R/ui_components.R")
source("R/server_logic.R")

# Run the app
shinyApp(ui = ui, server = server)