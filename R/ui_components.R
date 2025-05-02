# UI Components

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Urban Mobility Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Temporal Analysis", tabName = "temporal", icon = icon("clock")),
      menuItem("Spatial Analysis", tabName = "spatial", icon = icon("map")),
      menuItem("Route Analysis", tabName = "routes", icon = icon("route")),
      menuItem("Efficiency Comparison", tabName = "efficiency", icon = icon("chart-line")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    ),
    
    # Add city selector (would be populated with real cities in production)
    selectInput("citySelect", "Select City:",
                choices = c("New York", "London", "Tokyo", "Sample Data")),
    
    # Date range selector
    dateRangeInput("dateRange", "Date Range:",
                  start = Sys.Date() - 30, end = Sys.Date()),
    
    # Add other filters as needed
    selectInput("routeType", "Transportation Type:",
                choices = c("All", "Subway", "Bus", "Rail", "Tram"),
                selected = "All")
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          infoBoxOutput("totalRidershipBox"),
          infoBoxOutput("avgDailyRidershipBox"),
          infoBoxOutput("busyHourBox")
        ),
        fluidRow(
          box(
            title = "Daily Ridership Trend",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("dailyTrendPlot", height = 250) %>% withSpinner()
          ),
          box(
            title = "Transportation Mode Share",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("modeSharePlot", height = 250) %>% withSpinner()
          )
        ),
        fluidRow(
          box(
            title = "Top 10 Busiest Stations",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("busyStationsPlot", height = 300) %>% withSpinner()
          )
        )
      ),
      
      # Temporal Analysis Tab
      tabItem(tabName = "temporal",
        fluidRow(
          box(
            title = "Hourly Ridership Pattern",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("hourlyPatternPlot", height = 300) %>% withSpinner()
          )
        ),
        fluidRow(
          box(
            title = "Weekly Ridership Pattern",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("weeklyPatternPlot", height = 300) %>% withSpinner()
          ),
          box(
            title = "Monthly Ridership Trend",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("monthlyTrendPlot", height = 300) %>% withSpinner()
          )
        ),
        fluidRow(
          box(
            title = "Peak vs. Off-Peak Analysis",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("peakAnalysisPlot", height = 300) %>% withSpinner()
          )
        )
      ),
      
      # Spatial Analysis Tab
      tabItem(tabName = "spatial",
        fluidRow(
          box(
            title = "Transit Activity Map",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            leafletOutput("activityMap", height = 600) %>% withSpinner()
          )
        ),
        # Spatial Analysis Tab (continued)
        fluidRow(
          box(
            title = "Spatial Distribution Analysis",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("spatialDistributionPlot", height = 300) %>% withSpinner()
          )
        )
      ),
      
      # Route Analysis Tab
      tabItem(tabName = "routes",
        fluidRow(
          box(
            width = 3,
            selectInput("routeSelect", "Select Route:",
                        choices = NULL)  # Will be populated in server
          ),
          box(
            width = 9,
            uiOutput("routeInfoUI")
          )
        ),
        fluidRow(
          box(
            title = "Route Map",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            leafletOutput("routeMap", height = 500) %>% withSpinner()
          ),
          box(
            title = "Route Performance",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("routePerformancePlot", height = 500) %>% withSpinner()
          )
        )
      ),
      
      # Efficiency Comparison Tab
      tabItem(tabName = "efficiency",
        fluidRow(
          box(
            title = "System Efficiency Metrics",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("efficiencyMetricsPlot", height = 300) %>% withSpinner()
          )
        ),
        fluidRow(
          box(
            title = "Route Efficiency Comparison",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("routeEfficiencyPlot", height = 400) %>% withSpinner()
          ),
          box(
            title = "Station Efficiency Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("stationEfficiencyPlot", height = 400) %>% withSpinner()
          )
        ),
        fluidRow(
          box(
            title = "Efficiency Insights",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            htmlOutput("efficiencyInsights") %>% withSpinner()
          )
        )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Data Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput("dataTable", "Select Data Table:",
                        choices = c("Stops", "Routes", "Ridership", "Trip Times"))
          )
        ),
        fluidRow(
          box(
            title = "Data Table",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("dataTableOutput") %>% withSpinner()
          )
        )
      )
    )
  )
)