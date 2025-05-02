# Server Logic

# Define server function
server <- function(input, output, session) {
  # Ensure all city data exists by creating it if it doesn't
  ensure_all_city_data()

  # Create and load sample data first
  sample_data_dir <- "data/sample"
  transit_data <- load_gtfs_data(sample_data_dir)

  # Reactive values to store selected data
  transit_data_reactive <- reactiveVal(transit_data)
  
  # Observer for city selection
  observeEvent(input$citySelect, {
    req(input$citySelect)
    
    # Show a notification that data is loading
    id <- showNotification("Loading data...", type = "message", duration = NULL)
    
    # Select the appropriate data directory
    city_dir <- if (input$citySelect == "Sample Data") {
      "data/sample"
    } else {
      paste0("data/", tolower(gsub(" ", "_", input$citySelect)))
    }
    
    # Load the data for the selected city
    tryCatch({
      new_transit_data <- load_gtfs_data(city_dir)
      transit_data_reactive(new_transit_data)
      
      # Update the map center for the selected city
      if (input$citySelect == "New York") {
        leafletProxy("activityMap") %>% setView(lng = -74.0, lat = 40.75, zoom = 11)
      } else if (input$citySelect == "London") {
        leafletProxy("activityMap") %>% setView(lng = -0.1, lat = 51.5, zoom = 11)
      } else if (input$citySelect == "Tokyo") {
        leafletProxy("activityMap") %>% setView(lng = 139.75, lat = 35.7, zoom = 11)
      }
      
    }, error = function(e) {
      showNotification(
        paste("Error loading data for", input$citySelect, ":", e$message),
        type = "error",
        duration = 10
      )
    })
    
    # Remove the loading notification
    removeNotification(id)
  })
  
  # Update route selector choices
  observe({
    routes <- transit_data_reactive()$routes
    route_choices <- setNames(
      routes$route_id, 
      paste(routes$route_short_name, "-", routes$route_long_name)
    )
    updateSelectInput(session, "routeSelect", choices = route_choices)
  })
  
  # Overview tab outputs
  output$totalRidershipBox <- renderInfoBox({
    ridership_data <- transit_data_reactive()$ridership
    total_ridership <- sum(ridership_data$entries)
    
    infoBox(
      "Total Ridership", 
      format(total_ridership, big.mark = ","),
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avgDailyRidershipBox <- renderInfoBox({
    ridership_data <- transit_data_reactive()$ridership
    daily_ridership <- ridership_data %>%
      group_by(date) %>%
      summarize(total = sum(entries)) %>%
      pull(total) %>%
      mean() %>%
      round()
    
    infoBox(
      "Avg. Daily Ridership", 
      format(daily_ridership, big.mark = ","),
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$busyHourBox <- renderInfoBox({
    ridership_data <- transit_data_reactive()$ridership
    busy_hour <- ridership_data %>%
      group_by(hour) %>%
      summarize(total = sum(entries)) %>%
      arrange(desc(total)) %>%
      head(1) %>%
      pull(hour)
    
    # Format hour for display
    busy_hour_display <- ifelse(
      busy_hour < 12,
      paste0(busy_hour, " AM"),
      ifelse(busy_hour == 12, "12 PM", paste0(busy_hour - 12, " PM"))
    )
    
    infoBox(
      "Busiest Hour", 
      busy_hour_display,
      icon = icon("clock"),
      color = "red"
    )
  })
  
  output$dailyTrendPlot <- renderPlotly({
    ridership_data <- transit_data_reactive()$ridership
    daily_trend <- ridership_data %>%
      group_by(date) %>%
      summarize(total_entries = sum(entries))
    
    p <- ggplot(daily_trend, aes(x = date, y = total_entries)) +
      geom_line(color = "#1976D2", size = 1) +
      geom_point(color = "#1976D2", size = 2) +
      labs(
        x = "Date",
        y = "Total Entries"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$modeSharePlot <- renderPlotly({
  routes_data <- transit_data_reactive()$routes
  
  # Create a simplified mode count
  mode_types <- c("0" = "Tram", "1" = "Subway", "2" = "Rail", "3" = "Bus")
  mode_counts <- table(routes_data$route_type)
  
  # Create a data frame for plotting
  mode_share <- data.frame(
    mode = mode_types[names(mode_counts)],
    count = as.numeric(mode_counts)
  )
  
  # Create a bar chart instead of a pie chart
  p <- ggplot(mode_share, aes(x = mode, y = count, fill = mode)) +
    geom_col() +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Transportation Mode Distribution",
      x = "Mode Type",
      y = "Number of Routes",
      fill = "Mode"
    ) +
    theme_minimal()
  
  ggplotly(p)
})
  
  output$busyStationsPlot <- renderPlotly({
    top_stations_data <- busiest_stops_analysis(
      transit_data_reactive()$ridership,
      transit_data_reactive()$stops
    ) %>%
      head(10)
    
    # Format data for visualization
    top_stations_data$stop_name <- factor(
      top_stations_data$stop_name,
      levels = rev(top_stations_data$stop_name)
    )
    
    # Create the plot
    p <- ggplot(top_stations_data, aes(y = stop_name)) +
      geom_col(aes(x = total_entries), fill = "#1E88E5") +
      labs(
        x = "Total Entries",
        y = NULL
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Temporal Analysis outputs
  output$hourlyPatternPlot <- renderPlotly({
    ridership_data <- transit_data_reactive()$ridership
    
    # Calculate hourly pattern
    hourly_pattern <- ridership_data %>%
      group_by(hour) %>%
      summarize(
        entries = sum(entries),
        exits = sum(exits)
      ) %>%
      tidyr::pivot_longer(
        cols = c("entries", "exits"),
        names_to = "activity_type",
        values_to = "count"
      )
    
    # Create hour labels
    hour_labels <- c(
      "12 AM", paste0(1:11, " AM"), "12 PM", paste0(1:11, " PM")
    )
    hourly_pattern$hour_label <- factor(
      hour_labels[hourly_pattern$hour + 1],
      levels = hour_labels
    )
    
    # Create the plot
    p <- ggplot(hourly_pattern, aes(x = hour_label, y = count, fill = activity_type)) +
      geom_col(position = "dodge") +
      scale_fill_manual(
        values = c("entries" = "#1E88E5", "exits" = "#FFC107"),
        labels = c("entries" = "Entries", "exits" = "Exits")
      ) +
      labs(
        x = "Hour",
        y = "Passenger Count",
        fill = "Activity Type"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p)
  })
  
  output$weeklyPatternPlot <- renderPlotly({
    ridership_data <- transit_data_reactive()$ridership
    
    # Add day of week
    ridership_data$day_of_week <- weekdays(ridership_data$date)
    ridership_data$day_of_week <- factor(
      ridership_data$day_of_week,
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    )
    
    # Calculate weekly pattern
    weekly_pattern <- ridership_data %>%
      group_by(day_of_week) %>%
      summarize(
        entries = sum(entries),
        exits = sum(exits)
      ) %>%
      tidyr::pivot_longer(
        cols = c("entries", "exits"),
        names_to = "activity_type",
        values_to = "count"
      )
    
    # Create the plot
    p <- ggplot(weekly_pattern, aes(x = day_of_week, y = count, fill = activity_type)) +
      geom_col(position = "dodge") +
      scale_fill_manual(
        values = c("entries" = "#1E88E5", "exits" = "#FFC107"),
        labels = c("entries" = "Entries", "exits" = "Exits")
      ) +
      labs(
        x = "Day of Week",
        y = "Passenger Count",
        fill = "Activity Type"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$monthlyTrendPlot <- renderPlotly({
    ridership_data <- transit_data_reactive()$ridership
    
    # Add month
    ridership_data$month <- format(ridership_data$date, "%b")
    
    # Calculate monthly pattern
    monthly_pattern <- ridership_data %>%
      group_by(month) %>%
      summarize(
        entries = sum(entries),
        exits = sum(exits)
      ) %>%
      tidyr::pivot_longer(
        cols = c("entries", "exits"),
        names_to = "activity_type",
        values_to = "count"
      )
    
    # Create the plot
    p <- ggplot(monthly_pattern, aes(x = month, y = count, fill = activity_type)) +
      geom_col(position = "dodge") +
      scale_fill_manual(
        values = c("entries" = "#1E88E5", "exits" = "#FFC107"),
        labels = c("entries" = "Entries", "exits" = "Exits")
      ) +
      labs(
        x = "Month",
        y = "Passenger Count",
        fill = "Activity Type"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$peakAnalysisPlot <- renderPlotly({
    ridership_data <- transit_data_reactive()$ridership
    
    # Define peak hours (typically 7-9 AM and 4-6 PM)
    peak_hours <- c(7, 8, 9, 16, 17, 18)
    ridership_data$peak_category <- ifelse(
      ridership_data$hour %in% peak_hours,
      "Peak",
      "Off-Peak"
    )
    
    # Calculate peak vs off-peak stats
    peak_analysis <- ridership_data %>%
      group_by(peak_category, date) %>%
      summarize(
        entries = sum(entries),
        exits = sum(exits)
      ) %>%
      group_by(peak_category) %>%
      summarize(
        avg_entries = mean(entries),
        avg_exits = mean(exits)
      ) %>%
      tidyr::pivot_longer(
        cols = c("avg_entries", "avg_exits"),
        names_to = "activity_type",
        values_to = "average"
      )
    
    # Create the plot
    p <- ggplot(peak_analysis, aes(x = peak_category, y = average, fill = activity_type)) +
      geom_col(position = "dodge") +
      scale_fill_manual(
        values = c("avg_entries" = "#1E88E5", "avg_exits" = "#FFC107"),
        labels = c("avg_entries" = "Entries", "avg_exits" = "Exits")
      ) +
      labs(
        x = "Time Period",
        y = "Average Daily Passenger Count",
        fill = "Activity Type"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Spatial Analysis outputs
  output$activityMap <- renderLeaflet({
    stops_sf <- transit_data_reactive()$stops_sf
    ridership_data <- transit_data_reactive()$ridership
    
    # Create activity map
    stops_map <- create_stops_map(stops_sf, ridership_data)
    
    return(stops_map)
  })
  
  output$spatialDistributionPlot <- renderPlotly({
    stops_data <- transit_data_reactive()$stops
    ridership_data <- transit_data_reactive()$ridership
    
    # Calculate total activity per stop
    stop_activity <- ridership_data %>%
      group_by(stop_id) %>%
      summarize(
        total_activity = sum(entries) + sum(exits)
      ) %>%
      left_join(stops_data, by = "stop_id")
    
    # Create the plot (heatmap-like visualization)
    p <- ggplot(stop_activity, aes(x = stop_lon, y = stop_lat)) +
      geom_point(aes(color = total_activity, size = total_activity)) +
      scale_color_viridis() +
      scale_size_continuous(range = c(2, 10)) +
      labs(
        x = "Longitude",
        y = "Latitude",
        color = "Total Activity",
        size = "Total Activity"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Route Analysis outputs
  output$routeInfoUI <- renderUI({
    route_id <- input$routeSelect
    
    if (is.null(route_id)) return(NULL)
    
    route_info <- transit_data_reactive()$routes %>%
      filter(route_id == !!route_id)
    
    # Route type labels
    route_types <- c(
      "0" = "Tram",
      "1" = "Subway",
      "2" = "Rail",
      "3" = "Bus"
    )
    
    route_type_label <- route_types[as.character(route_info$route_type)]
    
    # Create info boxes
    fluidRow(
      column(4, 
        tags$div(class = "info-box",
          tags$p(class = "info-box-label", "Route Name"),
          tags$p(class = "info-box-value", route_info$route_long_name)
        )
      ),
      column(4, 
        tags$div(class = "info-box",
          tags$p(class = "info-box-label", "Route Number"),
          tags$p(class = "info-box-value", route_info$route_short_name)
        )
      ),
      column(4, 
        tags$div(class = "info-box",
          tags$p(class = "info-box-label", "Transport Type"),
          tags$p(class = "info-box-value", route_type_label)
        )
      )
    )
  })
  
  output$routeMap <- renderLeaflet({
    route_id <- input$routeSelect
    
    if (is.null(route_id)) return(NULL)
    
    # Create route map
    route_map <- create_route_map(transit_data_reactive(), route_id)
    
    return(route_map)
  })
  
  output$routePerformancePlot <- renderPlotly({
    route_id <- input$routeSelect
    
    if (is.null(route_id)) return(NULL)
    
    # Get stops for the selected route
    route_stops_ids <- transit_data_reactive()$route_stops %>%
      filter(route_id == !!route_id) %>%
      arrange(stop_sequence) %>%
      pull(stop_id)
    
    # Get ridership data for those stops
    route_ridership <- transit_data_reactive()$ridership %>%
      filter(stop_id %in% route_stops_ids) %>%
      group_by(hour) %>%
      summarize(
        total_entries = sum(entries),
        total_exits = sum(exits)
      )
    
    # Create hour labels
    hour_labels <- c(
      "12 AM", paste0(1:11, " AM"), "12 PM", paste0(1:11, " PM")
    )
    route_ridership$hour_label <- factor(
      hour_labels[route_ridership$hour + 1],
      levels = hour_labels
    )
    
    # Create the plot
    p <- ggplot(route_ridership, aes(x = hour_label)) +
      geom_line(aes(y = total_entries, color = "Entries"), size = 1) +
      geom_line(aes(y = total_exits, color = "Exits"), size = 1) +
      scale_color_manual(
        values = c("Entries" = "#1E88E5", "Exits" = "#FFC107")
      ) +
      labs(
        x = "Hour",
        y = "Passenger Count",
        color = "Activity Type"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p)
  })
  
  # Efficiency Comparison outputs
  output$efficiencyMetricsPlot <- renderPlotly({
    routes_data <- transit_data_reactive()$routes
    ridership_data <- transit_data_reactive()$ridership
    stop_times_data <- transit_data_reactive()$stop_times
    route_stops_data <- transit_data_reactive()$route_stops
    
    # Define mode types
    mode_types <- c("0" = "Tram", "1" = "Subway", "2" = "Rail", "3" = "Bus")
    
    # Calculate efficiency metrics: passengers per stop
    efficiency_by_mode <- route_stops_data %>%
      left_join(routes_data, by = "route_id") %>%
      group_by(route_type) %>%
      summarize(stop_count = n_distinct(stop_id)) %>%
      left_join(
        route_stops_data %>%
          left_join(routes_data, by = "route_id") %>%
          left_join(ridership_data, by = "stop_id") %>%
          group_by(route_type) %>%
          summarize(total_passengers = sum(entries + exits, na.rm = TRUE)),
        by = "route_type"
      ) %>%
      mutate(
        passengers_per_stop = total_passengers / stop_count,
        mode = mode_types[as.character(route_type)]
      )
    
    # Create the plot
    p <- ggplot(efficiency_by_mode, aes(x = mode, y = passengers_per_stop, fill = mode)) +
      geom_col() +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = "Transportation Mode",
        y = "Passengers per Stop",
        fill = "Mode"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$routeEfficiencyPlot <- renderPlotly({
    routes_data <- transit_data_reactive()$routes
    ridership_data <- transit_data_reactive()$ridership
    route_stops_data <- transit_data_reactive()$route_stops
    
    # Calculate efficiency metrics by route
    route_efficiency <- route_stops_data %>%
      group_by(route_id) %>%
      summarize(stop_count = n_distinct(stop_id)) %>%
      left_join(
        route_stops_data %>%
          left_join(ridership_data, by = "stop_id") %>%
          group_by(route_id) %>%
          summarize(total_passengers = sum(entries + exits, na.rm = TRUE)),
        by = "route_id"
      ) %>%
      mutate(passengers_per_stop = total_passengers / stop_count) %>%
      left_join(routes_data, by = "route_id") %>%
      mutate(route_name = paste(route_short_name, "-", route_long_name)) %>%
      arrange(desc(passengers_per_stop)) %>%
      head(10)  # Top 10 most efficient routes
    
    # Create the plot
    p <- ggplot(route_efficiency, aes(x = reorder(route_name, passengers_per_stop), y = passengers_per_stop, fill = route_short_name)) +
      geom_col() +
      coord_flip() +
      labs(
        x = NULL,
        y = "Passengers per Stop",
        title = "Top 10 Most Efficient Routes"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$stationEfficiencyPlot <- renderPlotly({
    stops_data <- transit_data_reactive()$stops
    ridership_data <- transit_data_reactive()$ridership
    
    # Calculate station efficiency
    station_efficiency <- ridership_data %>%
      group_by(stop_id) %>%
      summarize(
        total_entries = sum(entries),
        total_exits = sum(exits),
        total_activity = sum(entries) + sum(exits),
        hourly_variance = var(entries + exits)
      ) %>%
      mutate(
        efficiency_score = total_activity / (hourly_variance + 1)  # Adding 1 to avoid division by zero
      ) %>%
      left_join(stops_data, by = "stop_id") %>%
      arrange(desc(efficiency_score)) %>%
      head(10)  # Top 10 most efficient stations
    
    # Create the plot
    p <- ggplot(station_efficiency, aes(x = reorder(stop_name, efficiency_score), y = efficiency_score, fill = efficiency_score)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis() +
      labs(
        x = NULL,
        y = "Efficiency Score",
        title = "Top 10 Most Efficient Stations"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$efficiencyInsights <- renderUI({
    routes_data <- transit_data_reactive()$routes
    ridership_data <- transit_data_reactive()$ridership
    route_stops_data <- transit_data_reactive()$route_stops
    
    # Calculate average efficiency by mode
    mode_efficiency <- route_stops_data %>%
      left_join(routes_data, by = "route_id") %>%
      group_by(route_type) %>%
      summarize(stop_count = n_distinct(stop_id)) %>%
      left_join(
        route_stops_data %>%
          left_join(routes_data, by = "route_id") %>%
          left_join(ridership_data, by = "stop_id") %>%
          group_by(route_type) %>%
          summarize(total_passengers = sum(entries + exits, na.rm = TRUE)),
        by = "route_type"
      ) %>%
      mutate(
        passengers_per_stop = total_passengers / stop_count,
        mode = c("0" = "Tram", "1" = "Subway", "2" = "Rail", "3" = "Bus")[as.character(route_type)]
      ) %>%
      arrange(desc(passengers_per_stop))
    
    # Generate insights
    most_efficient_mode <- mode_efficiency$mode[1]
    least_efficient_mode <- mode_efficiency$mode[nrow(mode_efficiency)]
    
    # Create HTML output
    HTML(paste0(
      "<div class='efficiency-insights'>",
      "<h4>System Efficiency Insights</h4>",
      "<p><strong>Most Efficient Mode:</strong> ", most_efficient_mode, " has the highest passenger throughput per stop, suggesting effective station placement and high utilization.</p>",
      "<p><strong>Least Efficient Mode:</strong> ", least_efficient_mode, " shows lower passenger density per stop, which may indicate opportunities for route optimization or consolidation.</p>",
      "<p><strong>Peak vs. Off-Peak:</strong> There is significant variation between peak and off-peak utilization, suggesting opportunities for demand-responsive service adjustments.</p>",
      "<p><strong>Recommendations:</strong></p>",
      "<ul>",
      "<li>Consider increasing service frequency on high-efficiency routes during peak hours</li>",
      "<li>Evaluate low-efficiency stops for potential consolidation or service changes</li>",
      "<li>Implement dynamic scheduling based on temporal demand patterns</li>",
      "</ul>",
      "</div>"
    ))
  })
  
  # Data Explorer outputs
  output$dataTableOutput <- renderDT({
    table_choice <- input$dataTable
    
    data_to_display <- switch(table_choice,
      "Stops" = transit_data_reactive()$stops,
      "Routes" = transit_data_reactive()$routes,
      "Ridership" = transit_data_reactive()$ridership %>% head(1000),  # Limit rows for performance
      "Trip Times" = transit_data_reactive()$stop_times %>% head(1000)  # Limit rows for performance
    )
    
    datatable(data_to_display, options = list(pageLength = 10))
  })
}