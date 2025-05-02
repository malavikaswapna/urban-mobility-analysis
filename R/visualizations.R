# Visualization functions

# Peak Hours Visualization
create_peak_hours_plot <- function(hourly_data) {
  # Create a data frame with hour categories
  hour_labels <- c(
    "12am", "1am", "2am", "3am", "4am", "5am", "6am", "7am", "8am", "9am", "10am", "11am",
    "12pm", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm", "9pm", "10pm", "11pm"
  )
  
  hourly_data$hour_label <- factor(hour_labels[hourly_data$hour + 1], levels = hour_labels)
  
  # Create the plot
  p <- ggplot(hourly_data, aes(x = hour_label)) +
    geom_col(aes(y = total_entries, fill = "Entries"), alpha = 0.7) +
    geom_col(aes(y = total_exits, fill = "Exits"), alpha = 0.7) +
    scale_fill_manual(values = c("Entries" = "#1E88E5", "Exits" = "#FFC107")) +
    labs(
      title = "Transit Activity by Hour of Day",
      x = "Hour",
      y = "Number of Passengers",
      fill = "Activity Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  return(p)
}

# Weekly Pattern Visualization
create_weekly_pattern_plot <- function(ridership_data) {
  # Aggregate by day of week
  ridership_data$day_of_week <- weekdays(ridership_data$date)
  ridership_data$day_of_week <- factor(
    ridership_data$day_of_week,
    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  )
  
  weekly_pattern <- ridership_data %>%
    group_by(day_of_week) %>%
    summarize(
      total_entries = sum(entries),
      total_exits = sum(exits),
      total_activity = sum(entries) + sum(exits)
    )
  
  # Create the plot
  p <- ggplot(weekly_pattern, aes(x = day_of_week, y = total_activity)) +
    geom_col(fill = "#2E7D32", alpha = 0.8) +
    labs(
      title = "Transit Activity by Day of Week",
      x = "Day of Week",
      y = "Total Passenger Activity"
    ) +
    theme_minimal()
  
  return(p)
}

# Top Stations Visualization
create_top_stations_plot <- function(busiest_stops, n = 10) {
  top_n_stops <- head(busiest_stops, n)
  top_n_stops$stop_name <- factor(top_n_stops$stop_name, levels = rev(top_n_stops$stop_name))
  
  p <- ggplot(top_n_stops, aes(y = stop_name)) +
    geom_col(aes(x = total_entries, fill = "Entries"), alpha = 0.7) +
    geom_col(aes(x = total_exits, fill = "Exits"), alpha = 0.7) +
    scale_fill_manual(values = c("Entries" = "#1E88E5", "Exits" = "#FFC107")) +
    labs(
      title = paste("Top", n, "Busiest Transit Stops"),
      x = "Passenger Count",
      y = NULL,
      fill = "Activity Type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

# Create a stops map
create_stops_map <- function(stops_sf, ridership_data) {
  # Calculate total activity per stop
  stop_activity <- ridership_data %>%
    group_by(stop_id) %>%
    summarize(
      total_entries = sum(entries),
      total_exits = sum(exits),
      total_activity = sum(entries) + sum(exits)
    )
  
  # Join with spatial data
  stops_with_activity <- stops_sf %>%
    left_join(stop_activity, by = "stop_id")
  
  # Create a leaflet map
  map <- leaflet(stops_with_activity) %>%
    addTiles() %>%
    addCircleMarkers(
      radius = ~sqrt(total_activity) / 50,
      color = "#E53935",
      fillColor = "#E53935",
      fillOpacity = 0.7,
      weight = 1,
      popup = ~paste0(
        "<strong>", stop_name, "</strong><br>",
        "Total Entries: ", format(total_entries, big.mark = ","), "<br>",
        "Total Exits: ", format(total_exits, big.mark = ","), "<br>",
        "Total Activity: ", format(total_activity, big.mark = ",")
      )
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("#E53935"),
      labels = c("Transit Stop Activity"),
      opacity = 0.7
    )
  
  return(map)
}

# Create a route visualization
create_route_map <- function(transit_data, route_id) {
  # Error handling
  tryCatch({
    # Get stops for the selected route
    route_stops_ids <- transit_data$route_stops %>%
      filter(route_id == !!route_id) %>%
      arrange(stop_sequence) %>%
      pull(stop_id)
    
    if(length(route_stops_ids) == 0) {
      return(leaflet() %>% 
               addTiles() %>%
               addControl("No stops found for this route", position = "topright"))
    }
    
    # Get spatial data for those stops
    route_stops_sf <- transit_data$stops_sf %>%
      filter(stop_id %in% route_stops_ids)
    
    # Create a simple map without polylines to avoid the metaData error
    map <- leaflet(route_stops_sf) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 5,
        color = "#1976D2",
        fillColor = "#1976D2",
        fillOpacity = 0.7,
        weight = 1,
        popup = ~paste0("<strong>", stop_name, "</strong>")
      )
    
    return(map)
  }, error = function(e) {
    # Return a default map in case of error
    leaflet() %>%
      addTiles() %>%
      addControl(paste("Error creating route map:", e$message), position = "topright")
  })
}

# Visualize top OD pairs
visualize_od_flows <- function(top_flows, stops_data) {
  # Create network representation of top flows
  nodes <- data.frame(
    id = unique(c(top_flows$origin_id, top_flows$destination_id)),
    stringsAsFactors = FALSE
  )
  nodes$label <- stops_data$stop_name[match(nodes$id, stops_data$stop_id)]
  
  edges <- data.frame(
    from = top_flows$origin_id,
    to = top_flows$destination_id,
    width = top_flows$flow / max(top_flows$flow) * 10,
    label = round(top_flows$flow, 1),
    stringsAsFactors = FALSE
  )
  
  # Create network graph
  flow_graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  
  # Visualize
  p <- ggraph(flow_graph, layout = "fr") +
    geom_edge_link(aes(width = width, alpha = width), 
                  arrow = arrow(length = unit(2, 'mm')), 
                  end_cap = circle(3, 'mm')) +
    geom_node_point(size = 5, color = "#1976D2") +
    geom_node_text(aes(label = label), repel = TRUE) +
    scale_edge_width(range = c(0.5, 3)) +
    scale_edge_alpha(range = c(0.3, 1)) +
    theme_graph() +
    labs(
      title = "Top Origin-Destination Flows",
      subtitle = "Width = Passenger Flow Volume"
    )
  
  return(p)
}

# Visualize time performance
visualize_time_performance <- function(hourly_performance) {
  p <- ggplot(hourly_performance, aes(x = hour_label)) +
    geom_col(aes(y = avg_duration), fill = "#1976D2") +
    geom_line(aes(y = trips_count / 5, group = 1), color = "#E53935", size = 1) +  # Scaled for dual y-axis
    scale_y_continuous(
      name = "Average Trip Duration (minutes)",
      sec.axis = sec_axis(~. * 5, name = "Number of Trips")
    ) +
    labs(
      title = "Transit Performance by Time of Day",
      x = "Hour"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y.left = element_text(color = "#1976D2"),
      axis.title.y.right = element_text(color = "#E53935")
    )
  
  return(p)
}