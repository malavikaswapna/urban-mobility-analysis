# Network analysis functions

# Create network representation of transit system
create_transit_network <- function(transit_data) {
  # Extract edges (connections between stops)
  route_connections <- transit_data$stop_times %>%
    arrange(trip_id, stop_sequence) %>%
    group_by(trip_id) %>%
    mutate(next_stop_id = lead(stop_id)) %>%
    filter(!is.na(next_stop_id)) %>%
    select(from = stop_id, to = next_stop_id) %>%
    distinct()
  
  # Create network graph
  transit_graph <- graph_from_data_frame(
    d = route_connections,
    vertices = transit_data$stops,
    directed = TRUE
  )
  
  return(transit_graph)
}

# Calculate network centrality measures
calculate_network_metrics <- function(transit_graph) {
  # Add centrality measures to stops
  V(transit_graph)$degree <- degree(transit_graph)
  V(transit_graph)$betweenness <- betweenness(transit_graph)
  V(transit_graph)$closeness <- closeness(transit_graph)
  
  # Extract node attributes as data frame
  network_metrics <- data.frame(
    stop_id = V(transit_graph)$stop_id,
    stop_name = V(transit_graph)$stop_name,
    degree = V(transit_graph)$degree,
    betweenness = V(transit_graph)$betweenness,
    closeness = V(transit_graph)$closeness
  )
  
  return(network_metrics)
}

# Identify critical connection points in the network
identify_critical_nodes <- function(transit_graph, network_metrics) {
  # Sort by betweenness centrality (high values = critical transfer points)
  critical_nodes <- network_metrics %>%
    arrange(desc(betweenness)) %>%
    head(10)
  
  return(critical_nodes)
}

# Visualize the transit network
visualize_transit_network <- function(transit_graph, network_metrics) {
  # Convert to tidygraph object
  tidy_graph <- as_tbl_graph(transit_graph)
  
  # Add metrics to graph
  tidy_graph <- tidy_graph %>%
    activate(nodes) %>%
    left_join(network_metrics, by = c("stop_id" = "stop_id"))
  
  # Create network visualization
  p <- ggraph(tidy_graph, layout = "fr") +
    geom_edge_link(alpha = 0.2) +
    geom_node_point(aes(size = betweenness, color = degree), alpha = 0.8) +
    scale_size_continuous(range = c(1, 10)) +
    scale_color_viridis() +
    theme_graph() +
    labs(
      title = "Transit Network Analysis",
      subtitle = "Node size = Betweenness, Color = Degree Centrality"
    )
  
  return(p)
}

# Create Origin-Destination (OD) matrix from ridership data
create_od_matrix <- function(transit_data) {
  # In real applications, this would use actual OD data
  # For this example, we'll create a synthetic OD matrix
  stops <- transit_data$stops
  n_stops <- nrow(stops)
  
  # Create empty matrix
  od_matrix <- matrix(0, nrow = n_stops, ncol = n_stops)
  rownames(od_matrix) <- stops$stop_id
  colnames(od_matrix) <- stops$stop_id
  
  # Fill with synthetic data (in real application, use actual journey data)
  # Here we use a simple gravity model based on stop activity
  ridership_by_stop <- transit_data$ridership %>%
    group_by(stop_id) %>%
    summarize(
      total_entries = sum(entries),
      total_exits = sum(exits)
    )
  
  # For each pair of stops
  for (i in 1:n_stops) {
    for (j in 1:n_stops) {
      if (i != j) {
        # Find ridership values
        origin_entries <- ridership_by_stop$total_entries[ridership_by_stop$stop_id == stops$stop_id[i]]
        dest_exits <- ridership_by_stop$total_exits[ridership_by_stop$stop_id == stops$stop_id[j]]
        
        # Gravity model: OD flow proportional to origin entries * destination exits
        # and inversely proportional to distance
        if (length(origin_entries) > 0 && length(dest_exits) > 0) {
          dist <- sqrt((stops$stop_lon[i] - stops$stop_lon[j])^2 + 
                        (stops$stop_lat[i] - stops$stop_lat[j])^2)
          
          # Prevent division by zero
          if (dist == 0) dist <- 0.0001
          
          # Calculate flow
          flow <- (origin_entries * dest_exits) / (dist * 100)
          
          # Add to matrix
          od_matrix[i, j] <- flow
        }
      }
    }
  }
  
  return(od_matrix)
}

# Analyze travel patterns from OD matrix
analyze_travel_patterns <- function(od_matrix, stops_data) {
  # Convert matrix to data frame for analysis
  od_df <- as.data.frame(od_matrix)
  od_df$origin_id <- rownames(od_df)
  
  od_long <- od_df %>%
    pivot_longer(
      cols = -origin_id,
      names_to = "destination_id",
      values_to = "flow"
    ) %>%
    filter(origin_id != destination_id) %>%  # Remove same-stop flows
    arrange(desc(flow))
  
  # Add stop names
  od_long$origin_name <- stops_data$stop_name[match(od_long$origin_id, stops_data$stop_id)]
  od_long$destination_name <- stops_data$stop_name[match(od_long$destination_id, stops_data$stop_id)]
  
  # Get top flows
  top_flows <- head(od_long, 20)
  
  return(list(
    od_long = od_long,
    top_flows = top_flows
  ))
}

# Analyze transit performance by time of day
analyze_time_performance <- function(transit_data) {
  # Calculate average travel times by hour
  stop_times <- transit_data$stop_times
  
  # Calculate trip durations
  trip_durations <- stop_times %>%
    group_by(trip_id) %>%
    summarize(
      start_time = min(arrival_seconds),
      end_time = max(departure_seconds),
      duration = max(departure_seconds) - min(arrival_seconds),
      start_hour = floor(start_time / 3600) %% 24  # Get hour of day
    )
  
  # Average duration by hour
  hourly_performance <- trip_durations %>%
    group_by(start_hour) %>%
    summarize(
      avg_duration = mean(duration) / 60,  # Convert to minutes
      trips_count = n()
    )
  
  # Create hour labels
  hour_labels <- c(
    "12 AM", paste0(1:11, " AM"), "12 PM", paste0(1:11, " PM")
  )
  hourly_performance$hour_label <- factor(
    hour_labels[hourly_performance$start_hour + 1],
    levels = hour_labels
  )
  
  return(hourly_performance)
}