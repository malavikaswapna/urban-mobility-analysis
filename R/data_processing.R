# Data processing functions

# Function to download and extract GTFS data
download_gtfs <- function(url, city_name) {
  destination_file <- paste0("data/", city_name, ".zip")
  download.file(url, destination_file)
  unzip(destination_file, exdir = paste0("data/", city_name))
  return(paste0("data/", city_name))
}

# Function to create city-specific data
create_city_data <- function(city_name) {
  # Create directory for the city
  city_dir <- paste0("data/", tolower(gsub(" ", "_", city_name)))
  dir.create(city_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Define coordinate ranges for different cities
  coord_ranges <- list(
    "New York" = list(lat_min = 40.7, lat_max = 40.8, lon_min = -74.02, lon_max = -73.95),
    "London" = list(lat_min = 51.4, lat_max = 51.6, lon_min = -0.2, lon_max = 0.0),
    "Tokyo" = list(lat_min = 35.6, lat_max = 35.8, lon_min = 139.7, lon_max = 139.9)
  )
  
  # Get coordinate range for the city
  coords <- coord_ranges[[city_name]]
  if (is.null(coords)) {
    # Default to NYC coordinates if city not in list
    coords <- list(lat_min = 40.7, lat_max = 40.8, lon_min = -74.02, lon_max = -73.95)
  }
  
  # Create sample stops data with city-specific location
  stops <- data.frame(
    stop_id = 1:100,
    stop_name = paste0(city_name, " Stop ", 1:100),
    stop_lat = runif(100, coords$lat_min, coords$lat_max),
    stop_lon = runif(100, coords$lon_min, coords$lon_max)
  )
  
  # Create sample routes with city-specific names
  routes <- data.frame(
    route_id = 1:10,
    route_short_name = paste0(substr(city_name, 1, 1), 1:10),
    route_long_name = paste0(city_name, " Route ", LETTERS[1:10]),
    route_type = sample(0:3, 10, replace = TRUE)  # 0:tram, 1:subway, 2:rail, 3:bus
  )
  
  # Create sample trips
  trips <- data.frame(
    trip_id = 1:1000,
    route_id = sample(1:10, 1000, replace = TRUE),
    service_id = sample(1:5, 1000, replace = TRUE)
  )
  
  # Create sample stop times (when vehicles arrive at stops)
  stop_times <- data.frame(
    trip_id = rep(1:1000, each = 5),
    stop_sequence = rep(1:5, 1000),
    stop_id = sample(1:100, 5000, replace = TRUE),
    arrival_time = paste0(
      sample(5:23, 5000, replace = TRUE), ":",
      sample(0:59, 5000, replace = TRUE), ":",
      sample(0:59, 5000, replace = TRUE)
    ),
    departure_time = paste0(
      sample(5:23, 5000, replace = TRUE), ":",
      sample(0:59, 5000, replace = TRUE), ":",
      sample(0:59, 5000, replace = TRUE)
    )
  )
  
  # Different ridership patterns for different cities
  ridership_multipliers <- list(
    "New York" = list(min = 200, max = 7000),
    "London" = list(min = 150, max = 6000),
    "Tokyo" = list(min = 300, max = 9000)
  )
  
  multiplier <- ridership_multipliers[[city_name]]
  if (is.null(multiplier)) {
    multiplier <- list(min = 100, max = 5000)  # Default
  }
  
  # Create sample ridership data
  current_date <- Sys.Date()
  ridership <- data.frame(
    date = rep(seq(current_date - 30, current_date, by = "day"), each = 100),
    stop_id = rep(1:100, 31),
    entries = round(runif(3100, multiplier$min, multiplier$max)),
    exits = round(runif(3100, multiplier$min, multiplier$max)),
    hour = sample(0:23, 3100, replace = TRUE)
  )
  
  # Create directories and save data
  write.csv(stops, file.path(city_dir, "stops.csv"), row.names = FALSE)
  write.csv(routes, file.path(city_dir, "routes.csv"), row.names = FALSE)
  write.csv(trips, file.path(city_dir, "trips.csv"), row.names = FALSE)
  write.csv(stop_times, file.path(city_dir, "stop_times.csv"), row.names = FALSE)
  write.csv(ridership, file.path(city_dir, "ridership.csv"), row.names = FALSE)
  
  # Return the data directory
  return(city_dir)
}

# Create sample data for testing
create_sample_data <- function() {
  dir.create("data/sample", recursive = TRUE, showWarnings = FALSE)

  # Create sample stops data
  stops <- data.frame(
    stop_id = 1:100,
    stop_name = paste0("Stop ", 1:100),
    stop_lat = runif(100, 40.7, 40.8),  # NYC latitude range
    stop_lon = runif(100, -74.02, -73.95)  # NYC longitude range
  )
  
  # Create sample routes
  routes <- data.frame(
    route_id = 1:10,
    route_short_name = paste0("R", 1:10),
    route_long_name = paste0("Route ", LETTERS[1:10]),
    route_type = sample(0:3, 10, replace = TRUE)  # 0:tram, 1:subway, 2:rail, 3:bus
  )
  
  # Create sample trips
  trips <- data.frame(
    trip_id = 1:1000,
    route_id = sample(1:10, 1000, replace = TRUE),
    service_id = sample(1:5, 1000, replace = TRUE)
  )
  
  # Create sample stop times (when vehicles arrive at stops)
  stop_times <- data.frame(
    trip_id = rep(1:1000, each = 5),
    stop_sequence = rep(1:5, 1000),
    stop_id = sample(1:100, 5000, replace = TRUE),
    arrival_time = paste0(
      sample(5:23, 5000, replace = TRUE), ":",
      sample(0:59, 5000, replace = TRUE), ":",
      sample(0:59, 5000, replace = TRUE)
    ),
    departure_time = paste0(
      sample(5:23, 5000, replace = TRUE), ":",
      sample(0:59, 5000, replace = TRUE), ":",
      sample(0:59, 5000, replace = TRUE)
    )
  )
  
  # Create sample ridership data
  current_date <- Sys.Date()
  ridership <- data.frame(
    date = rep(seq(current_date - 30, current_date, by = "day"), each = 100),
    stop_id = rep(1:100, 31),
    entries = round(runif(3100, 100, 5000)),
    exits = round(runif(3100, 100, 5000)),
    hour = sample(0:23, 3100, replace = TRUE)
  )
  
  # Create directories and save data
  dir.create("data/sample", showWarnings = FALSE)
  write.csv(stops, "data/sample/stops.csv", row.names = FALSE)
  write.csv(routes, "data/sample/routes.csv", row.names = FALSE)
  write.csv(trips, "data/sample/trips.csv", row.names = FALSE)
  write.csv(stop_times, "data/sample/stop_times.csv", row.names = FALSE)
  write.csv(ridership, "data/sample/ridership.csv", row.names = FALSE)
  
  # Return the data directory
  return("data/sample")
}

# Check if data directory has all required files
check_data_directory <- function(data_dir) {
  required_files <- c("stops.csv", "routes.csv", "trips.csv", "stop_times.csv", "ridership.csv")
  
  # Check if directory exists
  if (!dir.exists(data_dir)) {
    stop(paste("Directory", data_dir, "does not exist"))
  }
  
  # Check if all required files exist
  missing_files <- setdiff(required_files, list.files(data_dir))
  if (length(missing_files) > 0) {
    stop(paste("Missing required files in", data_dir, ":", paste(missing_files, collapse = ", ")))
  }
  
  return(TRUE)
}

# Function to load and process GTFS data
load_gtfs_data <- function(data_dir) {
  # Check if data directory is valid
  check_data_directory(data_dir)
  
  # Load GTFS files
  stops <- read.csv(file.path(data_dir, "stops.csv"))
  routes <- read.csv(file.path(data_dir, "routes.csv"))
  trips <- read.csv(file.path(data_dir, "trips.csv"))
  stop_times <- read.csv(file.path(data_dir, "stop_times.csv"))
  ridership <- read.csv(file.path(data_dir, "ridership.csv"))
  
  # Process dates
  ridership$date <- as.Date(ridership$date)
  
  # Process times
  process_time <- function(time_str) {
    parts <- strsplit(time_str, ":")[[1]]
    hours <- as.numeric(parts[1])
    minutes <- as.numeric(parts[2])
    seconds <- as.numeric(parts[3])
    return(hours * 3600 + minutes * 60 + seconds)
  }
  
  # Apply time processing (vectorized for efficiency)
  stop_times$arrival_seconds <- sapply(stop_times$arrival_time, process_time)
  stop_times$departure_seconds <- sapply(stop_times$departure_time, process_time)
  
  # Create route-stop mapping
  route_stops <- stop_times %>%
    left_join(trips, by = "trip_id") %>%
    select(route_id, stop_id, stop_sequence) %>%
    distinct()
  
  # Create spatial stops object
  stops_sf <- sf::st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = 4326)
  
  # Return processed data
  return(list(
    stops = stops,
    stops_sf = stops_sf,
    routes = routes,
    trips = trips,
    stop_times = stop_times,
    ridership = ridership,
    route_stops = route_stops
  ))
}

# Analysis: Peak Hours
peak_hours_analysis <- function(ridership_data) {
  hourly_ridership <- ridership_data %>%
    group_by(hour) %>%
    summarize(
      total_entries = sum(entries),
      total_exits = sum(exits),
      total_activity = sum(entries) + sum(exits)
    ) %>%
    arrange(desc(total_activity))
  
  # Return the top peak hours
  return(hourly_ridership)
}

# Analysis: Busiest Stops
busiest_stops_analysis <- function(ridership_data, stops_data) {
  stop_activity <- ridership_data %>%
    group_by(stop_id) %>%
    summarize(
      total_entries = sum(entries),
      total_exits = sum(exits),
      total_activity = sum(entries) + sum(exits)
    ) %>%
    arrange(desc(total_activity)) %>%
    left_join(stops_data, by = "stop_id") %>%
    select(stop_id, stop_name, total_entries, total_exits, total_activity)
  
  return(stop_activity)
}

# Analysis: Route Popularity
route_popularity_analysis <- function(transit_data) {
  # Join ridership data with route information
  route_activity <- transit_data$ridership %>%
    left_join(transit_data$route_stops, by = "stop_id", relationship = "many-to-many") %>%
    filter(!is.na(route_id)) %>%
    group_by(route_id) %>%
    summarize(total_activity = sum(entries) + sum(exits)) %>%
    arrange(desc(total_activity)) %>%
    left_join(transit_data$routes, by = "route_id") %>%
    select(route_id, route_short_name, route_long_name, total_activity)
  
  return(route_activity)
}

# Scheduled data update function
scheduled_data_update <- function(config_file = "config.json") {
  # Load configuration
  config <- jsonlite::read_json(config_file)
  
  # Get current data sources
  data_sources <- list.dirs(config$data_directory, full.names = FALSE, recursive = FALSE)
  
  # For each data source
  for (source in data_sources) {
    # Check if data is too old
    last_modified <- file.info(file.path(config$data_directory, source))$mtime
    data_age <- as.numeric(difftime(Sys.time(), last_modified, units = "days"))
    
    if (data_age > config$max_data_age) {
      # In a real application, this would fetch new data
      message(paste("Data for", source, "is", round(data_age), "days old. Updating..."))
      
      # For this example, we recreate the appropriate data
      if (source == "sample") {
        create_sample_data()
        message("Sample data recreated.")
      } else if (source == "new_york") {
        create_city_data("New York")
        message("New York data recreated.")
      } else if (source == "london") {
        create_city_data("London")
        message("London data recreated.")
      } else if (source == "tokyo") {
        create_city_data("Tokyo")
        message("Tokyo data recreated.")
      }
    }
  }
}

# Function to ensure all city data exists
ensure_all_city_data <- function() {
  # List of cities to ensure we have data for
  cities <- c("New York", "London", "Tokyo")
  
  # Create data for each city if it doesn't exist
  for (city in cities) {
    city_dir <- paste0("data/", tolower(gsub(" ", "_", city)))
    if (!dir.exists(city_dir) || length(list.files(city_dir)) == 0) {
      message(paste("Creating data for", city))
      create_city_data(city)
    }
  }
  
  # Make sure sample data exists too
  if (!dir.exists("data/sample") || length(list.files("data/sample")) == 0) {
    message("Creating sample data")
    create_sample_data()
  }
}