# Global constants and configuration
COLOR_PALETTE <- list(
  primary = "#1976D2",
  secondary = "#FFC107",
  success = "#2E7D32",
  warning = "#FF9800",
  danger = "#E53935"
)

# Load configuration
config <- if(file.exists("config.json")) {
  jsonlite::read_json("config.json")
} else {
  list(
    app_title = "Urban Mobility Analysis",
    data_directory = "data/",
    refresh_interval = 86400,
    max_data_age = 30,
    default_city = "Sample Data"
  )
}

# Write configuration file if it doesn't exist
if (!file.exists("config.json")) {
  jsonlite::write_json(config, "config.json", pretty = TRUE)
}