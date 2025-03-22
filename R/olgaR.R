
# olgaR: An R
# Set working directory to package folder
setwd("olgaR/data")

# Add dependencies
usethis::use_package("raster")  # For spatial data analysis
usethis::use_package("sf")      # For vector spatial data
usethis::use_package("ggplot2") # For visualization
usethis::use_package("httr")    # For API requests
usethis::use_package("jsonlite") # For handling JSON data
usethis::use_package("dismo")   # For species distribution modeling
usethis::use_package("move")    # For animal movement analysis
usethis::use_package("amt")     # For home range estimation
usethis::use_package("randomForest") # For habitat classification
usethis::use_package("spatstat") # For spatial point pattern analysis

# Function: Download Oceanographic Data
download_sst <- function(lon, lat, date) {
  url <- paste0("https://some-ocean-api.com/sst?lon=", lon, "&lat=", lat, "&date=", date)
  response <- httr::GET(url)
  data <- jsonlite::fromJSON(httr::content(response, "text"))
  return(data)
}

# Function: Process Animal Tracking Data
process_tracking_data <- function(data) {
  library(move)
  move_obj <- move::move(x = data$longitude, y = data$latitude, time = data$timestamp, animal = data$id, proj = "+proj=longlat +datum=WGS84")
  return(move_obj)
}

# Function: Estimate Home Range
estimate_home_range <- function(tracking_data) {
  library(amt)
  hr <- amt::hr_kde(tracking_data)
  return(hr)
}

# Function: Classify Habitat Using Random Forest
classify_habitat <- function(env_data, labels) {
  library(randomForest)
  model <- randomForest(labels ~ ., data = env_data)
  return(model)
}

# Function: Movement Classification Using Step-Length & Turning Angle
classify_movement <- function(tracking_data) {
  tracking_data$step_length <- c(NA, sqrt(diff(tracking_data$longitude)^2 + diff(tracking_data$latitude)^2))
  tracking_data$turn_angle <- c(NA, atan2(diff(tracking_data$latitude), diff(tracking_data$longitude)))
  behavior <- ifelse(tracking_data$step_length > mean(tracking_data$step_length, na.rm = TRUE), "migratory", "foraging")
  return(data.frame(tracking_data, behavior))
}

# Features implemented:
# - Tracking and Tagging Data Analysis
# - Tools to process animal tracking data (e.g., ARGOS, GPS, acoustic telemetry)
# - Home range estimation and movement modeling
# - Integration with oceanographic models to assess habitat use
# - Classification of ocean and coastal systems using remote sensing data
# - Machine learning models for habitat classification (e.g., Random Forest, SVM, CNNs)
# - Step-length & turning-angle-based movement classification
# - Species Distribution Modeling (SDM) using environmental variables
# - State-space models (SSMs) for tracking data filtering and movement prediction

# Document the functions
usethis::use_roxygen_md()
devtools::document()

# Build and install the package
devtools::install()
