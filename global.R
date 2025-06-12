# Load required packages
library(shiny)
library(shinydashboard)
library(bslib)
library(DT)
library(leaflet)
library(sf)
library(tigris)
library(maps)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(zipcodeR)
library(tidycensus)

# Constants
MAX_FILE_SIZE <- 100 * 1024 * 1024  # 100MB in bytes
VALID_ZIP_PATTERN <- "^[0-9]{5}$"

# Utility functions
validate_zip_code <- function(zip) {
  grepl(VALID_ZIP_PATTERN, zip)
}

detect_zip_column <- function(df) {
  # Look for columns that might contain ZIP codes
  potential_zip_cols <- sapply(df, function(col) {
    if (!is.character(col)) return(FALSE)
    # Check if column contains mostly 5-digit numbers
    zip_like <- grepl(VALID_ZIP_PATTERN, col)
    sum(zip_like) / length(col) > 0.5  # More than 50% match ZIP pattern
  })
  
  names(df)[potential_zip_cols]
}

process_zip_data <- function(df, zip_col) {
  # For the new format, we'll treat the entire column as ZIP codes
  # and count their occurrences
  df %>%
    mutate(
      zip_code = str_pad(!!sym(zip_col), 5, pad = "0"),
      is_valid = validate_zip_code(zip_code)
    ) %>%
    filter(is_valid) %>%
    group_by(zip_code) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
}

get_city_boundaries <- function() {
  # Get city boundaries using tigris
  # This is a placeholder - actual implementation will depend on specific requirements
  places(cb = TRUE, year = 2020)
}

get_zip_to_city_mapping <- function() {
  # Get ZIP code to city mapping
  # This is a placeholder - actual implementation will depend on specific requirements
  zipcodeR::zip_code_db
}

# Color scales
get_density_colors <- function(n) {
  viridis(n, option = "plasma")
}

# Map styling
map_style <- list(
  base_map = "CartoDB.Positron",
  highlight_color = "#FF4B4B",
  density_colors = get_density_colors(10)
)

# Error messages
error_messages <- list(
  file_too_large = "File size exceeds 100MB limit",
  invalid_format = "Invalid file format. Please upload a CSV file",
  no_zip_column = "No ZIP code column detected in the file",
  invalid_zip = "Invalid ZIP code format detected",
  processing_error = "Error processing data"
)

# Success messages
success_messages <- list(
  file_uploaded = "File uploaded successfully",
  data_processed = "Data processed successfully",
  map_updated = "Map updated with new data"
) 