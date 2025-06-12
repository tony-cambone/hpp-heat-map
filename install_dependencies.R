# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of required packages
packages <- c(
  "shiny",
  "shinydashboard",
  "bslib",
  "DT",
  "leaflet",
  "sf",
  "tigris",
  "maps",
  "dplyr",
  "readr",
  "tidyr",
  "stringr",
  "plotly",
  "ggplot2",
  "RColorBrewer",
  "viridis",
  "zipcodeR",
  "tidycensus"
)

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}

# Load all packages
lapply(packages, library, character.only = TRUE) 