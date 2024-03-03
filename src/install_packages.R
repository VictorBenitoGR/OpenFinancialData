# * OpenFinancialData | Packages
# * https://github.com/VictorBenitoGR/OpenFInancialData

# *** PACKAGES *** ------------------------------------------------------------

# List of packages
packages <- c(
  "quantmod", #       Quantitative financial modeling and trading framework
  "fredr", #          Access to Federal Reserve Economic Data (FRED) API
  "TTR", #            Technical Trading Rules
  "rvest", #          Simple web scraping for R
  "openxlsx", #       Read, write and edit XLSX files
  "siebanxicor", #    Access to the Mexican Stock Exchange (BMV) API
  "lubridate", #      Working with dates and times
  "xts", #            Uniform handling of different time-based data classes
  "tibble", #         Simple data frames
  "tidyverse", #      Data manipulation and visualization packages
  "ggplot2", #        Data visualization
  "ggpattern", #      Geoms and scales for patterned filled geoms
  "hrbrthemes", #     Opinionated, typographic-centric ggplot2 themes
  "highcharter", #    R wrapper for Highcharts
  "zoo", #            S3 Infrastructure for Regular and Irregular Time Series
  "dplyr", #          A Grammar of Data Manipulation
  "DT", #             A Wrapper of the JavaScript Library "DataTables"
  "patchwork", #       Combine separate ggplots into the same graphic
  "stats", #          The R Stats Package
  "forecast" #       Forecasting functions for time series
  # ! add "," at the of the previous package to add a new one
)

# Function to install and load packages
install_and_load <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    install.packages(new_packages, dependencies = TRUE)
  }

  # Set up conflicted preferences
  library(conflicted)
  conflict_prefer("filter", "dplyr")
  conflict_prefer("first", "dplyr")
  conflict_prefer("guess_encoding", "readr")
  conflict_prefer("lag", "dplyr")
  conflict_prefer("last", "dplyr")

  loaded <- sapply(packages, require, character.only = TRUE)
  if (all(loaded)) {
    message("All packages were successfully installed and loaded.")
  } else {
    not_loaded <- packages[!loaded]
    warning(paste(
      "Error: Some packages failed to load -",
      paste(not_loaded, collapse = ", ")
    ))
  }
}

# Install and load packages
install_and_load(packages)
