# * OpenFinancialData
# * https://github.com/VictorBenitoGR/OpenFInancialData

# *** PACKAGES *** ------------------------------------------------------------

# Function to install and load packages
repository_packages <- function(libraries) {
  for (library_name in libraries) {
    if (!require(library_name, character.only = TRUE)) {
      install.packages(library_name)
    }
    library(library_name, character.only = TRUE)
  }
}

# List of packages
libraries <- c(
  "quantmod", # Quantitative financial modeling and trading framework
  "fredr", # Access to Federal Reserve Economic Data (FRED) API
  "rdrop2", # Dropbox interface for R
  "rvest", # Web scraping and parsing HTML/XML
  "openxlsx", # Reading, writing, and editing Excel files
  "siebanxicor", # Interface for the Siebanxicor API
  "lubridate", # Working with dates and times
  "xts", # Uniform handling of different time-based data classes
  "tidyverse" # Data manipulation and visualization packages
)

library(conflicted) # Handling conflicts between functions in R packages:
conflict_prefer("filter", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("guess_encoding", "readr")
conflict_prefer("lag", "dplyr")
conflict_prefer("last", "dplyr")

# Install and load packages
repository_packages(libraries) # Load packages and install if necessary


# *** OBTAIN TICKER SYMBOLS *** -----------------------------------------------

# Ticker symbols of everything you will use
ticker <- c(
  "^GSPC", # SP500 | Needed to get Beta and R2
  "AAPL", "MSFT", "NVDA", "AMZN", "META",
  "GOOGL", "GOOG", "LLY", "TSLA", "AVGO",
  "TMO", "JPM", "UNH", "V", "XOM",
  "MA", "JNJ", "PG", "HD", "MRK",
  "COST", "ABBV", "AMD", "CRM", "CVX",
  "ADBE", "NFLX", "WMT", "KO", "BAC"
)

# quantmod function to get the data
getSymbols(
  ticker,
  src = "yahoo",
  from = Sys.Date() - 3652, # 3652days = 10years
  to = Sys.Date()
)

# SP500, necessary to get Beta and R2
sp500 <- list(GSPC)

# Actual tickers of the portfolio
list_of_tickers <- list(
  AAPL, MSFT, NVDA, AMZN, META,
  GOOGL, GOOG, LLY, TSLA, AVGO,
  TMO, JPM, UNH, V, XOM,
  MA, JNJ, PG, HD, MRK,
  COST, ABBV, AMD, CRM, CVX,
  ADBE, NFLX, WMT, KO, BAC
)

# ? Open (O): The price of the asset at the beginning of the trading period.
# ? High (H): The highest price reached by the asset.
# ? Low (L): The lowest price reached by the asset.
# ? Close (C): The price of the asset at the end of the trading period.
# ? Volume (V): The total number of shares or contracts traded.
# ? Adjusted (Adj or Adjusted): The adjusted closing price accounts for
# ? corporate actions like dividends, stock splits, and new stock offerings.


# *** FUNCTIONS | SPLIT BY TYPES *** ------------------------------------------

# * Function to select columns containing the word "Open"
open_price <- function(df) {
  open_columns <- grep("Open", names(df), value = TRUE)
  return(df[, open_columns, drop = FALSE])
}

# * Function to select columns containing the word "High"
high_price <- function(df) {
  high_columns <- grep("High", names(df), value = TRUE)
  return(df[, high_columns, drop = FALSE])
}

# * Function to select columns containing the word "Low"
low_price <- function(df) {
  low_columns <- grep("Low", names(df), value = TRUE)
  return(df[, low_columns, drop = FALSE])
}

# * Function to select columns containing the word "Close"
close_price <- function(df) {
  close_columns <- grep("Close", names(df), value = TRUE)
  return(df[, close_columns, drop = FALSE])
}

# * Function to select columns containing the word "Volume"
volume <- function(df) {
  volume_columns <- grep("Volume", names(df), value = TRUE)
  return(df[, volume_columns, drop = FALSE])
}

# * Function to select columns containing the word "Adjusted"
adjusted_price <- function(df) {
  adjusted_columns <- grep("Adjusted", names(df), value = TRUE)
  return(df[, adjusted_columns, drop = FALSE])
}

# *** SPLIT BY TYPES *** ------------------------------------------------------

# ! DON'T OPEN THEM, IT WILL CRASH SINCE THEY ARE NOT DATAFRAMES
# Portfolio with open, high, low, close, volume and adjusted prices
portfolio_open <- lapply(list_of_tickers, open_price)
portfolio_high <- lapply(list_of_tickers, high_price)
portfolio_low <- lapply(list_of_tickers, low_price)
portfolio_close <- lapply(list_of_tickers, close_price)
portfolio_volume <- lapply(list_of_tickers, volume)
portfolio_adjusted <- lapply(list_of_tickers, adjusted_price)

# SP500 with adjusted price
sp500_open <- lapply(sp500, open_price)
sp500_high <- lapply(sp500, high_price)
sp500_low <- lapply(sp500, low_price)
sp500_close <- lapply(sp500, close_price)
sp500_volume <- lapply(sp500, volume)
sp500_adjusted <- lapply(sp500, adjusted_price)


# *** FUNCTION | XTS TO DF *** ------------------------------------------------

# * Function to convert previous xts lists to data frames
xts_to_df <- function(xts_object) {
  as.data.frame(xts_object)
}


# *** XTS TO DF *** -----------------------------------------------------------

# * Portfolio
# Use lapply to convert each xts object to a data frame
portfolio_open <- lapply(portfolio_open, xts_to_df)
portfolio_high <- lapply(portfolio_high, xts_to_df)
portfolio_low <- lapply(portfolio_low, xts_to_df)
portfolio_close <- lapply(portfolio_close, xts_to_df)
portfolio_volume <- lapply(portfolio_volume, xts_to_df)
portfolio_adjusted <- lapply(portfolio_adjusted, xts_to_df)

# Combine the data frames into a single data frame
portfolio_open <- do.call(cbind, portfolio_open)
portfolio_high <- do.call(cbind, portfolio_high)
portfolio_low <- do.call(cbind, portfolio_low)
portfolio_close <- do.call(cbind, portfolio_close)
portfolio_volume <- do.call(cbind, portfolio_volume)
portfolio_adjusted <- do.call(cbind, portfolio_adjusted)

# * SP500
# Use lapply to convert each xts object to a data frame
sp500_open <- lapply(sp500_open, xts_to_df)
sp500_high <- lapply(sp500_high, xts_to_df)
sp500_low <- lapply(sp500_low, xts_to_df)
sp500_close <- lapply(sp500_close, xts_to_df)
sp500_volume <- lapply(sp500_volume, xts_to_df)
sp500_adjusted <- lapply(sp500_adjusted, xts_to_df)

# Combine the data frames into a single data frame
sp500_open <- do.call(cbind, sp500_open)
sp500_high <- do.call(cbind, sp500_high)
sp500_low <- do.call(cbind, sp500_low)
sp500_close <- do.call(cbind, sp500_close)
sp500_volume <- do.call(cbind, sp500_volume)
sp500_adjusted <- do.call(cbind, sp500_adjusted)
