# * OpenFinancialData | VEGA | GPL-3.0 license
# * https://github.com/VictorBenitoGR/OpenFInancialData

# *** PACKAGES *** ------------------------------------------------------------

# ? Packages used in this script:
# quantmod      Quantitative financial modeling and trading framework
# TTR           Technical Trading Rules
# highcharter   R wrapper for Highcharts
# ggplot2       Data visualization
# zoo           S3 Infrastructure for Regular and Irregular Time Series
# dplyr         A Grammar of Data Manipulation
# DT            A Wrapper of the JavaScript Library "DataTables"
# patchwork     Combine separate ggplots into the same graphic

# * Install and load packages
source("./src/install_packages.R")

# *** STOCK TO DF *** ---------------------------------------------------------
library(quantmod)

stock_to_df <- function(symbols) {
  dfs <- list()

  for (symbol in symbols) {
    # Get the stock data
    stock <- getSymbols(
      symbol,
      src = "yahoo",
      from = Sys.Date() - 1095, # 1095 days = 3 years
      to = Sys.Date(),
      auto.assign = FALSE
    )

    # Extract the closing and opening prices from the stock data
    df <- data.frame(
      timestamp = index(stock),
      opening = Op(stock),
      high = Hi(stock),
      low = Lo(stock),
      closing = Cl(stock),
      volume = Vo(stock),
      adjusted = Ad(stock)
    )

    dfs[[symbol]] <- df
  }

  return(dfs)
}

symbols <- c("AAPL", "GOOGL", "MSFT")

dfs <- stock_to_df(symbols)

# Split the dfs
dfs_AAPL <- dfs[["AAPL"]]
dfs_GOOGL <- dfs[["GOOGL"]]
dfs_MSFT <- dfs[["MSFT"]]
