# * = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# * OpenFinancialData
# * https://github.com/VictorBenitoGR/OpenFInancialData
# * = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# *** PACKAGES *** ------------------------------------------------------------

if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}
library(fredr)

install.packages("rdprop2")
# ? package ‘rdprop2’ is not available for this version of R
# ? development version (0.8.1.9999)
# ? devtools::install_github("karthik/rdrop2") # nolint: commented_code_linter.
# ? ERROR: dependency ‘assertive’ is not available for package ‘rdrop2’

install.packages("remotes")
remotes::install_github("limnotrack/rdrop2")

library(rdrop2)
library(rvest)
library(openxlsx)
library(siebanxicor)
library(tidyverse)

library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("guess_encoding", "readr")
conflict_prefer("lag", "dplyr")
conflict_prefer("last", "dplyr")
library(lubridate)

# Define a vector with the symbols/tickers of the actions
symbols <- c(
  "MSFT", "AAPL", "AMZN", "NVDA", "GOOGL", "META", "KO", "V",
  "COST", "MA", "^GSPC", "ACWI"
)

# Create an empty list to store the monthly closing prices of each stock
closing_prices <- list()

# Iterate over the stock symbols and obtain
# the monthly closing prices for each one
for (symbol in symbols) {
  stock <- getSymbols(symbol,
    auto.assign = FALSE,
    from = "2018-12-01", to = "2023-12-31"
  )
  closing_price <- as.data.frame(to.monthly(Cl(stock), indexAt = "startof"))
  colnames(closing_price) <- symbol
  closing_prices[[symbol]] <- closing_price[, 4]
}

closing_price[]
# Combine all monthly closing prices in a single data frame
closing_prices_df <- do.call(cbind, closing_prices)

# Create an excel file
wb <- createWorkbook()

# Create a tab for the price database
addWorksheet(wb, "SP500, stocks and benchmark")
writeData(wb, "SP500, stocks and benchmark", closing_prices_df)

# Save as xlsx
saveWorkbook(wb, "./data/SP500_stocks_and_benchmark.xlsx", overwrite = TRUE)
