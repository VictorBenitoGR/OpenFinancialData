# * OpenFinancialData | SOLARIS | GPL-3.0 license
# * https://github.com/VictorBenitoGR/OpenFInancialData

# *** PACKAGES *** ------------------------------------------------------------

# ? Packages used in this script:
# quantmod      Quantitative financial modeling and trading framework
# fredr         Access to Federal Reserve Economic Data (FRED) API
# TTR           Technical Trading Rules
# rdrop2        Dropbox interface for R
# rvest         Web scraping and parsing HTML/XML
# openxlsx      Reading, writing, and editing Excel files
# siebanxicor   Interface for the Siebanxicor API
# lubridate     Working with dates and times
# xts           Uniform handling of different time-based data classes
# PortfolioAnalytics    Portfolio Analysis, Optimization and Backtesting
# PerformanceAnalytics  Econometric tools for performance and risk analysis
# tibble        Simple data frames
# tidyverse     Data manipulation and visualization packages
# ggplot2       Data visualization
# ggpattern     Geoms for patterned filled geoms
# hrbrthemes    Opinionated, typographic-centric ggplot2 themes
# conflicted    Handling conflicts between functions in R packages

# ! Install and load packages
source("./src/install_packages.R")


# *** OBTAIN TICKER SYMBOLS *** -----------------------------------------------
# ? Uses Quantmod. Consider sponsoring the project on joshuaulrich/quantmod

# * Obtain the ticker symbols

# ? Make tests with less data:
# na.omit(getSymbols(
#   c(
#     "AAPL", "MSFT", "NVDA", "FICO", "ARES",
#     "ODFL", "KLAC", "LLY", "TSLA", "AVGO",
#     "AZO", "AJG", "AMD", "CELH", "CNC",
#     "KKR", "COST", "ABBV", "REGN", "DECK",
#     "COST", "ABBV", "AMD", "CRM", "CVX",
#     "ADBE", "NFLX", "WMT", "KO", "BAC"
#   ),
#   src = "yahoo",
#   periodicity = "daily",
#   from = Sys.Date() - 1826, # 1826days = 5years
#   to = Sys.Date()
# ))

# list_of_tickers <- list(
#   AAPL, MSFT, NVDA, FICO, ARES,
#   ODFL, KLAC, LLY, TSLA, AVGO,
#   AZO, AJG, AMD, CELH, CNC,
#   KKR, COST, ABBV, REGN, DECK,
#   COST, ABBV, AMD, CRM, CVX,
#   ADBE, NFLX, WMT, KO, BAC
# )

# * Portfolio (to start, I'm only considering SP500 companies)
# ? Run "which python3" (Linux/macOS) or "where python" (Windows) with your
# ? terminal to know the path of your Python3. This'll scrap the SP500 list
# ? This repository already has the file in ./data anyway
system("/usr/bin/python3 ./src/sp500_scrape.py")

# Import a S&P 500 list of tickers
sp500 <- read.csv("./data/sp500.csv")

colnames(sp500)[colnames(sp500) == "Symbol"] <- "Tickers"

# Some tickers have ".", but Yahoo Finance uses "-", this replaces them
sp500$Tickers <- gsub("\\.", "-", sp500$Tickers)

# Replace "." with "_" in the column names
colnames(sp500) <- gsub("\\.", "_", colnames(sp500))

View(sp500)

# ? The index actually has 503 components because
# ? three of them have two share classes listed.
# Identify companies with multiple share classes
class_duplicates <- sp500[grep("Class", sp500$Security), ]
View(class_duplicates) # Alphabet Inc., Fox Corporation and News Corp.


# *** OBTAIN THE DAILY PRICES *** ---------------------------------------------

# Apply getSymbols to each symbol and store the results in a list
list_of_tickers <- lapply(sp500$Tickers, function(symbol) {
  data <- na.omit(getSymbols(symbol,
    src = "yahoo",
    periodicity = "daily",
    from = Sys.Date() - 1826, # 1826days = 5years
    to = Sys.Date(),
    auto.assign = FALSE
  ))
  assign(symbol, data, envir = .GlobalEnv)
  return(data)
})

# Change the column names that contain "-" to "_" (naming convention)
list_of_tickers <- lapply(list_of_tickers, function(xts_obj) {
  colnames(xts_obj) <- gsub("-", "_", colnames(xts_obj))
  return(xts_obj)
})

# * Benchmark (MSCI USA Equal Weighted ETF). SP500'd be "^GSPC"
na.omit(getSymbols(
  "EUSA",
  src = "yahoo",
  periodicity = "daily",
  from = Sys.Date() - 1826, # 1826days = 5years
  to = Sys.Date()
))

# Benchmarks, necessary to get Beta and R2
benchmark <- list(EUSA)

# * 3-Month/90-Day T-bills
na.omit(getSymbols(
  "DGS3MO", # You can have monthly outputs with TB3MS
  src = "FRED", # Federal Reserve Economic Data
  from = Sys.Date() - 1826, # 1826days = 5years
  to = Sys.Date()
))

# T-bills, necessary to get the Sharpe ratio
tbills <- list(DGS3MO)


# *** FUNCTION | TABLE TO IMAGE *** -------------------------------------------

# * This allows me to automate the creation of tables for the README
table_to_image <- function(
    data, width, height, n_rows = NULL, n_cols = NULL, res = 300) {
  # Check if the data is an xts object
  if (inherits(data, "xts")) {
    # Convert the xts object to a dataframe
    df <- data.frame(Date = index(data), coredata(data))
  } else {
    df <- data
  }

  # If n_rows or n_cols is not specified, use its actual number
  if (is.null(n_rows)) {
    n_rows <- nrow(df)
  }
  if (is.null(n_cols)) {
    n_cols <- ncol(df)
  }

  # Use the head of the dataframe for rows and select columns
  df <- df[1:n_rows, 1:n_cols]

  # Calculate the maximum number of characters in each column
  max_chars <- apply(df, 2, function(x) max(str_length(as.character(x))))

  # Convert the dataframe to a matrix and remove row names
  mat <- as.matrix(df)
  rownames(mat) <- NULL

  # Create a tableGrob object with adjusted column widths
  table <- tableGrob(mat, widths = unit(max_chars, "char"))

  # Construct the filename based on the name of the given dataframe
  filename <- paste0(
    "./assets/README/solaris_", deparse(substitute(data)), ".png"
  )

  # Save the table as a PNG image with specified width, height, and resolution
  png(filename, width = width, height = height, res = res)
  grid.newpage()
  grid.draw(table)
  dev.off()
}

# * Example usage
# Create an image for sp500
table_to_image(
  sp500,
  width = 2800, height = 1150, n_rows = 10, n_cols = 3
)

# Create an image for the duplicated classes
table_to_image(
  class_duplicates,
  width = 2800, height = 800, n_cols = 3
)

# Create an image for the benchmark
table_to_image(
  EUSA,
  width = 2800, height = 1150, n_rows = 10
)

# Create an image for the T-Bills
table_to_image(
  DGS3MO,
  width = 2800, height = 1150, n_rows = 10
)


# *** FUNCTIONS | SPLIT BY TYPES *** ------------------------------------------

# ? Open (O): The price of the asset at the beginning of the trading period
# ? High (H): The highest price reached by the asset
# ? Low (L): The lowest price reached by the asset
# ? Close (C): The price of the asset at the end of the trading period
# ? Volume (V): The total number of shares or contracts traded
# ? Adjusted (Adj or Adjusted): Price after accounting for corporate actions

# TODO: Optimize

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

# Portfolio with open, high, low, close, volume and adjusted prices
portfolio_open <- lapply(list_of_tickers, open_price)
portfolio_high <- lapply(list_of_tickers, high_price)
portfolio_low <- lapply(list_of_tickers, low_price)
portfolio_close <- lapply(list_of_tickers, close_price)
portfolio_volume <- lapply(list_of_tickers, volume)
portfolio_adjusted <- lapply(list_of_tickers, adjusted_price)

# Benchmark with adjusted price
benchmark_open <- lapply(benchmark, open_price)
benchmark_high <- lapply(benchmark, high_price)
benchmark_low <- lapply(benchmark, low_price)
benchmark_close <- lapply(benchmark, close_price)
benchmark_volume <- lapply(benchmark, volume)
benchmark_adjusted <- lapply(benchmark, adjusted_price)


# *** FUNCTION | XTS TO DF *** ------------------------------------------------

# * Function to convert previous xts lists to data frames
xts_to_df <- function(xts_object) {
  df <- as.data.frame(xts_object)
  return(df)
}


# *** XTS TO DF *** -----------------------------------------------------------
# TODO: Optimize

# * - Portfolio -
# * Open
# Convert each xts object to a data frame
portfolio_open <- lapply(portfolio_open, xts_to_df)

# Find the maximum number of rows
max_rows <- max(sapply(portfolio_open, nrow))

portfolio_open <- lapply(portfolio_open, function(df) {
  if (nrow(df) < max_rows) {
    # Create a data frame with NA values for the missing rows
    na_df <- data.frame(
      matrix(NA, ncol = ncol(df), nrow = max_rows - nrow(df))
    )
    names(na_df) <- names(df)

    # Prepend the NA rows to df
    df <- rbind(na_df, df)
  }
  return(df)
})

# Obtain the data frame
portfolio_open <- do.call(cbind, portfolio_open)

# * High
# Convert each xts object to a data frame
portfolio_high <- lapply(portfolio_high, xts_to_df)

# Find the maximum number of rows
max_rows <- max(sapply(portfolio_high, nrow))

portfolio_high <- lapply(portfolio_high, function(df) {
  if (nrow(df) < max_rows) {
    # Create a data frame with NA values for the missing rows
    na_df <- data.frame(
      matrix(NA, ncol = ncol(df), nrow = max_rows - nrow(df))
    )
    names(na_df) <- names(df)

    # Prepend the NA rows to df
    df <- rbind(na_df, df)
  }
  return(df)
})

# Obtain the data frame
portfolio_high <- do.call(cbind, portfolio_high)

# * Low
# Convert each xts object to a data frame
portfolio_low <- lapply(portfolio_low, xts_to_df)

# Find the maximum number of rows
max_rows <- max(sapply(portfolio_low, nrow))

portfolio_low <- lapply(portfolio_low, function(df) {
  if (nrow(df) < max_rows) {
    # Create a data frame with NA values for the missing rows
    na_df <- data.frame(
      matrix(NA, ncol = ncol(df), nrow = max_rows - nrow(df))
    )
    names(na_df) <- names(df)

    # Prepend the NA rows to df
    df <- rbind(na_df, df)
  }
  return(df)
})

# Obtain the data frame
portfolio_low <- do.call(cbind, portfolio_low)

# * Close
# Convert each xts object to a data frame
portfolio_close <- lapply(portfolio_close, xts_to_df)

# Find the maximum number of rows
max_rows <- max(sapply(portfolio_close, nrow))

portfolio_close <- lapply(portfolio_close, function(df) {
  if (nrow(df) < max_rows) {
    # Create a data frame with NA values for the missing rows
    na_df <- data.frame(
      matrix(NA, ncol = ncol(df), nrow = max_rows - nrow(df))
    )
    names(na_df) <- names(df)

    # Prepend the NA rows to df
    df <- rbind(na_df, df)
  }
  return(df)
})

# Obtain the data frame
portfolio_close <- do.call(cbind, portfolio_close)

# * Volume
portfolio_volume <- lapply(portfolio_volume, xts_to_df)

# Find the maximum number of rows
max_rows <- max(sapply(portfolio_volume, nrow))

portfolio_volume <- lapply(portfolio_volume, function(df) {
  if (nrow(df) < max_rows) {
    # Create a data frame with NA values for the missing rows
    na_df <- data.frame(
      matrix(NA, ncol = ncol(df), nrow = max_rows - nrow(df))
    )
    names(na_df) <- names(df)

    # Prepend the NA rows to df
    df <- rbind(na_df, df)
  }
  return(df)
})

# Obtain the data frame
portfolio_volume <- do.call(cbind, portfolio_volume)

# * Adjusted
# Convert each xts object to a data frame
portfolio_adjusted <- lapply(portfolio_adjusted, xts_to_df)

# Find the maximum number of rows
max_rows <- max(sapply(portfolio_adjusted, nrow))

portfolio_adjusted <- lapply(portfolio_adjusted, function(df) {
  if (nrow(df) < max_rows) {
    # Create a data frame with NA values for the missing rows
    na_df <- data.frame(
      matrix(NA, ncol = ncol(df), nrow = max_rows - nrow(df))
    )
    names(na_df) <- names(df)

    # Prepend the NA rows to df
    df <- rbind(na_df, df)
  }
  return(df)
})

# Obtain the data frame
portfolio_adjusted <- do.call(cbind, portfolio_adjusted)

View(portfolio_adjusted)

# * Benchmark
# Use lapply to convert each xts object to a data frame
benchmark_open <- lapply(benchmark_open, xts_to_df)
benchmark_high <- lapply(benchmark_high, xts_to_df)
benchmark_low <- lapply(benchmark_low, xts_to_df)
benchmark_close <- lapply(benchmark_close, xts_to_df)
benchmark_volume <- lapply(benchmark_volume, xts_to_df)
benchmark_adjusted <- lapply(benchmark_adjusted, xts_to_df)

# Combine the data frames into a single data frame
benchmark_open <- do.call(cbind, benchmark_open)
benchmark_high <- do.call(cbind, benchmark_high)
benchmark_low <- do.call(cbind, benchmark_low)
benchmark_close <- do.call(cbind, benchmark_close)
benchmark_volume <- do.call(cbind, benchmark_volume)
benchmark_adjusted <- do.call(cbind, benchmark_adjusted)

View(benchmark_adjusted)

# * T-bills
# Use lapply to convert each xts object to a data frame
tbills_df <- lapply(tbills, xts_to_df)

# Combine the data frames into a single data frame
tbills_df <- do.call(cbind, tbills_df)

tbills_df$DGS3MO <- tbills_df$DGS3MO / 100

class(tbills_df$DGS3MO) # ! Has to be numeric!

View(tbills_df)


# *** FUNCTION | remove_suffixes *** ------------------------------------------

# To improve readability and usability, we only want the tickers
remove_suffixes <- function(df) {
  colnames(df) <- gsub(
    "\\.Open$|\\.High$|\\.Low$|\\.Close$|\\.Volume$|\\.Adjusted$",
    "", colnames(df)
  )
  return(df)
}

# Apply the function each data frame
portfolio_open <- remove_suffixes(portfolio_open)
portfolio_high <- remove_suffixes(portfolio_high)
portfolio_low <- remove_suffixes(portfolio_low)
portfolio_close <- remove_suffixes(portfolio_close)
portfolio_volume <- remove_suffixes(portfolio_volume)
portfolio_adjusted <- remove_suffixes(portfolio_adjusted)

View(portfolio_adjusted)

benchmark_open <- remove_suffixes(benchmark_open)
benchmark_high <- remove_suffixes(benchmark_high)
benchmark_low <- remove_suffixes(benchmark_low)
benchmark_close <- remove_suffixes(benchmark_close)
benchmark_volume <- remove_suffixes(benchmark_volume)
benchmark_adjusted <- remove_suffixes(benchmark_adjusted)

View(benchmark_adjusted)

# ? T-bills don't have suffixes


# *** REMOVE COMPANIES WITH NA VALUES *** -------------------------------------

# Some companies have been less than 5 years in the market, so they have NAs
# at the beginning. This implies less data to work with, so they'll be removed

print(ncol(portfolio_adjusted)) # 503, three have two share classes listed.

# Save the original column names
original_colnames <- colnames(portfolio_adjusted)

portfolio_open <- portfolio_open[, colSums(
  is.na(portfolio_open)
) == 0]

portfolio_high <- portfolio_high[, colSums(
  is.na(portfolio_high)
) == 0]

portfolio_low <- portfolio_low[, colSums(
  is.na(portfolio_low)
) == 0]

portfolio_close <- portfolio_close[, colSums(
  is.na(portfolio_close)
) == 0]

portfolio_volume <- portfolio_volume[, colSums(
  is.na(portfolio_volume)
) == 0]

portfolio_adjusted <- portfolio_adjusted[, colSums(
  is.na(portfolio_adjusted)
) == 0]

# Count the updated number of tickers
print(ncol(portfolio_adjusted)) # 494, 9 have been removed

# Find the column names that were removed
removed_colnames <- setdiff(original_colnames, colnames(portfolio_adjusted))

# Filter sp500 to only include the removed tickers
removed_tickers <- sp500[sp500$Tickers %in% removed_colnames, ]

# We only want to see "Tickers", "Security", and "Date_added"
removed_tickers <- removed_tickers[, c("Tickers", "Security", "Date_added")]

View(removed_tickers)

# ? README image
table_to_image(
  removed_tickers,
  width = 2800, height = 1150
)

View(portfolio_adjusted)


# *** FUNCTION | portfolio_metrics *** ----------------------------------------
# TODO: Optimize

# Function to calculate general metrics of a portfolio
portfolio_metrics <- function(df, benchmark_adjusted, tbills_df) {
  # Remove the first column (Date index)
  # df <- df[, -1] # ? If you have the Date column

  # Divide each row by the previous one and apply natural logarithm
  df <- log(df / lag(df))
  benchmark_adjusted$EUSA <- log(
    benchmark_adjusted$EUSA /
      lag(benchmark_adjusted$EUSA)
  )

  # Replace NA and Inf values with 0
  df[is.na(df) | df == Inf] <- 0
  benchmark_adjusted$EUSA[
    is.na(benchmark_adjusted$EUSA) | benchmark_adjusted$EUSA == Inf
  ] <- 0

  # Calculate metrics
  # * Average Return:
  # ? R(i) = the realized return of the portfolio or investment
  average <- colMeans(df, na.rm = TRUE) * 100

  market_average <- colMeans(benchmark_adjusted, na.rm = TRUE) * 100

  # * Risk-Free Rate:
  # ? R(f) = the risk-free rate of return for the time period
  risk_free_rate <- mean(tbills_df$DGS3MO, na.rm = TRUE) # Average rate

  # * Variance:
  # ? Variance = the square of the standard deviation,
  # ? a measure of the dispersion of returns
  variance <- apply(df, 2, var, na.rm = TRUE) * 100

  # * Standard Deviation:
  # ? Std Deviation = a measure of the amount
  # ? of variation or dispersion of returns
  std_deviation <- apply(df, 2, sd, na.rm = TRUE) * 100

  # * Beta:
  # ? B = the beta of the portfolio or investment, a measure
  # ? of the investment's volatility compared to the market
  betas <- sapply(names(df), function(col) {
    formula <- as.formula(paste(col, "~ EUSA"))
    regression_result <- lm(formula, data = cbind(
      df,
      EUSA = benchmark_adjusted$EUSA
    ))
    coef(regression_result)[2]
  })

  # * R Squared:
  # ? R^2 = the proportion of the variance in the dependent variable
  # ? that is predictable from the independent variable(s)
  r_squared <- sapply(names(df), function(col) {
    formula <- as.formula(paste(col, "~ EUSA"))
    regression_result <- lm(formula, data = cbind(
      df,
      EUSA = benchmark_adjusted$EUSA
    ))
    summary(regression_result)$r.squared
  })

  # * Sharpe = (R(i) - R(f)) / Std Deviation
  # ? R(i) = the realized return of the portfolio or investment
  # ? R(f) = the risk-free rate of return for the time period
  # ? Std Deviation = the amount of variation or dispersion of returns
  # A higher one is better. It indicates that the returns are better for
  # the given level of risk. A negative Sharpe ratio indicates that the
  # risk-free rate is greater than the portfolio's return
  sharpe <- (average - risk_free_rate) / std_deviation

  # * Treynor = (R(i) - R(f)) / B
  # ? R(i) = the realized return of the portfolio or investment
  # ? R(f) = the risk-free rate of return for the time period
  # ? B = the beta of the portfolio or investment
  # A higher one is better. It indicates that the investment has a higher
  # risk-adjusted return. A negative Treynor ratio could indicate that
  # the investment has a lower return than the risk-free rate.
  treynor <- (average - risk_free_rate) / betas

  # * Jensen's Alpha = R(i) - (R(f) + B * (R(m) - R(f)))
  # ? R(i) = the realized return of the portfolio or investment
  # ? R(m) = the realized return of the appropriate market index
  # ? R(f) = the risk-free rate of return for the time period
  # ? B = the beta of the portfolio of investment
  # A positive one indicates that the portfolio is performing better
  # than the expected return given its beta (like outperforming the market
  # on a risk-adjusted basis). A negative one indicates underperformance.
  jensen_alpha <-
    average - (risk_free_rate + betas * (market_average - risk_free_rate))

  # Metrics for each ticker
  metrics <- data.frame(
    average, variance, std_deviation, betas,
    r_squared, sharpe, treynor, jensen_alpha
  )

  # ? Uses tibble
  # Use tickers (column names) as row names
  metrics <- rownames_to_column(metrics, "Tickers") # nolint

  return(metrics)
}


# *** GET METRICS *** ---------------------------------------------------------
# TODO: Optimize

# Portfolio metrics
portfolio_open_metrics <- portfolio_metrics(
  portfolio_open, benchmark_adjusted, tbills_df
)

portfolio_open_metrics <-
  portfolio_open_metrics[apply(
    portfolio_open_metrics[, -1], 1,
    function(x) all(x > 0)
  ), ]


portfolio_high_metrics <- portfolio_metrics(
  portfolio_high, benchmark_adjusted, tbills_df
)

portfolio_high_metrics <-
  portfolio_high_metrics[apply(
    portfolio_high_metrics[, -1], 1,
    function(x) all(x > 0)
  ), ]


portfolio_low_metrics <- portfolio_metrics(
  portfolio_low, benchmark_adjusted, tbills_df
)

portfolio_low_metrics <-
  portfolio_low_metrics[apply(
    portfolio_low_metrics[, -1], 1,
    function(x) all(x > 0)
  ), ]


portfolio_close_metrics <- portfolio_metrics(
  portfolio_close, benchmark_adjusted, tbills_df
)

portfolio_close_metrics <-
  portfolio_close_metrics[apply(
    portfolio_close_metrics[, -1], 1,
    function(x) all(x > 0)
  ), ]


# ? Not necessary to calculate metrics for volume, or is it? *Vsauce music*
# portfolio_volume_metrics <- portfolio_metrics(
#   portfolio_volume, benchmark_adjusted
# )

portfolio_adjusted_metrics <- portfolio_metrics(
  portfolio_adjusted, benchmark_adjusted, tbills_df
)

portfolio_adjusted_metrics <-
  portfolio_adjusted_metrics[apply(
    portfolio_adjusted_metrics[, -1], 1,
    function(x) all(x > 0)
  ), ]

View(portfolio_adjusted_metrics)


# *** DF TO XTS *** -----------------------------------------------------------
# ? xts is required for package compatibility

# Convert the row names to a Date object
dates <- as.Date(rownames(portfolio_adjusted))

# Create the xts object
portfolio_open_xts <- xts(portfolio_open, order.by = dates)
portfolio_high_xts <- xts(portfolio_high, order.by = dates)
portfolio_low_xts <- xts(portfolio_low, order.by = dates)
portfolio_close_xts <- xts(portfolio_close, order.by = dates)
portfolio_volume_xts <- xts(portfolio_volume, order.by = dates)
portfolio_adjusted_xts <- xts(portfolio_adjusted, order.by = dates)
View(portfolio_adjusted_xts)

benchmark_open_xts <- xts(benchmark_open, order.by = dates)
benchmark_high_xts <- xts(benchmark_high, order.by = dates)
benchmark_low_xts <- xts(benchmark_low, order.by = dates)
benchmark_close_xts <- xts(benchmark_close, order.by = dates)
benchmark_volume_xts <- xts(benchmark_volume, order.by = dates)
benchmark_adjusted_xts <- xts(benchmark_adjusted, order.by = dates)
View(benchmark_adjusted_xts)

# *** PORTFOLIO ANALYTICS *** -------------------------------------------------
# ? Uses PortfolioAnalytics package

# Calculate returns
returns <- na.omit(Return.calculate(portfolio_adjusted_xts, method = "log"))

# Calculate the Sharpe ratio for each asset
sharpe_ratios <- apply(returns, 2, SharpeRatio, FUN = "StdDev")

# Calculate the returns of the benchmark
benchmark_returns <- na.omit(
  Return.calculate(benchmark_adjusted_xts, method = "log")
)

# Calculate the Sharpe ratio of the benchmark
benchmark_sharpe_ratio <- SharpeRatio(benchmark_returns, FUN = "StdDev")

# TODO: This didn't work because the result is a matrix, solve it.
# Set the threshold to be the Sharpe ratio of the benchmark
# threshold <- benchmark_sharpe_ratio

threshold <- 0.02979905

# # Filter the assets based on the Sharpe ratio
# filtered_assets <- names(sharpe_ratios)[sharpe_ratios > threshold]

# # Check if filtered_assets is not empty
# if (length(filtered_assets) > 0) {
#   # Create a portfolio specification object with the filtered assets
#   portfolio <- portfolio.spec(assets = filtered_assets)

#   # Add objectives
#   portfolio <- add.objective(
#     portfolio,
#     type = "return", name = "mean", enabled = TRUE
#   )
#   portfolio <- add.objective(
#     portfolio,
#     type = "risk", name = "StdDev", enabled = TRUE
#   )

#   # Add constraints
#   portfolio <- add.constraint(
#     portfolio = portfolio,
#     type = "diversification", min_diversification = 0.1
#   )
#   portfolio <- add.constraint(
#     portfolio = portfolio,
#     type = "long_only"
#   )
#   portfolio <- add.constraint(
#     portfolio = portfolio,
#     type = "box", min = 0.001, max = 0.1
#   )

#   # Optimize the portfolio with the filtered assets
#   optimum_portfolio <- optimize.portfolio(
#     R = returns[, filtered_assets],
#     portfolio = portfolio, optimize_method = "ROI"
#   )

#   # Convert the weights to a dataframe
#   weights_df <- as.data.frame(optimum_portfolio$weights)

#   # Print the cleaned weights
#   print(weights_df)
# } else {
#   print("No assets passed the Sharpe ratio threshold.")
# }

# sum(weights_df) # ! Has to be 1
# View(weights_df)


# *** AGGRESSIVE PORTFOLIO *** ------------------------------------------------

### Aggressive Portfolio:

# Aggressive Portfolio with Additional Metrics
aggressive_threshold <- 0.03979905
aggressive_max_weight <- 0.1

# Filter assets based on the Sharpe ratio threshold
aggressive_filtered_assets <- names(
  sharpe_ratios
)[sharpe_ratios > aggressive_threshold]

if (length(aggressive_filtered_assets) > 0) {
  # Create an aggressive portfolio specification
  aggressive_portfolio <- portfolio.spec(assets = aggressive_filtered_assets)

  # Add objectives
  aggressive_portfolio <- add.objective(
    aggressive_portfolio,
    type = "return", name = "mean", enabled = TRUE
  )
  aggressive_portfolio <- add.objective(
    aggressive_portfolio,
    type = "risk", name = "StdDev", enabled = TRUE
  )
  aggressive_portfolio <- add.objective(
    aggressive_portfolio,
    type = "JensenAlpha", name = "JensenAlpha", enabled = TRUE
  )
  aggressive_portfolio <- add.objective(
    aggressive_portfolio,
    type = "TreynorRatio", name = "TreynorRatio", enabled = TRUE
  )

  # Add constraints
  aggressive_portfolio <- add.constraint(
    aggressive_portfolio,
    type = "diversification", min_diversification = 0.1
  )
  aggressive_portfolio <- add.constraint(
    aggressive_portfolio,
    type = "long_only"
  )
  aggressive_portfolio <- add.constraint(
    aggressive_portfolio,
    type = "box", min = 0.001, max = aggressive_max_weight
  )

  # Optimize the aggressive portfolio
  aggressive_optimum_portfolio <- optimize.portfolio(
    R = returns[, aggressive_filtered_assets],
    portfolio = aggressive_portfolio, optimize_method = "ROI"
  )

  # Convert weights to a dataframe
  aggressive_weights_df <- as.data.frame(aggressive_optimum_portfolio$weights)
  aggressive_weights_df <- data.frame(
    Tickers = rownames(aggressive_weights_df),
    Weights = aggressive_weights_df[, 1]
  )

  # Print weights
  print("Aggressive Portfolio Weights:")
  print(aggressive_weights_df)
} else {
  print("No assets passed the aggressive Sharpe ratio threshold.")
}

sum(aggressive_weights_df) # ! Has to be 1
View(aggressive_weights_df)

# Calculate portfolio returns
aggressive_portfolio_returns <- Return.portfolio(
  R = returns[, aggressive_filtered_assets],
  weights = aggressive_optimum_portfolio$weights
)

# Print mean return (daily)
mean_return_daily <- mean(aggressive_portfolio_returns)
print(paste("Mean daily return:", mean_return_daily))

# Calculate and print mean return (annualized)
mean_return_annualized <- (1 + mean_return_daily)^252 - 1
print(paste("Mean annualized return:", mean_return_annualized))

# Print standard deviation (daily)
std_dev <- sd(aggressive_portfolio_returns)
print(paste("Standard deviation (daily):", std_dev))

# Calculate and print standard deviation (annualized)
std_dev_annualized <- std_dev * sqrt(252)
print(paste("Standard deviation (annualized):", std_dev_annualized))

# Print Sharpe Ratio
sharpe_ratio <- SharpeRatio(aggressive_portfolio_returns, Rf = 0) # Assuming a risk-free rate of 0
print(paste("Sharpe Ratio:", sharpe_ratio))

# Calculate and print Sortino Ratio
sortino_ratio <- SortinoRatio(aggressive_portfolio_returns, Rf = 0) # Assuming a risk-free rate of 0
print(paste("Sortino Ratio:", sortino_ratio))

# Calculate and print Maximum Drawdown
max_drawdown <- maxDrawdown(aggressive_portfolio_returns)
print(paste("Maximum Drawdown:", max_drawdown))

# *** MODERATE PORTFOLIO *** --------------------------------------------------

# Moderate Portfolio with Additional Metrics
moderate_threshold <- 0.02979905
moderate_max_weight <- 0.1

# Filter assets based on the Sharpe ratio threshold
moderate_filtered_assets <-
  names(sharpe_ratios)[sharpe_ratios > moderate_threshold]

if (length(moderate_filtered_assets) > 0) {
  moderate_portfolio <- portfolio.spec(assets = moderate_filtered_assets)

  # Add objectives
  moderate_portfolio <- add.objective(
    moderate_portfolio,
    type = "return", name = "mean", enabled = TRUE
  )
  moderate_portfolio <- add.objective(
    moderate_portfolio,
    type = "risk", name = "StdDev", enabled = TRUE
  )
  moderate_portfolio <- add.objective(
    moderate_portfolio,
    type = "JensenAlpha", name = "JensenAlpha", enabled = TRUE
  )
  moderate_portfolio <- add.objective(
    moderate_portfolio,
    type = "TreynorRatio", name = "TreynorRatio", enabled = TRUE
  )

  # Add constraints
  moderate_portfolio <- add.constraint(
    moderate_portfolio,
    type = "diversification", min_diversification = 0.1
  )
  moderate_portfolio <- add.constraint(
    moderate_portfolio,
    type = "long_only"
  )
  moderate_portfolio <- add.constraint(
    moderate_portfolio,
    type = "box", min = 0.001, max = moderate_max_weight
  )

  # Optimize the moderate portfolio
  moderate_optimum_portfolio <-
    optimize.portfolio(
      R = returns[, moderate_filtered_assets],
      portfolio = moderate_portfolio, optimize_method = "ROI"
    )

  # Convert weights to a dataframe
  moderate_weights_df <- as.data.frame(moderate_optimum_portfolio$weights)
  moderate_weights_df <- data.frame(
    Tickers = rownames(moderate_weights_df),
    Weights = moderate_weights_df[, 1]
  )

  # Print weights
  print("Moderate Portfolio Weights:")
  print(moderate_weights_df)
} else {
  print("No assets passed the moderate Sharpe ratio threshold.")
}

sum(moderate_weights_df) # ! Has to be 1
View(moderate_weights_df)

# Calculate portfolio returns
moderate_portfolio_returns <- Return.portfolio(
  R = returns[, moderate_filtered_assets],
  weights = moderate_optimum_portfolio$weights
)

# Print mean return (daily)
mean_return_daily <- mean(moderate_portfolio_returns)
print(paste("Mean daily return:", mean_return_daily))

# Calculate and print mean return (annualized)
mean_return_annualized <- (1 + mean_return_daily)^252 - 1
print(paste("Mean annualized return:", mean_return_annualized))

# Print standard deviation (daily)
std_dev <- sd(moderate_portfolio_returns)
print(paste("Standard deviation (daily):", std_dev))

# Calculate and print standard deviation (annualized)
std_dev_annualized <- std_dev * sqrt(252)
print(paste("Standard deviation (annualized):", std_dev_annualized))

# Print Sharpe Ratio
sharpe_ratio <- SharpeRatio(moderate_portfolio_returns, Rf = 0) # Assuming a risk-free rate of 0
print(paste("Sharpe Ratio:", sharpe_ratio))

# Calculate and print Sortino Ratio
sortino_ratio <- SortinoRatio(moderate_portfolio_returns, Rf = 0) # Assuming a risk-free rate of 0
print(paste("Sortino Ratio:", sortino_ratio))

# Calculate and print Maximum Drawdown
max_drawdown <- maxDrawdown(moderate_portfolio_returns)
print(paste("Maximum Drawdown:", max_drawdown))

# *** CONSERVATIVE PORTFOLIO *** ----------------------------------------------

# Conservative Portfolio with Additional Metrics
conservative_threshold <- 0.01979905
conservative_max_weight <- 0.01

# Filter assets based on the Sharpe ratio threshold
conservative_filtered_assets <-
  names(sharpe_ratios)[sharpe_ratios > conservative_threshold]

if (length(conservative_filtered_assets) > 0) {
  # Create a conservative portfolio specification
  conservative_portfolio <- portfolio.spec(
    assets = conservative_filtered_assets
  )

  # Add objectives
  conservative_portfolio <- add.objective(
    conservative_portfolio,
    type = "return", name = "mean", enabled = TRUE
  )
  conservative_portfolio <- add.objective(
    conservative_portfolio,
    type = "risk", name = "StdDev", enabled = TRUE
  )
  conservative_portfolio <- add.objective(
    conservative_portfolio,
    type = "JensenAlpha", name = "JensenAlpha", enabled = TRUE
  )
  conservative_portfolio <- add.objective(
    conservative_portfolio,
    type = "TreynorRatio", name = "TreynorRatio", enabled = TRUE
  )

  # Add constraints
  conservative_portfolio <- add.constraint(
    conservative_portfolio,
    type = "diversification", min_diversification = 0.1
  )
  conservative_portfolio <- add.constraint(
    conservative_portfolio,
    type = "long_only"
  )
  conservative_portfolio <- add.constraint(
    conservative_portfolio,
    type = "box", min = 0.001, max = conservative_max_weight
  )

  # Optimize the conservative portfolio
  conservative_optimum_portfolio <- optimize.portfolio(
    R = returns[, conservative_filtered_assets],
    portfolio = conservative_portfolio, optimize_method = "ROI"
  )

  # Convert weights to a dataframe
  conservative_weights_df <- as.data.frame(
    conservative_optimum_portfolio$weights
  )
  conservative_weights_df <- data.frame(
    Tickers = rownames(conservative_weights_df),
    Weights = conservative_weights_df[, 1]
  )

  # Print weights
  print("Conservative Portfolio Weights:")
  print(conservative_weights_df)
} else {
  print("No assets passed the conservative Sharpe ratio threshold.") # ! Fix
}

sum(conservative_weights_df) # ! Has to be 1
View(conservative_weights_df)

# Calculate portfolio returns
conservative_portfolio_returns <- Return.portfolio(
  R = returns[, conservative_filtered_assets],
  weights = conservative_optimum_portfolio$weights
)

# Print mean return (daily)
mean_return_daily <- mean(conservative_portfolio_returns)
print(paste("Mean daily return:", mean_return_daily))

# Calculate and print mean return (annualized)
mean_return_annualized <- (1 + mean_return_daily)^252 - 1
print(paste("Mean annualized return:", mean_return_annualized))

# Print standard deviation (daily)
std_dev <- sd(conservative_portfolio_returns)
print(paste("Standard deviation (daily):", std_dev))

# Calculate and print standard deviation (annualized)
std_dev_annualized <- std_dev * sqrt(252)
print(paste("Standard deviation (annualized):", std_dev_annualized))

# Print Sharpe Ratio
sharpe_ratio <- SharpeRatio(conservative_portfolio_returns, Rf = 0) # Assuming a risk-free rate of 0
print(paste("Sharpe Ratio:", sharpe_ratio))

# Calculate and print Sortino Ratio
sortino_ratio <- SortinoRatio(conservative_portfolio_returns, Rf = 0) # Assuming a risk-free rate of 0
print(paste("Sortino Ratio:", sortino_ratio))

# Calculate and print Maximum Drawdown
max_drawdown <- maxDrawdown(conservative_portfolio_returns)
print(paste("Maximum Drawdown:", max_drawdown))


# *** PORTFOLIO METRICS WITH WEIGHTS *** --------------------------------------

# * Aggressive
# Extract selected tickers from aggressive_weights_df
tickers_aggressive <- aggressive_weights_df$Tickers

# Filter portfolio_adjusted_metrics based on selected tickers
portfolio_aggressive <- portfolio_adjusted_metrics[
  portfolio_adjusted_metrics$Tickers %in% tickers_aggressive,
]

# Merge with aggressive_weights_df to add weights
portfolio_aggressive <- merge(
  portfolio_aggressive, aggressive_weights_df,
  by.x = "Tickers", by.y = "Tickers", all.x = TRUE
)

# Reorder the columns
portfolio_aggressive <-
  portfolio_aggressive[, c(
    1, ncol(portfolio_aggressive),
    2:(ncol(portfolio_aggressive) - 1)
  )]

View(portfolio_aggressive)

# * Moderate
# Extract selected tickers from moderate_weights_df
tickers_moderate <- moderate_weights_df$Tickers

# Filter portfolio_adjusted_metrics based on selected tickers
portfolio_moderate <- portfolio_adjusted_metrics[
  portfolio_adjusted_metrics$Tickers %in% tickers_moderate,
]

# Merge with moderate_weights_df to add weights
portfolio_moderate <- merge(
  portfolio_moderate, moderate_weights_df,
  by.x = "Tickers", by.y = "Tickers", all.x = TRUE
)

# Reorder the columns
portfolio_moderate <-
  portfolio_moderate[, c(
    1, ncol(portfolio_moderate),
    2:(ncol(portfolio_moderate) - 1)
  )]

View(portfolio_moderate)


# * Conservative
# Extract selected tickers from conservative_weights_df
tickers_conservative <- conservative_weights_df$Tickers

# Filter portfolio_adjusted_metrics based on selected tickers
portfolio_conservative <- portfolio_adjusted_metrics[
  portfolio_adjusted_metrics$Tickers %in% tickers_conservative,
]

# Merge with conservative_weights_df to add weights
portfolio_conservative <- merge(
  portfolio_conservative, conservative_weights_df,
  by.x = "Tickers", by.y = "Tickers", all.x = TRUE
)

# Reorder the columns
portfolio_conservative <-
  portfolio_conservative[, c(
    1, ncol(portfolio_conservative),
    2:(ncol(portfolio_conservative) - 1)
  )]

View(portfolio_conservative)


# *** OBTAIN WEIGHTS (MANUAL APPROACH) *** ------------------------------------

# calculate_weights <- function(df) {
#   # Normalize the metrics
#   normalize <- function(x) (x - min(x)) / (max(x) - min(x))
#   average_norm <- normalize(df$average)

#   # Inverse normalization because lower variance is better
#   variance_norm <- 1 - normalize(df$variance)

#   # Inverse normalization because lower std_deviation is better
#   std_deviation_norm <- 1 - normalize(df$std_deviation)

#   sharpe_norm <- normalize(df$sharpe)

#   # Inverse normalization because lower betas is better
#   betas_norm <- 1 - normalize(df$betas)

#   r_squared_norm <- normalize(df$r_squared)
#   treynor_norm <- normalize(df$treynor)

#   # Calculate scores for each profile
#   score_aggressive <- rowMeans(cbind(
#     average_norm, sharpe_norm, betas_norm, r_squared_norm, treynor_norm
#   )) # Higher risk, higher return
#   score_moderate <- rowMeans(cbind(
#     average_norm, variance_norm, std_deviation_norm, sharpe_norm,
#     betas_norm, r_squared_norm, treynor_norm
#   )) # Balanced
#   score_conservative <- rowMeans(cbind(
#     variance_norm, std_deviation_norm, betas_norm, r_squared_norm
#   )) # Lower risk

#   # Calculate weights
#   weights_aggressive <- score_aggressive / sum(score_aggressive)
#   weights_moderate <- score_moderate / sum(score_moderate)
#   weights_conservative <- score_conservative / sum(score_conservative)

#   # Add weights as the new second column
#   df_aggressive <- cbind(
#     df[, 1, drop = FALSE], weights_aggressive, df[, -1]
#   )
#   df_moderate <- cbind(
#     df[, 1, drop = FALSE], weights_moderate, df[, -1]
#   )
#   df_conservative <- cbind(
#     df[, 1, drop = FALSE], weights_conservative, df[, -1]
#   )

#   return(list(
#     aggressive = df_aggressive,
#     moderate = df_moderate,
#     conservative = df_conservative
#   ))
# }

# # Use the function
# portfolio_profiles <- calculate_weights(portfolio_adjusted_metrics)

# portfolio_aggressive <- portfolio_profiles$aggressive
# portfolio_moderate <- portfolio_profiles$moderate
# portfolio_conservative <- portfolio_profiles$conservative

# sum(portfolio_aggressive$weights_aggressive) # ! Has to be 1
# View(portfolio_aggressive)

# sum(portfolio_moderate$weights_moderate) # ! Has to be 1
# View(portfolio_moderate)

# sum(portfolio_conservative$weights_conservative) # ! Has to be 1
# View(portfolio_conservative)

# *** SECTOR AVERAGE VALUATION RATIOS *** -------------------------------------

# There are 11 sectors in the SP500
unique(sp500$GICS.Sector)

# Get all valuation ratios for the SP500
valuation_ratios_sp500 <- getQuote(
  sp500$Tickers,
  what = yahooQF(c(
    "Price/Book", # Stock against company assets
    "P/E Ratio", # Stock against last earnings report
    "Price/EPS Estimate Current Year", # Stock/Current year's earnings estimate
    "Price/EPS Estimate Next Year" # Stock/Next-year earnings estimate
  ))
)

# Convert row names to a column
valuation_ratios_sp500 <- rownames_to_column(
  valuation_ratios_sp500, "Tickers"
)

# Merge the two dataframes based on the "Tickers" column
valuation_ratios_sp500 <- merge(
  valuation_ratios_sp500, sp500[, c("Tickers", "GICS.Sector")],
  by = "Tickers", all.x = TRUE
)

View(valuation_ratios_sp500)

# Remove columns "Tickers" and "Trade Time" from valuation_ratios_sp500
valuation_ratios_sp500 <- valuation_ratios_sp500[, !(colnames(
  valuation_ratios_sp500
) %in% c("Tickers", "Trade Time"))]

# Group by sector and calculate the average valuation ratios
valuation_ratios_sector <- valuation_ratios_sp500 %>%
  group_by(GICS.Sector) %>%
  summarise_all(mean, na.rm = TRUE)

# Rename GICS.Sector to Sector
names(valuation_ratios_sector)[1] <- "Sector"

View(valuation_ratios_sector)


# *** PORTFOLIO VALUATION RATIOS *** ------------------------------------------
# TODO: Contribute to the quantmod package to get more ratios
# TODO: Optimize

# * Valuation ratios for the aggressive profile
# Get the valuation ratios, consult quantmod documentation
valuation_ratios_aggressive <- getQuote(
  portfolio_aggressive$Tickers,
  what = yahooQF(c(
    "Price/Book", # Stock against company assets
    "P/E Ratio", # Stock against last earnings report
    "Price/EPS Estimate Current Year", # Stock/Current year's earnings estimate
    "Price/EPS Estimate Next Year" # Stock/Next-year earnings estimate
  ))
)

# Convert row names to a column
valuation_ratios_aggressive <- rownames_to_column(
  valuation_ratios_aggressive, "Tickers"
)

View(valuation_ratios_aggressive)

# * Valuation ratios for the moderate profile
# Get the valuation ratios, consult quantmod documentation
valuation_ratios_moderate <- getQuote(
  portfolio_moderate$Tickers,
  what = yahooQF(c(
    "Price/Book", # Stock against company assets
    "P/E Ratio", # Stock against last earnings report
    "Price/EPS Estimate Current Year", # Stock/Current year's earnings estimate
    "Price/EPS Estimate Next Year" # Stock/Next-year earnings estimate
  ))
)

# Convert row names to a column
valuation_ratios_moderate <- rownames_to_column(
  valuation_ratios_moderate, "Tickers"
)

View(valuation_ratios_moderate)

# * Valuation ratios for the conservative profile
# Get the valuation ratios, consult quantmod documentation
valuation_ratios_conservative <- getQuote(
  portfolio_conservative$Tickers,
  what = yahooQF(c(
    "Price/Book", # Stock against company assets
    "P/E Ratio", # Stock against last earnings report
    "Price/EPS Estimate Current Year", # Stock/Current year's earnings estimate
    "Price/EPS Estimate Next Year" # Stock/Next-year earnings estimate
  ))
)

# Convert row names to a column
valuation_ratios_conservative <- rownames_to_column(
  valuation_ratios_conservative, "Tickers"
)

View(valuation_ratios_conservative)


# *** ABOVE OR BELOW AVERAGE *** ----------------------------------------------

library(dplyr)

above_below <- function(df) {
  # Get the names of the numeric columns in the dataframe
  numeric_cols <- names(df)[sapply(df, is.numeric)]

  # Convert "Invalid Number" to NA in the numeric columns
  df[numeric_cols][df[numeric_cols] == "Invalid Number"] <- NA

  # Use na.rm = TRUE to ignore NA values when calculating the mean
  df$PB_Status <- ifelse(df$`Price/Book` > mean(
    df$`Price/Book`,
    na.rm = TRUE
  ), "Above Average", "Below Average")

  df$PE_Status <- ifelse(df$`P/E Ratio` > mean(
    df$`P/E Ratio`,
    na.rm = TRUE
  ), "Above Average", "Below Average")

  df$PE_ECY_Status <- ifelse(
    df$`Price/EPS Estimate Current Year` > mean(
      df$`Price/EPS Estimate Current Year`,
      na.rm = TRUE
    ), "Above Average", "Below Average"
  )

  df$PE_ENY_Status <- ifelse(df$`Price/EPS Estimate Next Year` > mean(
    df$`Price/EPS Estimate Next Year`,
    na.rm = TRUE
  ), "Above Average", "Below Average")

  # Reorder the columns
  df <- df %>% select(1, 2, 3, 7, 4, 8, 5, 9, 6, 10)

  # Remove the second column (not actually needed)
  df <- df %>% select(-2)
  return(df)
}

# * valuation_ratios_aggressive
valuation_ratios_aggressive <- above_below(
  valuation_ratios_aggressive
)

View(valuation_ratios_aggressive)

# * valuation_ratios_moderate
valuation_ratios_moderate <- above_below(
  valuation_ratios_moderate
)

View(valuation_ratios_moderate)

# * valuation_ratios_conservative
valuation_ratios_conservative <- above_below(
  valuation_ratios_conservative
)

View(valuation_ratios_conservative)

# *** FINAL TICKERS *** -------------------------------------------------------

# Combine the Tickers columns from the three dataframes
tickers <- c(
  portfolio_aggressive$Tickers,
  portfolio_moderate$Tickers,
  portfolio_conservative$Tickers
)

# Remove duplicates
unique_tickers <- unique(tickers)

# Create the final_tickers dataframe
final_tickers <- data.frame(Tickers = unique_tickers)


# *** PORTFOLIO ADJUSTED FILTERED *** -----------------------------------------

# Filter portfolio_adjusted to only include the tickers in final_tickers
portfolio_adjusted <- portfolio_adjusted[, colnames(
  portfolio_adjusted
) %in% final_tickers$Tickers]

ncol(portfolio_adjusted) # 291

View(portfolio_adjusted)


# *** SPLIT DATAFRAMES *** ----------------------------------------------------

generate_tsa_dfs <- function(df) {
  # Initialize an empty list to store the dataframes
  df_list <- list()

  # Loop over the columns in the dataframe
  for (col_name in names(df)) {
    # Create a new dataframe with a single column and the original index
    new_df <- data.frame(cbind(row.names(df), df[[col_name]]),
      check.names = FALSE
    )

    # Set the column names of the new dataframe
    names(new_df) <- c("index", col_name)

    # Convert the column data back to numeric
    new_df[[col_name]] <- as.numeric(as.character(new_df[[col_name]]))

    # Set the row names of the new dataframe to the original index
    row.names(new_df) <- new_df$index
    new_df$index <- NULL

    # Add the new dataframe to the list
    df_list[[paste0(col_name, "_tsa")]] <- new_df
  }

  # Return the list of dataframes
  return(df_list)
}

# Use the function to generate the dataframes based on the portfolio_adjusted
tsa_dfs <- generate_tsa_dfs(portfolio_adjusted)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs, envir = .GlobalEnv)

# View(AAPL_tsa)
class(AAPL_tsa$AAPL) # Has to be numeric!


# # *** Last Data (ld) *** ----------------------------------------------------
# ! Not useful at all (or is it? ... nah)

# # * Calculate the method data
# # Creates a column for the method
# cols_ld <- function(df) {
#   # Create the "last_data" columns
#   last_data <- df %>% mutate( # nolint
#     across(everything(), lag, .names = "{.col}_last_data") # nolint
#   )

#   # Get the names of the original and 'last_data' columns
#   original_cols <- names(df)
#   last_data_cols <- grep("_last_data$", names(last_data), value = TRUE)

#   # Interleave the names of the original and 'last_data' columns
#   column_order <- c(rbind(
#     matrix(original_cols, nrow = 1),
#     matrix(last_data_cols, nrow = 1)
#   ))

#   # Reorder the columns in the 'last_data' dataframe
#   last_data <- last_data[, column_order]

#   # Return the modified dataframe
#   return(last_data)
# }

# # Apply the function to each dataframe
# tsa_dfs_ld <- lapply(tsa_dfs, cols_ld)

# # Create variables in the environment for each dataframe in the list
# list2env(tsa_dfs_ld, envir = .GlobalEnv)

# # View(AAPL_tsa)

# # * Error
# # Creates a column for the method
# cols_error_ld <- function(df) {
#   # Get the names of the first two columns
#   col1 <- names(df)[1]
#   col2 <- names(df)[2]

#   # Create the 'last_data_error' column
#   df[[paste0(col1, "_last_data_error")]] <- abs(df[[col1]] - df[[col2]])

#   # Return the modified dataframe
#   return(df)
# }

# # Apply the function to each dataframe
# tsa_dfs_error_ld <- lapply(tsa_dfs_ld, cols_error_ld)

# # Create variables in the environment for each dataframe in the list
# list2env(tsa_dfs_error_ld, envir = .GlobalEnv)

# # View(AAPL_tsa)

# # * Error Percentage
# # Define a function to create 'last_data_error_pct' columns
# cols_error_pct_ld <- function(df) {
#   # Get the name of the original column
#   original_col <- names(df)[1]

#   # Create the 'last_data_error_pct' column
#   df[[paste0(original_col, "_last_data_error_pct")]] <-
#     abs((df[[original_col]] - df[[paste0(original_col, "_last_data")]]) /
#       df[[original_col]]) * 100

#   # Return the modified dataframe
#   return(df)
# }

# # Apply the function to each dataframe
# tsa_dfs_error_pct_ld <- lapply(
#   tsa_dfs_error_ld, cols_error_pct_ld
# )

# # Create variables in the environment for each dataframe in the list
# list2env(tsa_dfs_error_pct_ld, envir = .GlobalEnv)

# # View(AAPL_tsa)


# *** Simple Average (sa) *** -------------------------------------------------

# * Calculate the method data
# Define a function to create 'simple_average' columns
cols_sa <- function(df) {
  # Get the name of the first column
  col1 <- names(df)[1]

  # Create the 'simple_average' column
  df[[paste0(col1, "_simple_average")]] <-
    cumsum(df[[col1]]) / seq_along(df[[col1]])

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_sa <- lapply(
  tsa_dfs, cols_sa
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_sa, envir = .GlobalEnv)

# View(AAPL_tsa)

# * Error
# Define a function to create 'simple_average_error' columns
cols_error_sa <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'simple_average_error' column
  df[[paste0(original_col, "_simple_average_error")]] <-
    abs(df[[original_col]] - df[[paste0(original_col, "_simple_average")]])

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_error_sa <- lapply(
  tsa_dfs_sa, cols_error_sa
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_error_sa, envir = .GlobalEnv)

# # View(AAPL_tsa)

# * Error Percentage
# Define a function to create 'simple_average_error_pct' columns
cols_error_pct_sa <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'simple_average_error_pct' column
  df[[paste0(original_col, "_simple_average_error_pct")]] <-
    abs((df[[original_col]] - df[[paste0(
      original_col, "_simple_average"
    )]]) / df[[original_col]]) * 100

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_error_pct_sa <- lapply(
  tsa_dfs_error_sa, cols_error_pct_sa
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_error_pct_sa, envir = .GlobalEnv)

# View(AAPL_tsa)


# *** FUNCTION | Moving Average n = 50 (ma_n50) *** ---------------------------
# * Calculate the method data
# ? Uses zoo

# Define a function to create 'moving_average_n50' columns
cols_ma_n50 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'moving_average_n50' column
  df[[paste0(original_col, "_moving_average_n50")]] <-
    rollapply(df[[original_col]], # nolint
      width = 50, FUN = mean, align = "right",
      fill = NA
    )

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_ma_n50 <- lapply(
  tsa_dfs_error_pct_sa, cols_ma_n50
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_ma_n50, envir = .GlobalEnv)

# View(AAPL_tsa)

# * Error
# Define a function to create 'moving_average_n50_error' columns
cols_error_ma_n50 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'moving_average_n50_error' column
  df[[paste0(
    original_col, "_moving_average_n50_error"
  )]] <- abs(df[[original_col]] - df[[paste0(
    original_col, "_moving_average_n50"
  )]])

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_error_ma_n50 <- lapply(
  tsa_dfs_ma_n50, cols_error_ma_n50
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_error_ma_n50, envir = .GlobalEnv)

# View(AAPL_tsa)

# * Error Percentage
# Define a function to create 'moving_average_error_pct' columns
cols_error_pct_ma_n50 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'moving_average_n50_error_pct' column
  df[[paste0(original_col, "_moving_average_n50_error_pct")]] <-
    abs((df[[original_col]] - df[[paste0(
      original_col, "_moving_average_n50"
    )]]) / df[[original_col]]) * 100

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_error_pct_ma_n50 <- lapply(
  tsa_dfs_error_ma_n50, cols_error_pct_ma_n50
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_error_pct_ma_n50, envir = .GlobalEnv)

# View(AAPL_tsa)


# *** Moving Average n = 200 (ma_n200) *** ------------------------------------

# * Calculate the method data
# Define a function to create 'moving_average_n200' columns
cols_ma_n200 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'moving_average_n200' column
  df[[paste0(original_col, "_moving_average_n200")]] <-
    rollapply(df[[original_col]], # nolint
      width = 200, FUN = mean, align = "right",
      fill = NA
    )

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_ma_n200 <- lapply(
  tsa_dfs_error_pct_ma_n50, cols_ma_n200
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_ma_n200, envir = .GlobalEnv)

# View(AAPL_tsa)

# * Error
# Define a function to create 'moving_average_n200_error' columns
cols_error_ma_n200 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'moving_average_n200_error' column
  df[[paste0(original_col, "_moving_average_n200_error")]] <-
    abs(df[[original_col]] - df[[paste0(
      original_col, "_moving_average_n200"
    )]])

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_error_ma_n200 <- lapply(
  tsa_dfs_ma_n200, cols_error_ma_n200
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_error_ma_n200, envir = .GlobalEnv)

# View(AAPL_tsa)

# * Error Percentage
# Define a function to create 'moving_average_n200_error_pct' columns
cols_error_pct_ma_n200 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'moving_average_n200_error_pct' column
  df[[paste0(original_col, "_moving_average_n200_error_pct")]] <-
    abs((df[[original_col]] - df[[paste0(
      original_col, "_moving_average_n200"
    )]]) / df[[original_col]]) * 100

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_error_pct_ma_n200 <- lapply(
  tsa_dfs_error_ma_n200, cols_error_pct_ma_n200
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_error_pct_ma_n200, envir = .GlobalEnv)

# View(AAPL_tsa)


# *** Weighted Moving Average n = 50 (wma_n50) *** ----------------------------

# * Calculate the method data
# Define a function to create 'weighted_moving_average_n50' columns
cols_wma_n50 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the weights
  weights <- seq(30, 1, length.out = 50)

  # Create the 'weighted_moving_average_n50' column
  df[[paste0(original_col, "_weighted_moving_average_n50")]] <-
    rollapply(df[[original_col]], # nolint
      width = 50, FUN = function(x) sum(x * weights) / sum(weights),
      align = "right", fill = NA
    )

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_wma_n50 <- lapply(
  tsa_dfs_error_pct_ma_n200, cols_wma_n50
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_wma_n50, envir = .GlobalEnv)

# View(AAPL_tsa)

# * Error
# Define a function to create 'weighted_moving_average_n50_error' columns
cols_error_wma_n50 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'weighted_moving_average_n50_error' column
  df[[paste0(original_col, "_weighted_moving_average_n50_error")]] <-
    abs(df[[original_col]] - df[[paste0(
      original_col, "_weighted_moving_average_n50"
    )]])

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_error_wma_n50 <- lapply(
  tsa_dfs_wma_n50, cols_error_wma_n50
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_error_wma_n50, envir = .GlobalEnv)

# # View(AAPL_tsa)

# * Error Percentage
# Define a function to create 'weighted_moving_average_n50_error_pct' columns
cols_error_pct_wma_n50 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'weighted_moving_average_n50_error_pct' column
  df[[paste0(original_col, "_weighted_moving_average_n50_error_pct")]] <-
    abs((df[[original_col]] - df[[paste0(
      original_col, "_weighted_moving_average_n50"
    )]]) / df[[original_col]]) * 100

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_error_pct_wma_n50 <- lapply(
  tsa_dfs_error_wma_n50,
  cols_error_pct_wma_n50
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_error_pct_wma_n50, envir = .GlobalEnv)

# View(AAPL_tsa)


# *** FUNCTION | Weighted Moving Average n = 200 (wma_n200) *** ---------------

# * Calculate the method data
# Define a function to create 'weighted_moving_average_n200' columns
cols_wma_n200 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the weights
  weights <- seq(30, 1, length.out = 200)

  # Create the 'weighted_moving_average_n200' column
  df[[paste0(original_col, "_weighted_moving_average_n200")]] <-
    rollapply(df[[original_col]], # nolint
      width = 200, FUN = function(x) sum(x * weights) / sum(weights),
      align = "right", fill = NA
    )

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_wma_n200 <- lapply(
  tsa_dfs_error_pct_wma_n50,
  cols_wma_n200
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_wma_n200, envir = .GlobalEnv)

# View(AAPL_tsa)

# * Error
# Define a function to create 'weighted_moving_average_n200_error' columns
cols_error_wma_n200 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'weighted_moving_average_n200_error' column
  df[[paste0(original_col, "_weighted_moving_average_n200_error")]] <-
    abs(df[[original_col]] - df[[paste0(
      original_col, "_weighted_moving_average_n200"
    )]])

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_error_wma_n200 <- lapply(
  tsa_dfs_wma_n200, cols_error_wma_n200
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_error_wma_n200, envir = .GlobalEnv)

# View(AAPL_tsa)

# * Error Percentage
# Define a function to create 'weighted_moving_average_n200_error_pct' columns
cols_error_pct_wma_n200 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'weighted_moving_average_n200_error_pct' column
  df[[paste0(original_col, "_weighted_moving_average_n200_error_pct")]] <-
    abs((df[[original_col]] - df[[paste0(
      original_col, "_weighted_moving_average_n200"
    )]]) / df[[original_col]]) * 100

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_error_pct_wma_n200 <- lapply(
  tsa_dfs_error_wma_n200,
  cols_error_pct_wma_n200
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_error_pct_wma_n200, envir = .GlobalEnv)

# View(AAPL_tsa)


# *** Exponential Smoothing alpha = 0.1 (es_a01) *** --------------------------

# * Calculate the method data
# ? Uses stats package
# Define a function to create 'exponential_smoothing_a0.1' columns
cols_es_a01 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'exponential_smoothing_a0.1' column
  df[[paste0(original_col, "_exponential_smoothing_a0.1")]] <-
    stats::filter(df[[original_col]], filter = 0.1, method = "recursive")

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_es_a01 <- lapply(
  tsa_dfs_error_pct_wma_n200,
  cols_es_a01
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_es_a01, envir = .GlobalEnv)

# View(AAPL_tsa)

# * Error
# Define a function to create 'exponential_smoothing_a0.1_error' columns
cols_error_es_a01 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'exponential_smoothing_a0.1_error' column
  df[[paste0(original_col, "_exponential_smoothing_a0.1_error")]] <-
    abs(df[[original_col]] - df[[paste0(
      original_col, "_exponential_smoothing_a0.1"
    )]])

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_error_es_a01 <- lapply(
  tsa_dfs_es_a01, cols_error_es_a01
)

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_error_es_a01, envir = .GlobalEnv)

# View(AAPL_tsa)

# * Error Percentage
# Define a function to create 'exponential_smoothing_a0.1_error_pct' columns
cols_error_pct_es_a01 <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'exponential_smoothing_a0.1_error_pct' column
  df[[paste0(original_col, "_exponential_smoothing_a0.1_error_pct")]] <-
    abs((df[[original_col]] - df[[paste0(
      original_col, "_exponential_smoothing_a0.1"
    )]]) / df[[original_col]]) * 100

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_error_pct_es_a01 <- lapply(
  tsa_dfs_error_es_a01,
  cols_error_pct_es_a01
)

tsa_dfs_error_pct <- tsa_dfs_error_pct_es_a01 # ! Final ver.

# Create variables in the environment for each dataframe in the list
list2env(tsa_dfs_error_pct, envir = .GlobalEnv) # ! Final version

View(AAPL_tsa)


# # *** Exponential Smoothing alpha = 0.9 (es_a09) *** ------------------------
# # ! Surprisingly bad!!!

# # * Calculate the method data
# # Define a function to create 'exponential_smoothing_a0.9' columns
# cols_es_a09 <- function(df) {
#   # Get the name of the original column
#   original_col <- names(df)[1]

#   # Create the 'exponential_smoothing_a0.9' column
#   df[[paste0(original_col, "_exponential_smoothing_a0.9")]] <-
#     stats::filter(df[[original_col]], filter = 0.9, method = "recursive")

#   # Return the modified dataframe
#   return(df)
# }

# # Apply the function to each dataframe
# tsa_dfs_es_a09 <- lapply(
#   tsa_dfs_exponential_smoothing_a0_1_error_pct,
#   cols_es_a09
# )

# # Create variables in the environment for each dataframe in the list
# list2env(tsa_dfs_es_a09, envir = .GlobalEnv)

# # View(AAPL_tsa)

# # * Error
# # Define a function to create 'exponential_smoothing_a0.9_error' columns
# cols_error_es_a09 <- function(df) {
#   # Get the name of the original column
#   original_col <- names(df)[1]

#   # Create the 'exponential_smoothing_a0.9_error' column
#   df[[paste0(original_col, "_exponential_smoothing_a0.9_error")]] <-
#     abs(df[[original_col]] - df[[paste0(
#       original_col, "_exponential_smoothing_a0.9"
#     )]])

#   # Return the modified dataframe
#   return(df)
# }

# # Apply the function to each dataframe
# tsa_dfs_error_es_a09 <- lapply(
#   tsa_dfs_es_a09, cols_error_es_a09
# )

# # Create variables in the environment for each dataframe in the list
# list2env(tsa_dfs_error_es_a09, envir = .GlobalEnv)

# # View(AAPL_tsa)

# # * Error Percentage
# # Define a function to create 'exponential_smoothing_a0.9_error_pct' columns
# cols_error_pct_es_a09 <- function(df) {
#   # Get the name of the original column
#   original_col <- names(df)[1]

#   # Create the 'exponential_smoothing_a0.9_error_pct' column
#   df[[paste0(original_col, "_exponential_smoothing_a0.9_error_pct")]] <-
#     abs((df[[original_col]] -
#       df[[paste0(original_col, "_exponential_smoothing_a0.9")]]) /
#       df[[original_col]]) * 100

#   # Return the modified dataframe
#   return(df)
# }

# # Apply the function to each dataframe
# tsa_dfs_error_pct_es_a09 <- lapply(
#   tsa_dfs_error_es_a09,
#   cols_error_pct_es_a09
# )

# # Create variables in the environment for each dataframe in the list
# list2env(tsa_dfs_error_pct_es_a09, envir = .GlobalEnv)

# # View(AAPL_tsa)

# # View(NVDA_tsa)


# # *** FUNCTION | arima_cols *** ---------------------------------------------
# # ? Uses forecast package
# # TODO: TAKES TOO LONG (or maybe it is wrong). Try again later

# # * Calculate the method data
# # Define a function to create 'arima' columns
# cols_arima <- function(df) {
#   # Get the name of the original column
#   original_col <- names(df)[1]

#   # Initialize the 'arima' column with NA values
#   df[[paste0(original_col, "_arima")]] <- NA

#   # Generate a rolling forecast
#   for (i in nrow(df):1) {
#     # Fit an ARIMA model to the data up to the current point
#     arima_model <- forecast::auto.arima(df[1:i, original_col])

#     # Forecast the next value
#     df[i, paste0(original_col, "_arima")] <- ifelse(i < nrow(df),
#       forecast::forecast(arima_model, h = 1)$mean, NA
#     )
#   }

#   # Return the modified dataframe
#   return(df)
# }

# # Apply the function to each dataframe in 'tsa_dfs_exponential_smoothing_a0_9'
# tsa_dfs_arima <- lapply(
#   tsa_dfs_error_pct_es_a09, cols_arima
# )

# # Create variables in the environment for each dataframe in the list
# list2env(tsa_dfs_arima, envir = .GlobalEnv)

# # View(AAPL_tsa)


# *** VISUALIZE DIFFERENCES *** -----------------------------------------------

# Get the data for AAPL
vd <- AAPL_tsa

# Convert row names to a column
vd <- vd %>% rownames_to_column("Date")

# colnames(vd)

# Convert the data to long format for plotting
vd_long <- vd %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, 2, 3, 6, 9, 12, 15, 18) %>% # Select columns by their positions
  pivot_longer(
    cols = -Date, # Exclude the Date column
    names_to = "Variable",
    values_to = "Value"
  )

visualize_differences <- ggplot(vd_long, aes(
  x = Date, y = Value, color = Variable
)) +
  geom_line(linewidth = 0.5, aes(linetype = Variable == names(vd)[1])) +
  scale_linetype_manual(values = c("solid", rep("dashed", 7)), guide = "none") +
  scale_color_manual(values = c(
    "red", "blue", "green", "orange",
    "purple", "yellow", "brown", "pink"
  )) +
  labs(x = "Date", y = "Value", color = "Variable") +
  theme_linedraw() +
  theme(legend.position = c(.15, .85)) # Legend inside the plot

# Save the plot
ggsave("./assets/tsa_plots/differences.jpg", visualize_differences,
  width = 16, height = 9
)

# Ctrl + P > differences.jpg


# *** FUNCTION | portfolio_tsa *** --------------------------------------------

# Initialize an empty dataframe
portfolio_tsa <- data.frame(matrix(ncol = 7, nrow = 0))

# Define the column names
colnames(portfolio_tsa) <- c(
  "company", "simple_average_error_pct", "moving_average_n50_error_pct",
  "moving_average_n200_error_pct", "weighted_moving_average_n50_error_pct",
  "weighted_moving_average_n200_error_pct",
  "exponential_smoothing_a0.1_error_pct"
)

# Initialize a vector to store the overall averages for each method
overall_averages <- numeric(6)

# Fill the dataframe
for (i in seq_along(tsa_dfs_error_pct)) {
  company_name <- names(tsa_dfs_error_pct)[i]
  df <- tsa_dfs_error_pct[[i]]

  portfolio_tsa[i, "company"] <- company_name
  portfolio_tsa[i, "simple_average_error_pct"] <-
    mean(df[[4]], na.rm = TRUE)
  portfolio_tsa[i, "moving_average_n50_error_pct"] <-
    mean(df[[7]], na.rm = TRUE)
  portfolio_tsa[i, "moving_average_n200_error_pct"] <-
    mean(df[[10]], na.rm = TRUE)
  portfolio_tsa[i, "weighted_moving_average_n50_error_pct"] <-
    mean(df[[13]], na.rm = TRUE)
  portfolio_tsa[i, "weighted_moving_average_n200_error_pct"] <-
    mean(df[[16]], na.rm = TRUE)
  portfolio_tsa[i, "exponential_smoothing_a0.1_error_pct"] <-
    mean(df[[19]], na.rm = TRUE)

  # Print the best method for this company
  best_method <- which.min(portfolio_tsa[i, 2:7])
  best_value <- portfolio_tsa[i, best_method + 1]
  print(paste(
    "Best method for", company_name,
    "is", colnames(portfolio_tsa)[best_method + 1],
    "with an average error of", best_value
  ))

  # Add the averages to the overall averages
  overall_averages <- overall_averages + portfolio_tsa[i, 2:7]
}

# View(NVDA_tsa) # Erratic

# Calculate the overall averages
overall_averages <- overall_averages /
  length(tsa_dfs_error_pct)

# Print the overall best method
overall_best_method <- which.min(overall_averages)
print(paste(
  "Overall best method is", colnames(portfolio_tsa)[overall_best_method + 1],
  "with an average error of", overall_averages[overall_best_method]
))

portfolio_tsa$company <- sub("_tsa", "", portfolio_tsa$company)

View(portfolio_tsa)


# *** FUNCTION | portfolio_tsa_plots *** --------------------------------------
# ? Uses ggplot2 and tidyr

portfolio_tsa_plots <- function(tsa_dfs_error_pct) {
  for (i in seq_along(tsa_dfs_error_pct)) {
    df <- tsa_dfs_error_pct[[i]]
    company_name <- names(tsa_dfs_error_pct)[i]
    company_name <- sub("_tsa", "", company_name)

    # Determine the best method
    all_columns <- seq(3, ncol(df), by = 3) # Adjust according to col place
    avg_errors <- colMeans(df[, all_columns], na.rm = TRUE)
    best_method <- order(avg_errors)[1]
    # Use the column before the best method
    best_method_column <- all_columns[best_method] - 1
    best_method_name <- colnames(df)[best_method_column]
    # Convert the data to long format for plotting
    df_long <- df %>%
      # Get the dates with the proper format
      mutate(Date = as.Date(rownames(df))) %>%
      pivot_longer(
        cols = all_of(c(1, best_method_column)),
        names_to = "Variable",
        values_to = "Value"
      )

    # Create the plot
    plot <- ggplot(df_long, aes(x = Date, y = Value, color = Variable)) +
      geom_line() +
      scale_color_manual(values = c("#000000", "red")) +
      labs(x = "Date", y = "Value", color = "Variable") +
      ggtitle(paste(company_name, "Time Series Analysis")) +
      labs(
        subtitle = paste(
          "Evolution of", best_method_name, " (red) over the last 5 years."
        ),
        y = "Adjusted Price",
        caption = paste(
          "R Plot: @VictorBenitoGR | GitHub Repository: ",
          "VictorBenitoGR/OpenFinancialData"
        )
      ) +
      theme_ipsum() +
      theme(
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 22),
        axis.title.x = element_blank(), # Remove x-axis label
        axis.title.y = element_blank(), # Remove y-axis label
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption = element_text(size = 15),
        panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.ticks = element_blank(),
        legend.position = "none"
      )

    # Save the plot
    ggsave(
      paste0("./assets/tsa_plots/", Sys.Date(), "/", company_name, ".jpg"),
      plot,
      width = 16, height = 9
    )
  }
}

# Call the function
portfolio_tsa_plots(tsa_dfs_error_pct)

# Ctrl + P > 11/AAPL.jpg


# *** TIME SERIES FORECASTING *** ---------------------------------------------

# ? In my opinion, it doesn't make sense to calculate the expected returns
# ? of the portfolio using current data. At least not for all the cases
# TODO: Soon!


# *** ADJUSTED PRICES BY PORTFOLIO *** ----------------------------------------

# Filter portfolio_adjusted to only include the tickers
adjusted_prices_aggressive <- portfolio_adjusted[, colnames(
  portfolio_adjusted
) %in% portfolio_aggressive$Tickers]

adjusted_prices_moderate <- portfolio_adjusted[, colnames(
  portfolio_adjusted
) %in% portfolio_moderate$Tickers]

adjusted_prices_conservative <- portfolio_adjusted[, colnames(
  portfolio_adjusted
) %in% portfolio_conservative$Tickers]


# *** GET THE SECTOR AND COMPANY NAME *** -------------------------------------

# * Portfolio Aggressive
# Merge portfolio_aggressive with sp500
portfolio_aggressive <- merge(
  portfolio_aggressive, sp500[, c("Tickers", "Security", "GICS.Sector")],
  by = "Tickers", all.x = TRUE
)

# Reorder the columns
portfolio_aggressive <- portfolio_aggressive %>% select(
  Tickers, Security, `GICS.Sector`, everything()
)

# * Portfolio Moderate
# Merge portfolio_moderate with sp500
portfolio_moderate <- merge(
  portfolio_moderate, sp500[, c("Tickers", "Security", "GICS.Sector")],
  by = "Tickers", all.x = TRUE
)

# Reorder the columns
portfolio_moderate <- portfolio_moderate %>% select(
  Tickers, Security, `GICS.Sector`, everything()
)


# *** GET THE DATE COLUMN *** -------------------------------------------------
# ? Uses Tibble package

# Portfolio
adjusted_prices_aggressive <- rownames_to_column(
  adjusted_prices_aggressive, "Date"
)
adjusted_prices_moderate <- rownames_to_column(
  adjusted_prices_moderate, "Date"
)
adjusted_prices_conservative <- rownames_to_column(
  adjusted_prices_conservative, "Date"
)
portfolio_open <- rownames_to_column(portfolio_open, "Date")
portfolio_high <- rownames_to_column(portfolio_high, "Date")
portfolio_low <- rownames_to_column(portfolio_low, "Date")
portfolio_close <- rownames_to_column(portfolio_close, "Date")
portfolio_volume <- rownames_to_column(portfolio_volume, "Date")
portfolio_adjusted <- rownames_to_column(portfolio_adjusted, "Date")

# Make all column names unique
names(portfolio_open) <- make.names(names(portfolio_open), unique = TRUE)

# Check if 'Date' column exists
if ("Date" %in% names(portfolio_open)) {
  # Drop 'Date' column
  portfolio_open$Date <- NULL
}
# List of data frames to process
data_frames <- list(portfolio_open, portfolio_high, portfolio_low, portfolio_close, portfolio_volume, portfolio_adjusted)

# Names of the data frames
data_frame_names <- c("portfolio_open", "portfolio_high", "portfolio_low", "portfolio_close", "portfolio_volume", "portfolio_adjusted")

# Process each data frame
for (i in seq_along(data_frames)) {
  # Make all column names unique
  names(data_frames[[i]]) <- make.names(names(data_frames[[i]]), unique = TRUE)

  # Check if 'Date' column exists
  if ("Date" %in% names(data_frames[[i]])) {
    # Drop 'Date' column
    data_frames[[i]]$Date <- NULL
  }

  # Convert row names to 'Date' column
  data_frames[[i]] <- rownames_to_column(data_frames[[i]], "Date")

  # Assign the processed data frame back to the original variable
  assign(data_frame_names[i], data_frames[[i]], envir = .GlobalEnv)
}

# Convert row names to 'Date' column
portfolio_open <- rownames_to_column(portfolio_open, "Date")
View(portfolio_open)
View(portfolio_adjusted)
# Benchmark
benchmark_open <- rownames_to_column(benchmark_open, "Date")
benchmark_high <- rownames_to_column(benchmark_high, "Date")
benchmark_low <- rownames_to_column(benchmark_low, "Date")
benchmark_close <- rownames_to_column(benchmark_close, "Date")
benchmark_volume <- rownames_to_column(benchmark_volume, "Date")
benchmark_adjusted <- rownames_to_column(benchmark_adjusted, "Date")

# T-bills
tbills_df <- rownames_to_column(tbills_df, "Date")
tbills_dfTB3MS <- rownames_to_column(tbills_dfTB3MS, "Date")

# *** CHARACTER TO DATE *** ---------------------------------------------------

# ? You need to give Date its proper format
class(portfolio_adjusted$Date) # character
class(benchmark_adjusted$Date) # character
class(tbills_df$Date) # character

# Portfolio
portfolio_open$Date <- as.Date(portfolio_open$Date, format = "%Y-%m-%d")
portfolio_high$Date <- as.Date(portfolio_high$Date, format = "%Y-%m-%d")
portfolio_low$Date <- as.Date(portfolio_low$Date, format = "%Y-%m-%d")
portfolio_close$Date <- as.Date(portfolio_close$Date, format = "%Y-%m-%d")
portfolio_volume$Date <- as.Date(portfolio_volume$Date, format = "%Y-%m-%d")
portfolio_adjusted$Date <- as.Date(portfolio_adjusted$Date, format = "%Y-%m-%d")

# Benchmark
benchmark_open$Date <- as.Date(benchmark_open$Date, format = "%Y-%m-%d")
benchmark_high$Date <- as.Date(benchmark_high$Date, format = "%Y-%m-%d")
benchmark_low$Date <- as.Date(benchmark_low$Date, format = "%Y-%m-%d")
benchmark_close$Date <- as.Date(benchmark_close$Date, format = "%Y-%m-%d")
benchmark_volume$Date <- as.Date(benchmark_volume$Date, format = "%Y-%m-%d")
benchmark_adjusted$Date <- as.Date(benchmark_adjusted$Date, format = "%Y-%m-%d")

# T-bills
tbills_df$Date <- as.Date(tbills_df$Date, format = "%Y-%m-%d")
tbills_dfTB3MS$Date <- as.Date(tbills_dfTB3MS$Date, format = "%Y-%m-%d")


class(portfolio_adjusted$Date) # Date
class(benchmark_adjusted$Date) # Date
class(tbills_df$Date) # Date
class(tbills_dfTB3MS$Date) # Date

# TODO: Search if there's a difference with POSIXct

View(portfolio_adjusted)


# *** VISUALIZATION *** -------------------------------------------------------

# aapl_adjusted_plot <- ggplot(portfolio_adjusted, aes(
#   x = Date, y = AAPL
# )) +
#   geom_area_pattern(
#     data = portfolio_adjusted,
#     pattern = "gradient",
#     fill = "#00000000",
#     pattern_fill = "#00000000",
#     pattern_fill2 = "#10006b"
#   ) +
#   ggtitle("APPL price adjusted") +
#   labs(
#     subtitle = "Evolution of Apple over the last 10 years",
#     y = "Price",
#     caption = paste("R Plot: @VictorBenitoGR | GitHub Repository:",
#                    "VictorBenitoGR/OpenFinancialData")
#   ) +
#   geom_smooth(method = loess, color = "red", fill = "#69b3a2", se = TRUE) +
#   theme_ipsum() +
#   theme(
#     plot.title = element_text(size = 28),
#     plot.subtitle = element_text(size = 22),
#     axis.title.x = element_blank(), # Remove x-axis label
#     axis.title.y = element_blank(), # Remove y-axis label
#     axis.text.x = element_text(size = 20),
#     axis.text.y = element_text(size = 20),
#     plot.caption = element_text(size = 15),
#     panel.border = element_blank(),
#     axis.line.x = element_line(),
#     axis.ticks = element_blank()
#   )

# # Save the plot
# ggsave("./assets/adjusted_plot/aapl_adjusted_plot.jpg", aapl_adjusted_plot,
#   width = 16, height = 9
# )


# *** EXPORT IF NECESSARY *** ------------------------------------------------

# Function to export dataframes to an Excel file
export_to_excel <- function(file, sheet_names, dataframes, col_width = 12) {
  library(openxlsx)

  # Create a new workbook
  wb <- createWorkbook()

  # Add each dataframe to the workbook as a new sheet
  for (i in seq_along(sheet_names)) {
    addWorksheet(wb, sheet_names[i])
    writeData(
      wb,
      sheet = sheet_names[i], x = dataframes[[i]], startCol = 1, startRow = 1
    )
    # Set the column widths
    setColWidths(wb,
      sheet = sheet_names[i],
      cols = seq_len(ncol(dataframes[[i]])),
      widths = col_width
    )
  }

  # Save the workbook as xlsx file, overwrite if it already exists
  saveWorkbook(wb, file, overwrite = TRUE)
}

export_to_excel(
  paste("./data/Portfolio_Analysis_Monthly_", Sys.Date(), ".xlsx"), c(
    "Portfolio Open Prices", "Portfolio High Prices",
    "Portfolio Low Prices", "Portfolio Close Prices",
    "Portfolio Volume", "Portfolio Adjusted Prices",
    "Benchmark Open Prices", "Benchmark High Prices",
    "Benchmark Low Prices", "Benchmark Close Prices",
    "Benchmark Volume", "Benchmark Adjusted Prices",
    "3-Month T-Bills Market Yield", " 3-MonthT-BillSecondaryMktRate"
  ), list(
    portfolio_open, portfolio_high, portfolio_low,
    portfolio_close, portfolio_volume, portfolio_adjusted,
    benchmark_open, benchmark_high, benchmark_low,
    benchmark_close, benchmark_volume, benchmark_adjusted,
    tbills_df, tbills_dfTB3MS
  )
)
