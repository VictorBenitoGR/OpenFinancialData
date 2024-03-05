# * OpenFinancialData
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
# tibble        Simple data frames
# tidyverse     Data manipulation and visualization packages
# ggplot2       Data visualization
# ggpattern     Geoms for patterned filled geoms
# hrbrthemes    Opinionated, typographic-centric ggplot2 themes
# conflicted    Handling conflicts between functions in R packages

# * Install and load packages
source("./src/install_packages.R")


# *** OBTAIN TICKER SYMBOLS *** -----------------------------------------------

# ! DON'T OPEN ANYTHING THAT'S NOT A DATAFRAME IF YOUR'E USING VSCODE
# ? Uses Quantmod. Consider sponsoring the project on joshuaulrich/quantmod
# * Obtain the ticker symbols
na.omit(getSymbols(
  c(
    "EUSA", # Benchmarks (MSCI USA Equal Weighted ETF) # "^GSPC" SP500
    "AAPL", "MSFT", "NVDA", "AMZN", "META",
    "GOOGL", "GOOG", "LLY", "TSLA", "AVGO",
    "TMO", "JPM", "UNH", "V", "XOM",
    "MA", "JNJ", "PG", "HD", "MRK",
    "COST", "ABBV", "AMD", "CRM", "CVX",
    "ADBE", "NFLX", "WMT", "KO", "BAC"
  ),
  src = "yahoo",
  from = Sys.Date() - 1826, # 1826days = 5years
  to = Sys.Date()
))

# * 3-Month/90-Day T-bills
na.omit(getSymbols(
  "DGS3MO",
  src = "FRED",
  from = Sys.Date() - 1826, # ! Make sure you have 60 months
  to = Sys.Date()
))

# Benchmarks, necessary to get Beta and R2
benchmark <- list(EUSA)

# T-bills, necessary to get the Sharpe ratio
tbills <- list(DGS3MO)

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

# ! DO NOT OPEN THEM, IT WILL CRASH SINCE THEY ARE NOT DATAFRAMES
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

head(portfolio_adjusted)
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


# * T-bills
# Use lapply to convert each xts object to a data frame
tbills_df <- lapply(tbills, xts_to_df)

# Combine the data frames into a single data frame
tbills_df <- do.call(cbind, tbills_df)

# Replace 'Invalid Number' with NA
tbills_df$DGS3MO <- sub("Invalid Number", NA, tbills_df$DGS3MO)

# Transform tbills_df$DGS3MO to numeric and divide by 100 for percentage
tbills_df$DGS3MO <- as.numeric(tbills_df$DGS3MO) / 100

class(tbills_df$DGS3MO) # ! Has to be numeric!

View(tbills_df)



# *** FUNCTION | tbills_metrics *** -------------------------------------------

# Function to calculate the risk-free rate
tbills_metrics <- function(df) {
  # Calculate the average risk-free rate
  risk_free_rate <- mean(df$DGS3MO, na.rm = TRUE)

  # Return the risk-free rate
  return(risk_free_rate)
}

# Calculate the risk-free rate
risk_free_rate <- tbills_metrics(tbills_df)

View(risk_free_rate)


# *** FUNCTION | benchmark_metrics *** -----------------------------------------

# Function to calculate the market average
benchmark_metrics <- function(df) {
  # Calculate the average market price
  market_average <- mean(df$EUSA.Adjusted, na.rm = TRUE)

  # Return the market average
  return(market_average)
}
View(benchmark_adjusted)

# Calculate the market average
market_average <- benchmark_metrics(benchmark_adjusted)

View(market_average)


# *** FUNCTION | portfolio_metrics *** ----------------------------------------

# Function to calculate general metrics of a portfolio
portfolio_metrics <- function(df, benchmark_adjusted) {
  # // Remove the first column (Date index)
  # // df <- df[, -1] # If you have the Date column

  # Divide each row by the previous one and apply natural logarithm
  # ? Uses dplyr
  df <- log(df / lag(df))
  benchmark_adjusted$EUSA.Adjusted <- log(
    benchmark_adjusted$EUSA.Adjusted /
      lag(benchmark_adjusted$EUSA.Adjusted) # ! Optimize
  )

  # Replace NA and Inf values with 0
  df[is.na(df) | df == Inf] <- 0
  benchmark_adjusted$EUSA.Adjusted[ # ! Optimize
    is.na(
      benchmark_adjusted$EUSA.Adjusted # ! Optimize
    ) | benchmark_adjusted$ColumnName == Inf
  ] <- 0

  # Calculate metrics
  average <- colMeans(df, na.rm = TRUE) * 100

  variance <- apply(df, 2, var, na.rm = TRUE) * 100

  std_deviation <- apply(df, 2, sd, na.rm = TRUE) * 100

  betas <- sapply(names(df), function(col) {
    formula <- as.formula(paste(col, "~ EUSA.Adjusted")) # ! Optimize
    regression_result <- lm(formula, data = cbind(
      df,
      EUSA.Adjusted = benchmark_adjusted$EUSA.Adjusted # ! Optimize
    ))
    coef(regression_result)[2]
  })

  r_squared <- sapply(names(df), function(col) {
    formula <- as.formula(paste(col, "~ EUSA.Adjusted")) # ! Optimize
    regression_result <- lm(formula, data = cbind(
      df,
      EUSA.Adjusted = benchmark_adjusted$EUSA.Adjusted # ! Optimize
    ))
    summary(regression_result)$r.squared
  })

  sharpe <- (average - risk_free_rate) / std_deviation

  treynor <- (average - risk_free_rate) / betas

  # ? Alpha = R(i) - (R(f) + B * (R(m) - R(f)))
  # ? where:
  # ? R(i) = the realized return of the portfolio or investment
  # ? R(m) = the realized return of the appropriate market index
  # ? R(f) = the risk-free rate of return for the time period
  # ? B = the beta of the portfolio of investment

  # jensen_aplha <- average - (
  #   risk_free_rate + betas * (market_average - risk_free_rate)
  # ) # ! Need market_average

  # Metrics for each ticker
  metrics <- data.frame(
    average, variance, std_deviation, betas, r_squared, sharpe, treynor
    # , jensen_aplha
  )

  # ? Uses tibble
  # Use tickers (column names) as row names
  metrics <- rownames_to_column(metrics, "Tickers") # nolint

  return(metrics)
}

# portfolio_adjusted_metrics <- portfolio_metrics(
#   portfolio_adjusted, benchmark_adjusted
# )

# View(portfolio_adjusted_metrics)

# *** GET METRICS *** ---------------------------------------------------------
# ! Optimize

# Portfolio metrics
portfolio_open_metrics <- portfolio_metrics(
  portfolio_open, benchmark_adjusted
)

portfolio_high_metrics <- portfolio_metrics(
  portfolio_high, benchmark_adjusted
)

portfolio_low_metrics <- portfolio_metrics(
  portfolio_low, benchmark_adjusted
)

portfolio_close_metrics <- portfolio_metrics(
  portfolio_close, benchmark_adjusted
)

# portfolio_volume_metrics <- portfolio_metrics(
#   portfolio_volume, benchmark_adjusted
# )

portfolio_adjusted_metrics <- portfolio_metrics(
  portfolio_adjusted, benchmark_adjusted
)

View(portfolio_adjusted_metrics)


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

# Use the function to generate the dataframes
tsa_dfs <- generate_tsa_dfs(portfolio_adjusted)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)
class(AAPL.Adjusted_tsa$AAPL.Adjusted) # Has to be numeric!


# # *** FUNCTION | last_data_cols *** ------------------------------------------
# ! Not useful at all

# # Creates a column for the method
# last_data_cols <- function(df) {
#   # Create the "last_data" columns
#   last_data <- df %>%
#     mutate(across(everything(), lag, .names = "{.col}_last_data"))

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
# tsa_dfs_last_data <- lapply(tsa_dfs, last_data_cols)

# # Create variables in your environment for each dataframe in the list
# list2env(tsa_dfs_last_data, envir = .GlobalEnv)

# # View(AAPL.Adjusted_tsa)


# # *** FUNCTION | last_data_error_cols *** ------------------------------------
# ! Not useful at all

# # Creates a column for the method
# last_data_error_cols <- function(df) {
#   # Get the names of the first two columns
#   col1 <- names(df)[1]
#   col2 <- names(df)[2]

#   # Create the 'last_data_error' column
#   df[[paste0(col1, "_last_data_error")]] <- abs(df[[col1]] - df[[col2]])

#   # Return the modified dataframe
#   return(df)
# }

# # Apply the function to each dataframe
# tsa_dfs_last_data_error <- lapply(tsa_dfs_last_data, last_data_error_cols)

# # Create variables in your environment for each dataframe in the list
# list2env(tsa_dfs_last_data_error, envir = .GlobalEnv)

# # View(AAPL.Adjusted_tsa)


# # *** FUNCTION | last_data_error_pct_cols *** --------------------------------
# ! Not useful at all

# # Define a function to create 'last_data_error_pct' columns
# last_data_error_pct_cols <- function(df) {
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
# tsa_dfs_last_data_error_pct <- lapply(
#   tsa_dfs_last_data_error, last_data_error_pct_cols
# )

# # Create variables in your environment for each dataframe in the list
# list2env(tsa_dfs_last_data_error_pct, envir = .GlobalEnv)

# # View(AAPL.Adjusted_tsa)


# *** FUNCTION | simple_average_cols *** --------------------------------------

# Define a function to create 'simple_average' columns
simple_average_cols <- function(df) {
  # Get the name of the first column
  col1 <- names(df)[1]

  # Create the 'simple_average' column
  df[[paste0(col1, "_simple_average")]] <-
    cumsum(df[[col1]]) / seq_along(df[[col1]])

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_simple_average <- lapply(
  tsa_dfs, simple_average_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_simple_average, envir = .GlobalEnv)

# # View(AAPL.Adjusted_tsa)


# *** FUNCTION | simple_average_error_cols *** --------------------------------

# Define a function to create 'simple_average_error' columns
simple_average_error_cols <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'simple_average_error' column
  df[[paste0(original_col, "_simple_average_error")]] <-
    abs(df[[original_col]] - df[[paste0(original_col, "_simple_average")]])

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_simple_average_error <- lapply(
  tsa_dfs_simple_average, simple_average_error_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_simple_average_error, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | simple_average_error_pct_cols *** ---------------------------

# Define a function to create 'simple_average_error_pct' columns
simple_average_error_pct_cols <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'simple_average_error_pct' column
  df[[paste0(original_col, "_simple_average_error_pct")]] <-
    abs((df[[original_col]] - df[[paste0(original_col, "_simple_average")]]) /
      df[[original_col]]) * 100

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_simple_average_error_pct <- lapply(
  tsa_dfs_simple_average_error, simple_average_error_pct_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_simple_average_error_pct, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | moving_average_n50_cols *** ----------------------------------

# ? Uses zoo

# Define a function to create 'moving_average_n50' columns
moving_average_n50_cols <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'moving_average_n50' column
  df[[paste0(original_col, "_moving_average_n50")]] <-
    rollapply(df[[original_col]],
      width = 50, FUN = mean, align = "right",
      fill = NA
    )

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_moving_average_n50 <- lapply(
  tsa_dfs_simple_average_error_pct, moving_average_n50_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_moving_average_n50, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | moving_average_n50_error_cols *** ----------------------------

# Define a function to create 'moving_average_n50_error' columns
moving_average_n50_error_cols <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'moving_average_n50_error' column
  df[[paste0(original_col, "_moving_average_n50_error")]] <-
    abs(df[[original_col]] -
      df[[paste0(original_col, "_moving_average_n50")]])

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_moving_average_n50_error <- lapply(
  tsa_dfs_moving_average_n50, moving_average_n50_error_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_moving_average_n50_error, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | moving average_error_n50_pct_cols *** ------------------------

# Define a function to create 'moving_average_error_pct' columns
moving_average_n50_error_pct_cols <- function(df) {
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
tsa_dfs_moving_average_n50_error_pct <- lapply(
  tsa_dfs_moving_average_n50_error, moving_average_n50_error_pct_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_moving_average_n50_error_pct, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | moving_average_n200_cols *** ---------------------------------

# Define a function to create 'moving_average_n200' columns
moving_average_n200_cols <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'moving_average_n200' column
  df[[paste0(original_col, "_moving_average_n200")]] <-
    rollapply(df[[original_col]],
      width = 200, FUN = mean, align = "right",
      fill = NA
    )

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_moving_average_n200 <- lapply(
  tsa_dfs_moving_average_n50_error_pct, moving_average_n200_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_moving_average_n200, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | moving_average_n200_error_cols *** ---------------------------

# Define a function to create 'moving_average_n200_error' columns
moving_average_n200_error_cols <- function(df) {
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
tsa_dfs_moving_average_n200_error <- lapply(
  tsa_dfs_moving_average_n200, moving_average_n200_error_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_moving_average_n200_error, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | moving_average_n200_error_pct_cols *** -----------------------

# Define a function to create 'moving_average_n200_error_pct' columns
moving_average_n200_error_pct_cols <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'moving_average_n200_error_pct' column
  df[[paste0(original_col, "_moving_average_n200_error_pct")]] <-
    abs((df[[original_col]] -
      df[[paste0(original_col, "_moving_average_n200")]]) /
      df[[original_col]]) * 100

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_moving_average_n200_error_pct <- lapply(
  tsa_dfs_moving_average_n200_error, moving_average_n200_error_pct_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_moving_average_n200_error_pct, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | weighted_moving_average_n50_cols *** -------------------------

# Define a function to create 'weighted_moving_average_n50' columns
weighted_moving_average_n50_cols <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the weights
  weights <- seq(30, 1, length.out = 50)

  # Create the 'weighted_moving_average_n50' column
  df[[paste0(original_col, "_weighted_moving_average_n50")]] <-
    rollapply(df[[original_col]],
      width = 50, FUN = function(x) sum(x * weights) / sum(weights),
      align = "right", fill = NA
    )

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_weighted_moving_average_n50 <- lapply(
  tsa_dfs_moving_average_n200_error_pct, weighted_moving_average_n50_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_weighted_moving_average_n50, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | weighted_moving_average_n50_error_cols *** -------------------

# Define a function to create 'weighted_moving_average_n50_error' columns
weighted_moving_average_n50_error_cols <- function(df) {
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
tsa_dfs_weighted_moving_average_n50_error <- lapply(
  tsa_dfs_weighted_moving_average_n50, weighted_moving_average_n50_error_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_weighted_moving_average_n50_error, envir = .GlobalEnv)

# # View(AAPL.Adjusted_tsa)


# *** FUNCTION | weighted_moving_average_n50_error_pct_cols *** ---------------

# Define a function to create 'weighted_moving_average_n50_error_pct' columns
weighted_moving_average_n50_error_pct_cols <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'weighted_moving_average_n50_error_pct' column
  df[[paste0(original_col, "_weighted_moving_average_n50_error_pct")]] <-
    abs((df[[original_col]] -
      df[[paste0(original_col, "_weighted_moving_average_n50")]]) /
      df[[original_col]]) * 100

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_weighted_moving_average_n50_error_pct <- lapply(
  tsa_dfs_weighted_moving_average_n50_error,
  weighted_moving_average_n50_error_pct_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_weighted_moving_average_n50_error_pct, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | weighted_moving_average_n200_cols *** ------------------------

# Define a function to create 'weighted_moving_average_n200' columns
weighted_moving_average_n200_cols <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the weights
  weights <- seq(30, 1, length.out = 200)

  # Create the 'weighted_moving_average_n200' column
  df[[paste0(original_col, "_weighted_moving_average_n200")]] <-
    rollapply(df[[original_col]],
      width = 200, FUN = function(x) sum(x * weights) / sum(weights),
      align = "right", fill = NA
    )

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_weighted_moving_average_n200 <- lapply(
  tsa_dfs_weighted_moving_average_n50_error_pct,
  weighted_moving_average_n200_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_weighted_moving_average_n200, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | weighted_moving_average_n200_error_cols *** ------------------

# Define a function to create 'weighted_moving_average_n200_error' columns
weighted_moving_average_n200_error_cols <- function(df) {
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
tsa_dfs_weighted_moving_average_n200_error <- lapply(
  tsa_dfs_weighted_moving_average_n200, weighted_moving_average_n200_error_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_weighted_moving_average_n200_error, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | weighted_moving_average_n200_error_pct_cols *** --------------

# Define a function to create 'weighted_moving_average_n200_error_pct' columns
weighted_moving_average_n200_error_pct_cols <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'weighted_moving_average_n200_error_pct' column
  df[[paste0(original_col, "_weighted_moving_average_n200_error_pct")]] <-
    abs((df[[original_col]] -
      df[[paste0(original_col, "_weighted_moving_average_n200")]]) /
      df[[original_col]]) * 100

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_weighted_moving_average_n200_error_pct <- lapply(
  tsa_dfs_weighted_moving_average_n200_error,
  weighted_moving_average_n200_error_pct_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_weighted_moving_average_n200_error_pct, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | exponential_smoothing_a0.1_cols *** --------------------------
# ? Uses stats package

# Define a function to create 'exponential_smoothing_a0.1' columns
exponential_smoothing_a0_1_cols <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'exponential_smoothing_a0.1' column
  df[[paste0(original_col, "_exponential_smoothing_a0.1")]] <-
    stats::filter(df[[original_col]], filter = 0.1, method = "recursive")

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_exponential_smoothing_a0_1 <- lapply(
  tsa_dfs_weighted_moving_average_n200_error_pct,
  exponential_smoothing_a0_1_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_exponential_smoothing_a0_1, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | exponential_smoothing_a0.1_error_cols *** --------------------

# Define a function to create 'exponential_smoothing_a0.1_error' columns
exponential_smoothing_a0_1_error_cols <- function(df) {
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
tsa_dfs_exponential_smoothing_a0_1_error <- lapply(
  tsa_dfs_exponential_smoothing_a0_1, exponential_smoothing_a0_1_error_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_exponential_smoothing_a0_1_error, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** FUNCTION | exponential_smoothing_a0.1_error_pct_cols *** ----------------

# Define a function to create 'exponential_smoothing_a0.1_error_pct' columns
exponential_smoothing_a0_1_error_pct_cols <- function(df) {
  # Get the name of the original column
  original_col <- names(df)[1]

  # Create the 'exponential_smoothing_a0.1_error_pct' column
  df[[paste0(original_col, "_exponential_smoothing_a0.1_error_pct")]] <-
    abs((df[[original_col]] -
      df[[paste0(original_col, "_exponential_smoothing_a0.1")]]) /
      df[[original_col]]) * 100

  # Return the modified dataframe
  return(df)
}

# Apply the function to each dataframe
tsa_dfs_exponential_smoothing_a0_1_error_pct <- lapply(
  tsa_dfs_exponential_smoothing_a0_1_error,
  exponential_smoothing_a0_1_error_pct_cols
)

# Create variables in your environment for each dataframe in the list
list2env(tsa_dfs_exponential_smoothing_a0_1_error_pct, envir = .GlobalEnv)

View(AAPL.Adjusted_tsa)


# # *** FUNCTION | exponential_smoothing_a0.9_cols *** -------------------------
## ! Surprisingly bad!!!

# # Define a function to create 'exponential_smoothing_a0.9' columns
# exponential_smoothing_a0_9_cols <- function(df) {
#   # Get the name of the original column
#   original_col <- names(df)[1]

#   # Create the 'exponential_smoothing_a0.9' column
#   df[[paste0(original_col, "_exponential_smoothing_a0.9")]] <-
#     stats::filter(df[[original_col]], filter = 0.9, method = "recursive")

#   # Return the modified dataframe
#   return(df)
# }

# # Apply the function to each dataframe
# tsa_dfs_exponential_smoothing_a0_9 <- lapply(
#   tsa_dfs_exponential_smoothing_a0_1_error_pct,
#   exponential_smoothing_a0_9_cols
# )

# # Create variables in your environment for each dataframe in the list
# list2env(tsa_dfs_exponential_smoothing_a0_9, envir = .GlobalEnv)

# # View(AAPL.Adjusted_tsa)


# # *** FUNCTION | exponential_smoothing_a0.9_error_cols *** -------------------
## ! Surprisingly bad!!!

# # Define a function to create 'exponential_smoothing_a0.9_error' columns
# exponential_smoothing_a0_9_error_cols <- function(df) {
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
# tsa_dfs_exponential_smoothing_a0_9_error <- lapply(
#   tsa_dfs_exponential_smoothing_a0_9, exponential_smoothing_a0_9_error_cols
# )

# # Create variables in your environment for each dataframe in the list
# list2env(tsa_dfs_exponential_smoothing_a0_9_error, envir = .GlobalEnv)

# # View(AAPL.Adjusted_tsa)


# # *** FUNCTION | exponential_smoothing_a0.9_error_pct_cols *** ---------------
## ! Surprisingly bad!!!

# # Define a function to create 'exponential_smoothing_a0.9_error_pct' columns
# exponential_smoothing_a0_9_error_pct_cols <- function(df) {
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
# tsa_dfs_exponential_smoothing_a0_9_error_pct <- lapply(
#   tsa_dfs_exponential_smoothing_a0_9_error,
#   exponential_smoothing_a0_9_error_pct_cols
# )

# # Create variables in your environment for each dataframe in the list
# list2env(tsa_dfs_exponential_smoothing_a0_9_error_pct, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)

# # View(NVDA.Adjusted_tsa)


# *** FUNCTION | arima_cols *** -----------------------------------------------
# ? Uses forecast package
# ! TAKES TOO LONG (or maybe is it wrong?). Try again later

# # Define a function to create 'arima' columns
# arima_cols <- function(df) {
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
#   tsa_dfs_exponential_smoothing_a0_1_error_pct, arima_cols
# )

# # Create variables in your environment for each dataframe in the list
# list2env(tsa_dfs_arima, envir = .GlobalEnv)

# View(AAPL.Adjusted_tsa)


# *** VISUALIZE DIFFERENCES *** ------------------------------------------------

# Get the data for AAPL
df <- AAPL.Adjusted_tsa

# Convert row names to a column
df <- df %>% rownames_to_column("Date")

# colnames(df)

# Convert the data to long format for plotting
df_long <- df %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, 2, 3, 6, 9, 12, 15, 18) %>% # Select columns by their positions
  pivot_longer(
    cols = -Date, # Exclude the Date column
    names_to = "Variable",
    values_to = "Value"
  )

visualize_differences <- ggplot(df_long, aes(
  x = Date, y = Value, color = Variable
)) +
  geom_line(linewidth = 0.5, aes(linetype = Variable == names(df)[1])) +
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


# *** FUNCTION | portfolio_tsa *** -------------------------------------------

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
for (i in 1:length(tsa_dfs_exponential_smoothing_a0_1_error_pct)) {
  company_name <- names(tsa_dfs_exponential_smoothing_a0_1_error_pct)[i]
  df <- tsa_dfs_exponential_smoothing_a0_1_error_pct[[i]]

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

# View(NVDA.Adjusted_tsa) # Erratic

# Calculate the overall averages
overall_averages <- overall_averages /
  length(tsa_dfs_exponential_smoothing_a0_1_error_pct)

# Print the overall best method
overall_best_method <- which.min(overall_averages)
print(paste(
  "Overall best method is", colnames(portfolio_tsa)[overall_best_method + 1],
  "with an average error of", overall_averages[overall_best_method]
))

View(portfolio_tsa)


# *** FUNCTION | portfolio_tsa_plots *** --------------------------------------
# ? Uses ggplot2 and tidyr

portfolio_tsa_plots <- function(tsa_dfs_exponential_smoothing_a0_1_error_pct) {
  for (i in 1:length(tsa_dfs_exponential_smoothing_a0_1_error_pct)) {
    df <- tsa_dfs_exponential_smoothing_a0_1_error_pct[[i]]
    company_name <- names(tsa_dfs_exponential_smoothing_a0_1_error_pct)[i]

    # Obtain the ticker without the ".Adjusted_tsa"
    company_title <- substr(
      names(tsa_dfs_exponential_smoothing_a0_1_error_pct)[i], 1,
      nchar(names(tsa_dfs_exponential_smoothing_a0_1_error_pct)[i]) - 13
    )

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
        cols = c(1, best_method_column),
        names_to = "Variable",
        values_to = "Value"
      )

    # Create the plot
    plot <- ggplot(df_long, aes(x = Date, y = Value, color = Variable)) +
      geom_line() +
      scale_color_manual(values = c("#000000", "red")) +
      labs(x = "Date", y = "Value", color = "Variable") +
      ggtitle(paste(company_title, "Time Series Analysis")) +
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
    ggsave(paste0("./assets/tsa_plots/", company_name, ".jpg"), plot,
      width = 16, height = 9
    )
  }
}

# Call the function
portfolio_tsa_plots(tsa_dfs_exponential_smoothing_a0_1_error_pct)

# Ctrl + P > AAPL_Adjusted_tsa.jpg


# *** TIME SERIES FORECASTING *** ---------------------------------------------



# *** OBTAIN WEIGHTS *** ------------------------------------------------------

# # Install and load the necessary packages
# install.packages(c("PerformanceAnalytics", "PortfolioAnalytics"))
# library(PerformanceAnalytics)
# library(PortfolioAnalytics)

# install.packages("timeDate")
# library(timeDate)

# # Install and load the timeSeries package
# install.packages("timeSeries")
# library(timeSeries)

# head(portfolio_adjusted_metrics)

# # Calculate inverse-variance weights
# inverse_variance <- 1 / portfolio_adjusted_metrics$variance
# weights <- inverse_variance / sum(inverse_variance)
# View(weights)

calculate_weights <- function(df) {
  # Normalize the metrics
  normalize <- function(x) (x - min(x)) / (max(x) - min(x))
  average_norm <- normalize(df$average)

  # Inverse normalization because lower variance is better
  variance_norm <- 1 - normalize(df$variance)

  # Inverse normalization because lower std_deviation is better
  std_deviation_norm <- 1 - normalize(df$std_deviation)

  sharpe_norm <- normalize(df$sharpe)

  # Inverse normalization because lower betas is better
  betas_norm <- 1 - normalize(df$betas)

  r_squared_norm <- normalize(df$r_squared)
  treynor_norm <- normalize(df$treynor)

  # Calculate scores for each profile
  score_risk <- rowMeans(cbind(
    average_norm, sharpe_norm, betas_norm, r_squared_norm, treynor_norm
  )) # Higher risk, higher return
  score_moderate <- rowMeans(cbind(
    average_norm, variance_norm, std_deviation_norm, sharpe_norm,
    betas_norm, r_squared_norm, treynor_norm
  )) # Balanced
  score_conservative <- rowMeans(cbind(
    variance_norm, std_deviation_norm, betas_norm, r_squared_norm
  )) # Lower risk

  # Calculate weights
  weights_risk <- score_risk / sum(score_risk)
  weights_moderate <- score_moderate / sum(score_moderate)
  weights_conservative <- score_conservative / sum(score_conservative)

  # Add weights as the new second column
  df_risk <- cbind(
    df[, 1, drop = FALSE], weights_risk, df[, -1]
  )
  df_moderate <- cbind(
    df[, 1, drop = FALSE], weights_moderate, df[, -1]
  )
  df_conservative <- cbind(
    df[, 1, drop = FALSE], weights_conservative, df[, -1]
  )

  return(list(
    risk = df_risk, moderate = df_moderate, conservative = df_conservative
  ))
}

# Use the function
portfolio_profiles <- calculate_weights(portfolio_adjusted_metrics)
portfolio_risk <- portfolio_profiles$risk
portfolio_moderate <- portfolio_profiles$moderate
portfolio_conservative <- portfolio_profiles$conservative

sum(portfolio_risk$weights_risk) # ! Has to be 1
View(portfolio_risk)

sum(portfolio_moderate$weights_moderate) # ! Has to be 1
View(portfolio_moderate)

sum(portfolio_conservative$weights_conservative) # ! Has to be 1
View(portfolio_conservative)


# *** GET THE DATE COLUMN *** -------------------------------------------------

# * You can't use the "first" column (index), use this from library(tibble)
# Portfolio
portfolio_open <- rownames_to_column(portfolio_open, "Date")
portfolio_high <- rownames_to_column(portfolio_high, "Date")
portfolio_low <- rownames_to_column(portfolio_low, "Date")
portfolio_close <- rownames_to_column(portfolio_close, "Date")
portfolio_volume <- rownames_to_column(portfolio_volume, "Date")
portfolio_adjusted <- rownames_to_column(portfolio_adjusted, "Date")

# Benchmark
benchmark_open <- rownames_to_column(benchmark_open, "Date")
benchmark_high <- rownames_to_column(benchmark_high, "Date")
benchmark_low <- rownames_to_column(benchmark_low, "Date")
benchmark_close <- rownames_to_column(benchmark_close, "Date")
benchmark_volume <- rownames_to_column(benchmark_volume, "Date")
benchmark_adjusted <- rownames_to_column(benchmark_adjusted, "Date")

# T-bills
tbills_df <- rownames_to_column(tbills_df, "Date")


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

class(portfolio_adjusted$Date) # Date
class(benchmark_adjusted$Date) # Date
class(tbills_df$Date) # Date
# TODO: Search if there's a difference with POSIXct

View(portfolio_adjusted)


# *** VISUALIZATION *** -------------------------------------------------------

# aapl_adjusted_plot <- ggplot(portfolio_adjusted, aes(
#   x = Date, y = AAPL.Adjusted
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

# Usage example
export_to_excel(
  "./data/Portfolio_Analysis.xlsx", c(
    "portfolio_adjusted_metrics", "portfolio_risk", "portfolio_moderate",
    "portfolio_conservative", "portfolio_adjusted", "benchmark_adjusted",
    "tbills"
  ), list(
    portfolio_adjusted_metrics, portfolio_risk, portfolio_moderate,
    portfolio_conservative, portfolio_adjusted, benchmark_adjusted, tbills_df
  )
)
