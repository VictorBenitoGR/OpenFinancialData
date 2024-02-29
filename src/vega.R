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
# Obtain the ticker symbols
na.omit(quantmod::getSymbols(
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
  from = Sys.Date() - 1095, # 1095days = 3years
  to = Sys.Date()
))

# Benchmarks, necessary to get Beta and R2
benchmark <- list(EUSA)

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

# *** GET THE DATE COLUMN *** -------------------------------------------------

# * You can't use the "first" column, use this from library(tibble)
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


# *** CHARACTER TO DATE *** ---------------------------------------------------

# ? You need to give Date its proper format
# * Portfolio
portfolio_open$Date <- as.Date(portfolio_open$Date, format = "%Y-%m-%d")
portfolio_high$Date <- as.Date(portfolio_high$Date, format = "%Y-%m-%d")
portfolio_low$Date <- as.Date(portfolio_low$Date, format = "%Y-%m-%d")
portfolio_close$Date <- as.Date(portfolio_close$Date, format = "%Y-%m-%d")
portfolio_volume$Date <- as.Date(portfolio_volume$Date, format = "%Y-%m-%d")
portfolio_adjusted$Date <- as.Date(portfolio_adjusted$Date, format = "%Y-%m-%d")

# * Benchmark
benchmark_open$Date <- as.Date(benchmark_open$Date, format = "%Y-%m-%d")
benchmark_high$Date <- as.Date(benchmark_high$Date, format = "%Y-%m-%d")
benchmark_low$Date <- as.Date(benchmark_low$Date, format = "%Y-%m-%d")
benchmark_close$Date <- as.Date(benchmark_close$Date, format = "%Y-%m-%d")
benchmark_volume$Date <- as.Date(benchmark_volume$Date, format = "%Y-%m-%d")
benchmark_adjusted$Date <- as.Date(benchmark_adjusted$Date, format = "%Y-%m-%d")


# *** FUNCTION | SUMMARIZE DATA *** -------------------------------------------

# Function to calculate summary statistics for a portfolio
summarize <- function(df, benchmark_adjusted) {
  # Divide each row by the previous one and apply natural logarithm
  library(dplyr)
  df <- log(df / lag(df))
  benchmark_adjusted$EUSA.Adjusted <- log(
    benchmark_adjusted$EUSA.Adjusted /
      lag(benchmark_adjusted$EUSA.Adjusted)
  )

  # Replace NA and Inf values with 0
  df[is.na(df) | df == Inf] <- 0
  benchmark_adjusted$EUSA.Adjusted[
    is.na(
      benchmark_adjusted$EUSA.Adjusted
    ) | benchmark_adjusted$ColumnName == Inf
  ] <- 0

  betas <- sapply(names(df), function(col) {
    formula <- as.formula(paste(col, "~ EUSA.Adjusted"))
    regression_result <- lm(formula, data = cbind(
      df,
      EUSA.Adjusted = benchmark_adjusted$EUSA.Adjusted
    ))
    coef(regression_result)[2]
  })
  r_squared <- sapply(names(df), function(col) {
    formula <- as.formula(paste(col, "~ EUSA.Adjusted"))
    regression_result <- lm(formula, data = cbind(
      df,
      EUSA.Adjusted = benchmark_adjusted$EUSA.Adjusted
    ))
    summary(regression_result)$r.squared
  })
  summary <- data.frame(
    Average = colMeans(df, na.rm = TRUE) * 100,
    Variance = apply(df, 2, var, na.rm = TRUE) * 100,
    Std_Deviation = apply(df, 2, sd, na.rm = TRUE) * 100,
    Beta = betas,
    R_Squared = r_squared
  )
  library(tibble)
  # Convert row names to a column named "Tickers"
  summary <- rownames_to_column(summary, "Tickers")
  return(summary)
}

portfolio_adjusted_summary <- summarize(portfolio_adjusted, benchmark_adjusted)
head(portfolio_adjusted)
head(portfolio_adjusted_summary)
View(portfolio_adjusted_summary)


# abc <- portfolio_adjusted <- portfolio_adjusted[, -1]
# abc <- abc / lag(abc)
# View(portfolio_adjusted)
# View(abc)


# *** SUMMARIZE DATA *** ------------------------------------------------------

# Portfolio summary
portfolio_open_summary <- summarize(portfolio_open, benchmark_adjusted)
portfolio_high_summary <- summarize(portfolio_high, benchmark_adjusted)
portfolio_low_summary <- summarize(portfolio_low, benchmark_adjusted)
portfolio_close_summary <- summarize(portfolio_close, benchmark_adjusted)
portfolio_volume_summary <- summarize(portfolio_volume, benchmark_adjusted)
portfolio_adjusted_summary <- summarize(portfolio_adjusted, benchmark_adjusted)

# # Benchmark summary (maybe against SP500?)
# benchmark_open_summary <- summarize(benchmark_open, benchmark_adjusted)
# benchmark_high_summary <- summarize(benchmark_high, benchmark_adjusted)
# benchmark_low_summary <- summarize(benchmark_low, benchmark_adjusted)
# benchmark_close_summary <- summarize(benchmark_close, benchmark_adjusted)
# benchmark_volume_summary <- summarize(benchmark_volume, benchmark_adjusted)
# benchmark_adjusted_summary <- summarize(benchmark_adjusted, benchmark_adjusted) # nolint: line_length_linter.


# *** FUNCTION | CALCULATE BETA AND R2 *** -------------------------------------

# # ? Test
# benchmark_adjusted <- lapply(benchmark, adjusted_price)
# benchmark_adjusted <- lapply(benchmark_adjusted, xts_to_df)
# benchmark_adjusted <- do.call(cbind, benchmark_adjusted)
# benchmark_adjusted_summary <- summarize(benchmark_adjusted)


# # Get the Ln of benchmark_adjusted
# benchmark_adjusted$Ln <- log(benchmark_adjusted$GSPC.Adjusted)
# View(benchmark_adjusted)

# View(portfolio_adjusted_summary)


# *** VISUALIZATION *** -------------------------------------------------------

aapl_adjusted_plot <- ggplot(portfolio_adjusted, aes(
  x = Date, y = AAPL.Adjusted
)) +
  geom_area_pattern(
    data = portfolio_adjusted,
    pattern = "gradient",
    fill = "#00000000",
    pattern_fill = "#00000000",
    pattern_fill2 = "#10006b"
  ) +
  ggtitle("APPL price adjusted") +
  labs(
    subtitle = "Evolution of Apple over the last 10 years",
    y = "Price",
    caption = "R Plot: @VictorBenitoGR | GitHub Repository: VictorBenitoGR/OpenFinancialData" # nolint: line_length_linter.
  ) +
  geom_smooth(method = loess, color = "red", fill = "#69b3a2", se = TRUE) +
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
    axis.ticks = element_blank()
  )

# Save the plot
ggsave("./assets/adjusted_plot/aapl_adjusted_plot.jpg", aapl_adjusted_plot,
  width = 16, height = 9
)


# !!! TESTS !!! ---------------------------------------------------------------
# plot(Ad(AAPL))


# chart_Series(AAPL,
#   name = NULL,
#   type = "candlesticks",
#   subset = "",
#   TA = "",
#   pars = chart_pars(),
#   theme = chart_theme(),
#   clev = 0
# )

# # Get it
# aapl <- list(AAPL)
# # Use lapply to convert each xts object to a data frame
# aapl <- lapply(aapl, xts_to_df)
# # Combine the data frames into a single data frame
# aapl <- do.call(cbind, aapl)
# # You can't use the "first" column, use this from library(tibble)
# aapl <- rownames_to_column(aapl, "Date")
# # You need to give Date its proper format
# aapl$Date <- as.Date(aapl$Date, format = "%Y-%m-%d")
# View(aapl)

# # "TTR Composite" (simulated data)
# data(aapl)

# # Bollinger Bands
# bbands <- BBands(aapl[, c("AAPL.High", "AAPL.Low", "AAPL.Close")])

# # Directional Movement Index
# adx <- ADX(aapl[, c("AAPL.High", "AAPL.Low", "AAPL.Close")])

# # Moving Averages
# ema <- EMA(aapl[, "AAPL.Close"], n = 20)
# sma <- SMA(aapl[, "AAPL.Close"], n = 20)

# # MACD
# macd <- MACD(aapl[, "AAPL.Close"])

# # RSI
# rsi <- RSI(aapl[, "AAPL.Close"])

# # Stochastics
# stoch <- stoch(aapl[, c("AAPL.High", "AAPL.Low", "AAPL.Close")])
# # TTR works with the chartSeries() function in quantmod. Here's an example that
# # uses chartSeries() and adds TTR-calculated indicators and overlays to the
# # chart.

# # "TTR Composite" (simulated data)
# data(AAPL)

# # Use quantmod's OHLCV extractor function to help create an xts object
# xaapl <- xts(OHLCV(aapl), aapl[["Date"]])

# chartSeries(xaapl, subset = "2019-02-22/", theme = "white")
# addBBands()
# addRSI()



# ggsave("./aapl.jpg", xaapl, width = 16, height = 9)

# *** EXPORT IF NECESSARY *** ------------------------------------------------

# Function to export dataframes to an Excel file
export_to_excel <- function(file, sheet_names, dataframes) {
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
  }

  # Save the workbook as xlsx file
  saveWorkbook(wb, file)
}

# Usage example
export_to_excel(
  "./data/stock_prices.xlsx", c(
    "portfolio_open", "portfolio_high", "portfolio_low",
    "portfolio_close", "portfolio_volume", "portfolio_adjusted",
    "benchmark_open", "benchmark_high", "benchmark_low",
    "benchmark_close", "benchmark_volume", "benchmark_adjusted"
  ), list(
    portfolio_open, portfolio_high, portfolio_low,
    portfolio_close, portfolio_volume, portfolio_adjusted,
    benchmark_open, benchmark_high, benchmark_low,
    benchmark_close, benchmark_volume, benchmark_adjusted
  )
)
