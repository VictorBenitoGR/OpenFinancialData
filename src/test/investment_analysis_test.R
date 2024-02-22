# * OpenFinancialData
# * https://github.com/VictorBenitoGR/OpenFInancialData

# * PACKAGES ------------------------------------------------------------------

install.packages(
  "quantmod", "fredr", "remotes,", "rdrop2", "rvest",
  "openxlsx", "siebanxicor", "tidyverse", "conflicted"
)
# ? install.packages("rdprop2") # nolint
# ? Can't use. Package ‘rdprop2’ is not available for this version of R
# ? development version (0.8.1.9999)
# ? devtools::install_github("karthik/rdrop2") # nolint: commented_code_linter.
# ? Can't use. Dependency ‘assertive’ is not available for package ‘rdrop2’
remotes::install_github("limnotrack/rdrop2")

library(quantmod) # Quantitative financial modeling and trading framework
library(fredr) # Access to Federal Reserve Economic Data (FRED) API
library(rdrop2) # Dropbox interface for R
library(rvest) # Web scraping and parsing HTML/XML
library(openxlsx) # Reading, writing, and editing Excel files
library(siebanxicor) # Interface for the Siebanxicor API
library(lubridate) # Working with dates and times
library(tidyverse) # Data manipulation and visualization packages
library(conflicted) # Handling conflicts between functions in R packages:
conflict_prefer("filter", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("guess_encoding", "readr")
conflict_prefer("lag", "dplyr")
conflict_prefer("last", "dplyr")


# * OBTAIN TICKETS ------------------------------------------------------------

ticker <- c(
  "^GSPC", # SP500
  "AAPL", "MSFT", "NVDA", "AMZN", "META",
  "GOOGL", "GOOG", "LLY", "TSLA", "AVGO",
  "TMO", "JPM", "UNH", "V", "XOM",
  "MA", "JNJ", "PG", "HD", "MRK",
  "COST", "ABBV", "AMD", "CRM", "CVX",
  "ADBE", "NFLX", "WMT", "KO", "BAC"
)

getSymbols(
  ticker,
  src = "yahoo",
  from = Sys.Date() - 3652, # 3652days = 10years
  to = Sys.Date()
)

# ? Open (O): The price of the asset at the beginning of the trading period.

# ? High (H): The highest price reached by the asset.

# ? Low (L): The lowest price reached by the asset.

# ? Close (C): The price of the asset at the end of the trading period.

# ? Volume (V): The total number of shares or contracts traded.

# ? Adjusted (Adj or Adjusted): The adjusted closing price accounts for
# ? corporate actions like dividends, stock splits, and new stock offerings.



# * PORTFOLIO (Adjusted) ------------------------------------------------------

# SP500
SP500 <- list(GSPC)



# Sample list of data frames
list_of_tickers <- list(
  AAPL, MSFT, NVDA, AMZN, META,
  GOOGL, GOOG, LLY, TSLA, AVGO,
  TMO, JPM, UNH, V, XOM,
  MA, JNJ, PG, HD, MRK,
  COST, ABBV, AMD, CRM, CVX,
  ADBE, NFLX, WMT, KO, BAC
)

# Function to select columns containing the word "Adjusted"
adjusted_price <- function(df) {
  adjusted_columns <- grep("Adjusted", names(df), value = TRUE)
  return(df[, adjusted_columns, drop = FALSE])
}

# Use lapply to apply the function to each data frame in the list
Portfolio.Adjusted <- lapply(list_of_tickers, select_adjusted_columns)
SP500 <- lapply(list_of_tickers, select_adjusted_columns)


library(xts) # Provide for uniform handling of different time-based data classes

# Function to convert xts to data frame
xts_to_df <- function(xts_object) {
  as.data.frame(xts_object)
}

# Use lapply to convert each xts object to a data frame
df_list <- lapply(Portfolio.Adjusted, xts_to_df)

# Combine the data frames into a single data frame
Portfolio.Adjusted <- do.call(cbind, df_list)

# !

Portfolio.Summary <- data.frame(Average = colMeans(Portfolio.Adjusted))

# !

# Function to calculate beta and R-squared
calculate_beta_r2 <- function(x, y) {
  fit <- lm(y ~ x)
  return(c(Beta = coef(fit)[2], R2 = summary(fit)$r.squared))
}

# // Creating Portfolio.Summary data frame
Portfolio.Summary <- data.frame(
  Average = colMeans(Portfolio.Adjusted),
  Variance = apply(Portfolio.Adjusted, 2, var),
  Std_Deviation = apply(Portfolio.Adjusted, 2, sd),
  Beta = apply(Portfolio.Adjusted, 2, function(col) calculate_beta_r2(col, benchmark_returns)),
  R2 = apply(Portfolio.Adjusted, 2, function(col) calculate_beta_r2(col, benchmark_returns))
)

# !
Portfolio.Summary <- data.frame(
  Average = colMeans(Portfolio.Adjusted),
  Variance = apply(Portfolio.Adjusted, 2, var),
  Std_Deviation = apply(Portfolio.Adjusted, 2, sd)
)





View(Portfolio.Summary)


# Notes

# 2 types of risk, systemic and non-systemic

# Drowdown - encontrar lo menos que ha tenido y lo mayor
# media móvil, banda de bollinger, patrones de velas, RSI, MACD
# Cruce de medias móviles, MACD

# Escoger una acción del portafolio, y aplicar backtesting, escoger los
# criterios del backtesting (horizonte y qué herramientas, 3 diferentes por
# equipo como medias ḿóviles RSi etc)

# 5 años semanales, velas
