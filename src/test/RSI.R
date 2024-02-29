# * OpenFinancialData | RSI Trading Strategy
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


# *** OBTAIN TICKER SYMBOLS *** -----------------------------------------------

# * Start here
# Specify the ticker symbol and the source
symbol <- "AAPL"

# Import the historical stock prices
aapl <- getSymbols(
  symbol,
  src = "yahoo",
  from = Sys.Date() - 1095, # 1095days = 3years
  to = Sys.Date()
)

# ? Open (O): The price of the asset at the beginning of the trading period.
# ? High (H): The highest price reached by the asset.
# ? Low (L): The lowest price reached by the asset.
# ? Close (C): The price of the asset at the end of the trading period.
# ? Volume (V): The total number of shares or contracts traded.
# ? Adjusted (Adj or Adjusted): The adjusted closing price accounts for
# ? corporate actions like dividends, stock splits, and new stock offerings.


# *** SPLIT BY TYPES *** ------------------------------------------------------

# Extract the adjusted prices
prices <- Ad(get(symbol))


# ***  RSI | INTERACTIVE *** --------------------------------------------------

# Calculate the RSI
rsi <- RSI(prices)

# Use highcharter for visualization
hc1 <- hchart(AAPL) %>%
  hc_title(text = "Daily Stock Price of AAPL (2021 - 2024)") %>%
  hc_xAxis(type = "datetime") %>%
  hc_yAxis(title = list(text = "Price"))
hc2 <- hchart(rsi) %>%
  hc_title(text = "RSI of AAPL (2021 - 2024)") %>%
  hc_xAxis(type = "datetime") %>%
  hc_yAxis(title = list(text = "RSI"))

grid <- hw_grid(list(hc1, hc2), ncol = 1) # Plot two graphs in one grid
grid


# *** RSI PLOTS *** -----------------------------------------------------------

# dataframe for RSI and Adjusted price
rsi_df <- data.frame(
  date = index(rsi),
  rsi = coredata(rsi),
  adjusted = as.numeric(AAPL$AAPL.Adjusted)
)
datatable(rsi_df)

# Plot RSI with shaded areas
gg1 <- ggplot(rsi_df, aes(x = date, y = rsi)) +
  geom_line(linewidth = 1, color = "#5F9EA0") +
  geom_rect(aes(
    xmin = date, xmax = lead(date),
    ymin = ifelse(rsi < 30, -Inf, NA), ymax = Inf
  ), fill = "blue", alpha = 0.2) +
  geom_rect(aes(
    xmin = date, xmax = lead(date),
    ymin = -Inf, ymax = ifelse(rsi > 60, Inf, NA)
  ), fill = "red", alpha = 0.2, inherit.aes = FALSE) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(title = paste("RSI for", symbol), x = "Date", y = "RSI") +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%Y-%m"
  ) +
  theme_light()

# Plot the stock price with shaded areas
gg2 <- ggplot(rsi_df, aes(x = date, y = adjusted)) +
  geom_line(linewidth = 1, color = "cornflowerblue") +
  geom_rect(aes(
    xmin = date, xmax = lead(date),
    ymin = ifelse(rsi < 30, -Inf, NA), ymax = Inf
  ), fill = "blue", alpha = 0.2) +
  geom_rect(aes(
    xmin = date, xmax = lead(date),
    ymin = -Inf, ymax = ifelse(rsi > 70, Inf, NA)
  ), fill = "red", alpha = 0.2) +
  labs(title = paste(
    "Adjusted Price for", symbol
  ), x = "Date", y = "Adjusted Price") +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%Y-%m"
  ) +
  theme_light()

# Helps plot two graphs in one grid for ggplot2 objects
aapl_adjusted_rsi <- gg2 + gg1 + plot_layout(ncol = 1)
ggsave("./assets/AAPL/aapl_adjusted_rsi.jpg", aapl_adjusted_rsi,
  width = 16, height = 9
)
a
# The blue area shows where the RSI is less than 30 and the red area shows where
# the RSI is over 60.

# *** RSI TRADING *** ---------------------------------------------------------
# For this part, we are going to implement RSI trading strategy. We
# are going to buy the stock when the RSI below 30 recovers and goes above 30
# and sell it when RSI above 60 goes down below 60, and then repeat this process
# again until we find all of the trading points.

# Finding indices for buying and selling
# Define threshold values
buy_threshold <- 30
sell_threshold <- 60

# Initialize variables
position <- "short"
last_signal <- "none"

buy_signals <- c()
sell_signals <- c()



# Loop through RSI values
for (i in 1:length(rsi)) {
  # Check for buy signal
  if (!is.na(rsi[i]) && rsi[i] < buy_threshold && position == "short") {
    # Wait for RSI to cross above buy threshold
    for (j in (i + 1):length(rsi)) {
      if (!is.na(rsi[j]) && rsi[j] > buy_threshold) {
        position <- "long"
        last_signal <- "buy"
        buy_signals <- c(buy_signals, j)
        break # Exit loop once buy signal is found
      }
    }
  }

  # Check for sell signal
  if (!is.na(rsi[i]) && rsi[i] > sell_threshold && position == "long") {
    # Wait for RSI to cross below sell threshold
    for (j in (i + 1):length(rsi)) {
      if (!is.na(rsi[j]) && rsi[j] < sell_threshold) {
        position <- "short"
        last_signal <- "sell"
        sell_signals <- c(sell_signals, j)

        # Find all buy signals between the current and next sell signals
        for (k in (buy_signals[length(buy_signals)] + 1):(j - 1)) {
          if (!is.na(rsi[k]) && rsi[k] < buy_threshold) {
            # Wait for RSI to cross above buy threshold
            for (m in (k + 1):j) {
              if (!is.na(rsi[m]) && rsi[m] > buy_threshold) {
                # Buy signal found between sell signals
                buy_signals <- c(buy_signals, m)
                break
              }
            }
          }
        }
        break # Exit loop once sell signal is found
      }
    }
  }
}
# Buy signal Index
buy_signals
## [1]  19 189 250
# Sell Signal Index
sell_signals
## [1]  62 285
# Create data frame with Date and Adjusted Price columns
nf <- data.frame(Date = index(rsi), Adjusted = rsi_df$adjusted)

# Add Trade column and initialize to "none"
nf$Trade <- rep(NA, nrow(nf))

# Set buy signals in Trade column to "buy"
nf$Trade[buy_signals] <- "buy"

# Set sell signals in Trade column to "sell"
nf$Trade[sell_signals] <- "sell"

# Sort data frame by Date
nf <- nf[order(nf$Date), ]
nf_omit <- na.omit(nf)
datatable(nf_omit)


# The table above shows when we have to buy the stocks and sell the stocks
# according to the strategy


# Filter out NA trades
trades <- na.omit(nf)
# Plot RSI and trades
gg_rsi1 <- ggplot(nf, aes(x = Date, y = Adjusted)) +
  geom_line(size = 0.8, color = "cornflowerblue") +
  geom_point(
    data = trades[trades$Trade == "buy", ], aes(color = "buy"), size = 4
  ) +
  geom_point(
    data = trades[trades$Trade == "sell", ], aes(color = "sell"), size = 4
  ) +
  scale_color_manual(
    values = c("buy" = "grey30", "sell" = "orangered")
  ) +
  labs(x = "Date", y = "Price", color = "Signal") +
  ggtitle("RSI Trading Signal (AAPL)") +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%Y-%m"
  ) +
  theme_light() +
  theme(legend.position = "top")

gg_rsi1

ggsave(
  "./assets/AAPL/aapl_rsi_trading_signal.jpg", gg_rsi1,
  width = 16, height = 9
)


# Calculate Returns

# Filter out NA trades
trades <- na.omit(nf)

# For first trade
buy_price <- trades$Adjusted[1]
sell_price <- trades$Adjusted[2]
returns1 <- (sell_price - buy_price) / buy_price

# For second trade
buy_prices <- c(trades$Adjusted[3], trades$Adjusted[4])
sell_price <- trades$Adjusted[5]
returns2 <- (sell_price - mean(buy_prices)) / mean(buy_prices)


# Total Cumulative Return
trading_return <- (1 + returns1) * (1 + returns2) - 1

# Buy&Hold
buy_hold_return <- (nf[301, "Adjusted"] - nf[1, "Adjusted"]) / nf[1, "Adjusted"]
# Make a dataframe to plot the return graph
returns_df <- data.frame(
  Trade = c("1st Trade", "2nd Trade", "RSI Trading Profit", "Buy&Hold"),
  Return = c(returns1, returns2, trading_return, buy_hold_return)
)

# Define the order of the trades
trade_order <- c("1st Trade", "2nd Trade", "RSI Trading Profit", "Buy&Hold")
returns_df$Trade <- factor(returns_df$Trade, levels = trade_order)
gg1 <- ggplot(returns_df, aes(
  x = Trade, y = Return, color = Trade, fill = Trade
)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  labs(
    title = "Returns from RSI Trading Strategy for AAPL",
    y = "Return",
    x = "Trade"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  geom_text(aes(label = scales::percent(Return)), vjust = -0.2, size = 4) +
  geom_text(data = filter(returns_df, Return < 0), aes(
    label = scales::percent(Return), y = Return - 0.01
  ), size = 4) +
  theme(legend.position = "top")

gg1

ggsave(
  "./assets/AAPL/aapl_returns_from_rsi.jpg", gg1,
  width = 16, height = 9
)


# *** RSI VS BOLLINGER BAND *** -----------------------------------------------
# We see that the rather than buy&holding the stock,
# trading made a better outcome. Now, let’s compare the performance with the
# Bollinger Band strategy.

AAPL$BBands <- BBands(Ad(AAPL), n = 20, sd = 2)

# * Add trade signal
# If the adjusted prices is below the band, it's a buy signal
AAPL$signal_b <- ifelse(Ad(AAPL) < AAPL$dn, 1,
  ifelse(Ad(AAPL) > AAPL$up, -1, 0)
) # If the adjusted prices is above the band, it's a sell signal

# Create trade signal using bollingerband strategy
trade_b <- ifelse(AAPL$signal == 1, "buy",
  ifelse(AAPL$signal == -1, "sell", "")
)

# Create new dataframe for trading strategy
df_b <- data.frame(
  Date = index(AAPL), Adjusted = Ad(AAPL),
  BBands_dn = AAPL$dn, BBands_up = AAPL$up, Trade = trade_b
)
# Change the column names
colnames(df_b) <- c("Date", "Adjusted", "BBands_dn", "BBands_up", "Trade")

datatable(df_b)


# Visualize
gg_bband1 <- ggplot(df_b, aes(x = Date, y = Adjusted)) +
  geom_line(size = 0.8, color = "steelblue") +
  geom_ribbon(aes(
    ymin = BBands_dn, ymax = BBands_up
  ), alpha = 0.2, fill = "black") +
  geom_point(data = df_b[df_b$Trade == "buy", ], aes(
    x = Date, y = Adjusted, color = "buy"
  ), size = 3) +
  geom_point(data = df_b[df_b$Trade == "sell", ], aes(
    x = Date, y = Adjusted, color = "sell"
  ), size = 3) +
  scale_color_manual(
    name = "Trade", values = c("buy" = "grey30", "sell" = "orangered")
  ) +
  labs(title = "Bollinger Band Signal (AAPL)", y = "Price", x = "Date") +
  theme_light() +
  theme(legend.position = "top") +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%Y-%m"
  )


aapl_bollinger_band_rsi <- gg_bband1 + gg_rsi1 + plot_layout(ncol = 1)
aapl_bollinger_band_rsi

ggsave(
  "./assets/AAPL/aapl_bollinger_band_rsi.jpg", aapl_bollinger_band_rsi,
  width = 16, height = 9
)

# Identify the first trade relevant indices
first_buy_index <- which(df_b$Trade == "buy")[1]
first_sell_index <- which(
  df_b$Trade == "sell" & index(df_b) > index(df_b)[first_buy_index]
)[1]

# Identify the second trade relevant indices
second_buy_index <- which(
  df_b$Trade == "buy" & index(df_b) > index(df_b)[first_sell_index]
)[1]
second_sell_index <- which(
  df_b$Trade == "sell" & index(df_b) > index(df_b)[second_buy_index]
)[1]

# Identify the third trade relevant indices
third_buy_index <- which(
  df_b$Trade == "buy" & index(df_b) > index(df_b)[second_sell_index]
)[1]
third_sell_index <- which(
  df_b$Trade == "sell" & index(df_b) > index(df_b)[third_buy_index]
)[1]

# Identify the fourth trade relevant indices
fourth_buy_index <- which(
  df_b$Trade == "buy" & index(df_b) > index(df_b)[third_sell_index]
)[1]
fourth_sell_index <- which(
  df_b$Trade == "sell" & index(df_b) > index(df_b)[fourth_buy_index]
)[1]
# Calculate average price and return for first buy-sell pair
if (!is.na(first_buy_index) && !is.na(first_sell_index)) {
  buy_prices_1 <- df_b[which(
    df_b$Trade == "buy" & index(df_b) <= index(df_b)[first_sell_index]
  ), "Adjusted"]
  buy_avg_price_1 <- mean(buy_prices_1, na.rm = TRUE)

  sell_price_1 <- df_b[first_sell_index, "Adjusted"]
  return_1 <- (sell_price_1 - buy_avg_price_1) / buy_avg_price_1
} else {
  return_1 <- NA
}

# Calculate average price and return for second buy-sell pair
if (!is.na(second_buy_index) && !is.na(second_sell_index)) {
  buy_prices_2 <- df_b[which(
    df_b$Trade == "buy" & index(df_b) > index(df_b)[first_sell_index] &
      index(df_b) < index(df_b)[second_sell_index]
  ), "Adjusted"]
  buy_avg_price_2 <- mean(buy_prices_2, na.rm = TRUE)

  sell_price_2 <- df_b[second_sell_index, "Adjusted"]
  return_2 <- (sell_price_2 - buy_avg_price_2) / buy_avg_price_2
} else {
  return_2 <- NA
}

# Calculate average price and return for third buy-sell pair
if (!is.na(third_buy_index) && !is.na(third_sell_index)) {
  buy_prices_3 <- df_b[which(
    df_b$Trade == "buy" & index(df_b) > index(df_b)[second_sell_index] &
      index(df_b) < index(df_b)[third_sell_index]
  ), "Adjusted"]
  buy_avg_price_3 <- mean(buy_prices_3, na.rm = TRUE)

  sell_price_3 <- df_b[third_sell_index, "Adjusted"]
  return_3 <- (sell_price_3 - buy_avg_price_3) / buy_avg_price_3
} else {
  return_3 <- NA
}


# Calculate average price and return for fourth buy-sell pair
if (!is.na(fourth_buy_index) && !is.na(fourth_sell_index)) {
  buy_prices_4 <- df_b[which(
    df_b$Trade == "buy" & index(df_b) > index(df_b)[third_sell_index] &
      index(df_b) < index(df_b)[fourth_sell_index]
  ), "Adjusted"]
  buy_avg_price_4 <- mean(buy_prices_4, na.rm = TRUE)

  sell_price_4 <- df_b[fourth_sell_index, "Adjusted"]
  return_4 <- (sell_price_4 - buy_avg_price_4) / buy_avg_price_4
} else {
  return_4 <- NA
}



bollinger_return <- (1 + return_1) * (1 + return_2) *
  (1 + return_3) * (1 + return_4) - 1

# Create Dataframe for visualize trade profit
returns_df <- data.frame(
  Trade = c(
    "1st Trade", "2nd Trade", "3rd Trade", "4th Trade",
    "Bollinger Trading Profit", "Buy&Hold"
  ),
  Return = c(
    return_1, return_2, return_3, return_4,
    bollinger_return, buy_hold_return
  )
)
trade_order <- c(
  "1st Trade", "2nd Trade", "3rd Trade", "4th Trade",
  "Bollinger Trading Profit", "Buy&Hold"
)
returns_df$Trade <- factor(returns_df$Trade, levels = trade_order)
# plot the returns using a bar chart
gg2 <- ggplot(returns_df, aes(
  x = Trade, y = Return, color = Trade, fill = Trade
)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Returns from Bollinger Band Trading Strategy for AAPL",
    y = "Return", x = "Trade"
  ) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(Return)), vjust = -0.2, size = 4) +
  geom_text(data = filter(returns_df, Return < 0), aes(
    label = scales::percent(Return), y = Return - 0.01
  ), size = 4) +
  theme_light() +
  theme(legend.position = "top")


aapl_returns_bollinger_band_rsi <- gg2 + gg1 + patchwork::plot_layout(ncol = 1)

ggsave(
  "./assets/AAPL/aapl_returns_bollinger_band_rsi.jpg",
  aapl_returns_bollinger_band_rsi,
  width = 16, height = 9
)


# Make a dataframe to plot the return graph
returns_df <- data.frame(
  Trade = c("RSI Trading Profit", "Bollinger Trading Profit", "Buy&Hold"),
  Return = c(trading_return, bollinger_return, buy_hold_return)
)

# Define the order of the trades
trade_order <- c("RSI Trading Profit", "Bollinger Trading Profit", "Buy&Hold")
returns_df$Trade <- factor(returns_df$Trade, levels = trade_order)

returns_df

rsi_trading
aapl_trading_strategy_return <- ggplot(returns_df, aes(
  x = Trade, y = Return, fill = Trade
)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  labs(title = "Trading Strategy Return (AAPL)", y = "Return", x = "Trade") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(Return)), vjust = -0.2, size = 4) +
  scale_fill_manual(values = c(
    "RSI Trading Profit" = "cornflowerblue",
    "Bollinger Trading Profit" = "#A29BFE",
    "Buy&Hold" = "lightgreen"
  )) +
  theme_light() +
  theme(legend.position = "top")

ggsave(
  "./assets/AAPL/aapl_trading_strategy_return.jpg",
  aapl_trading_strategy_return,
  width = 16, height = 9
)
