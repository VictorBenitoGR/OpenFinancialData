library(PerformanceAnalytics)
View(portfolio_adjusted)
sma20_portfolio_adjusted <- SMA(portfolio_adjusted$MMM, n = 20)
print(sma20_portfolio_adjusted)

head(AAPL)
