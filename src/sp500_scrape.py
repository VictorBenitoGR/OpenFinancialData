# * This script scrapes the S&P 500 constituents from Wikipedia and writes the data to a CSV file.

import pandas as pd

url = (
  "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies#S&P_500_component_stocks"
)
df = pd.read_html(url, header=0)[0]

# Write to CSV
df.to_csv("./data/sp500.csv", index=False)