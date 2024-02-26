# OpenFinancialData
Transparency, analysis, forecasting, visualization and improvement of the financial performance of the global economy. Provides two modules, **VEGA** (**V**isualization and **E**valuation of **G**lobal **A**ccounting) and **SOLARIS** (**S**ecurities **O**ptimization and **L**ong-term **A**nalysis of **R**isk on **I**nvestment **S**trategies).


## Background
I started this project during my university career at Tecnologico de Monterrey (ITESM), which has direct agreements with several companies for students to develop projects that address specific problems they have, like L'Oréal Mexico and MSCI USA.

![L'Oréal and MSCI](./assets/README/loreal_and_msci_itesm.png "L'Oréal and MSCI")

## VEGA (Visualization and Evaluation of Global Accounting)
This module seeks to automate financial statement analysis, allowing to visualize the evolution of public companies listed on the SEC (US, through the EDGAR system) and BMV/BIVA (Mexico, DataBursatil API). 

Its ultimate goal is to simplify the insertion of any company into the formal economy and even achieve its IPO.


### General forecast
Obtain the net profit for Q4 of each year and apply different forecasting methods such as **Simple Average**, **Moving Average** (n = 3 and more), **Weighted Moving Average**, **Exponential Smoothing** (using different alphas), etc., calculating their respective **Median Absolute Deviation (MAD)** and highlighting the one with the highest accuracy.

![Herdez Forecast](./assets/README/HerdezAnalysis1.png "Herdez Forecast")

Apply the most accurate forecasting method to the **Income Statement** with predictability up to 2 years.

![Herdez Income Statement Forecast](./assets/README/HerdezAnalysis2.png "Herdez Income Statement Forecast")

Apply the most accurate forecasting method to the **Balance Sheet** with predictability up to 2 years.

![Herdez Balance Sheet Forecast](./assets/README/HerdezAnalysis3.png "Herdez Balance Sheet Forecast")

### Accounting Ratios
Calculate current and forecasted **Accounting Ratios**, such as the **Activity** (Accounts Receivable Turnover, Inventory Turnover and Accounts Payable Turnover) and **Profitability** (Gross Profit Margin, Operating Profit Margin, Operating Profit Margin, Return on Assets and Return on Equity) ratios allowing their data visualization.

![Herdez Financial Ratios](./assets/README/HerdezAnalysis4.png "Herdez Financial Ratios")

## SOLARIS (Securities Optimization and Long-Term Analysis of Risk on Investment Strategies)
This module seeks to automate the analysis of financial portfolios and asset investments (with their respective depreciation/amortization impact).

Its ultimate goal is to simplify the insertion of any company to corporate investment, customizing the condensation of their tax implications.


### Benchmark Analysis
MSCI USA and SP500 are the base benchmarks used.

### Portfolio Analysis

#### Close Price and RSI
Get the closing price and RSI (NVIDIA case) up to 3 years of data (can be expanded easily).

![Closing Price and RSI](./assets/rsi/nvda_close_rsi.jpg "Closing Price and RSI")

#### Bollinger Band Signal and RSI Trading Signal
Identify the points where we would have a buy and sell signal following the Bollinger Band and RSI strategies.

![Bollinger Band RSI Trading Signal](./assets/rsi/nvda_bollinger_band_rsi_signal.jpg "Bollinger Band RSI Trading Signal")

#### Returns from Bollinger Band Trading Strategy and RSI Trading Strategy
Finally get the result of an hypothetic portfolio started 3 years ago with these strategies, identifying which one would be better (in this case for NVIDIA is Bollinger Trading) or if it'd be better to Buy & Hold.

![Returns for Bollinger Band and RSI](./assets/rsi/nvda_returns_bollinger_band_rsi.jpg "Bollinger Band RSI")



### Valuation Ratios
> [!IMPORTANT]  
> Soon!

### Asset investments and depreciation/amortization
Clarify the elements necessary to analyze potential investments, such as Investment Capacity, Loan, Price Forecast (1st year), Annual Sales Forecast, Revenue Forecast (1st year), Annual Demand Growth, Working Capital (% of sales), current Treasury Bills (US) or CETES (Mexico) yield, Minimum Expected Return, Annual Interest Rate on Credit, Annual Inflation, Income Tax, sales history and fixed monthly expenses.

![General Info](./assets/README/DepreciationAnalysis1.png "General Info")

Generate the different depreciation alternatives such as Straight-Line (**SLN**), Double Declining Balance (**DDB**), Sum of Years (**SYD**) or Modified Accelerated Cost Recovery System (**MACRS**), based on the cost of the asset including installation (**I**), the salvage value at the end of the useful life (**S**), the useful life in years (**N**), and the multiplier in the case of accelerated methods (which is usually **2 or 1.5**).

![Depreciation Comparison](./assets/README/DepreciationAnalysis2.png "Depreciation Comparison")

Based on this we unify the depreciated value according to each method, to compare them by year and by specific periods according to useful life.

![Depreciation Analysis](./assets/README/DepreciationAnalysis3.png "Depreciation Analysis")

Considering the initial data and its evolution with inflation, demand growth, interest rate and the best depreciation method obtained before, We calculate the cash flow forecast with the investment and its impact on the **Net Present Value (NPV)**, **Internal Rate of Return (IRR)** and **Profitability Index (PI)** for the **expected scenario**.

![Expected Scenario](./assets/README/DepreciationAnalysis4.png "Expected Scenario")

Customization of the **pessimistic scenario**, such as removing the increase in demand (stagnation) or higher inflation.

![Pessimistic Scenario](./assets/README/DepreciationAnalysis5.png "Pessimistic Scenario")

Customization of the **optimistic scenario**, such as a further increase in demand.

![Optimistic Scenario](./assets/README/DepreciationAnalysis6.png "Optimistic Scenario")

Based on the NPV, IIR, and PI obtained for the pessimistic, expected and optimistic scenarios, we calculate a **sensitivity analysis**.

![Sensitivity Analysis](./assets/README/DepreciationAnalysis7.png "Sensitivity Analysis")

> [!NOTE]
> Finally, a structure will be added to systematize the measurement of the Degree of Operating (DOL), Financial (DFL) and Total (DTL) Leverage.

## Future work

- Degree of Leverage.
- Local UX/UI.


## Contact

Feel free to reach out if you have any questions or feedback.

- **Email:** victorbenitogr@gmail.com
- **The subject must start with:**  [OpenFinancialData]