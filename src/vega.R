# * OpenFinancialData | VEGA | GPL-3.0 license
# * https://github.com/VictorBenitoGR/OpenFInancialData

# *** Packages ----------------------------------------------------------------

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

# *** Financial Statements ----------------------------------------------------

library(readxl)

# Comparativos de Reportes Financieros Nominales

# Razón Social: GRUPO HERDEZ, S.A.B. DE C.V.
# Ticker: HERDEZ
# Año Inicial: 2016
# Año Final: 2021

herdez <- read_excel("./data/herdez_financial_statements.xlsx", sheet = "financial_statement")
View(herdez)
# D
