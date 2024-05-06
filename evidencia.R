# * OpenFinancialData (proyecto propio) | SOLARIS | GPL-3.0
# * https://github.com/VictorBenitoGR/OpenFInancialData

# *** PACKAGES *** ------------------------------------------------------------

# ? Paquetes utilizados en este script:
# quantmod      Marco de modelado financiero cuantitativo y comercio
# fredr         Acceso a la API de Datos Económicos de la Reserva Federal (FRED)
# TTR           Reglas de Comercio Técnico
# rdrop2        Interfaz de Dropbox para R
# rvest         Web scraping y análisis de HTML/XML
# openxlsx      Lectura, escritura y edición de archivos Excel
# siebanxicor   Interfaz para la API de Siebanxicor
# lubridate     Trabajo con fechas y horas
# xts           Manejo uniforme de diferentes clases de datos basados en tiempo
# PortfolioAnalytics    Análisis de cartera, optimización y backtesting
# PerformanceAnalytics  Herramientas econométricas para análisis de rendimiento
# tibble        Marcos de datos simples
# tidyverse     Paquetes de manipulación y visualización de datos
# ggplot2       Visualización de datos
# ggpattern     Geoms para geoms rellenos con patrones
# hrbrthemes    Temas de ggplot2 centrados en la tipografía y con opiniones
# conflicted    Manejo de conflictos entre funciones en paquetes de R

# ! Instalar y cargar paquetes
source("./src/install_packages.R")


# *** OBTENER SÍMBOLOS DE TICKER *** ------------------------------------------

# * Obtener los símbolos de ticker
# ? Ejecute "which python3" (Linux/macOS) o "where python" (Windows) en su
# ? terminal para conocer la ruta de su Python3. Esto va a scrapear la lista
# ? del SP500. Adjunto el archivo en /data/ dentro del repositorio.
system("/usr/bin/python3 ./src/sp500_scrape.py")

# Importar una lista de tickers S&P 500
sp500 <- read.csv("./data/sp500.csv")

colnames(sp500)[colnames(sp500) == "Symbol"] <- "Tickers"

# Algunos tickers tienen ".", pero Yahoo Finance usa "-", esto los reemplaza
sp500$Tickers <- gsub("\\.", "-", sp500$Tickers)

# Reemplazar "." con "_" en los nombres de las columnas
colnames(sp500) <- gsub("\\.", "_", colnames(sp500))

View(sp500)


# *** Obtener los precios mensuales del S&P 500 *** ---------------------------

# Aplicar getSymbols a cada uno de los tickers
list_of_tickers <- lapply(sp500$Tickers, function(symbol) {
  data <- na.omit(getSymbols(symbol,
    src = "yahoo",
    periodicity = "monthly",
    from = Sys.Date() - 1826, # ! 1826 días = 5 años
    to = Sys.Date(),
    auto.assign = FALSE
  ))
  assign(symbol, data, envir = .GlobalEnv)
  return(data)
})

# Cambiar los nombres de las columnas que contienen "-" a "_"
list_of_tickers <- lapply(list_of_tickers, function(xts_obj) {
  colnames(xts_obj) <- gsub("-", "_", colnames(xts_obj))
  return(xts_obj)
})

# *** Usar solo los precios ajustados *** -------------------------------------

# * Para este ejercicio solo tomaremos como referencia los precios ajustados
adjusted_price <- function(df) {
  adjusted_columns <- grep("Adjusted", names(df), value = TRUE)
  return(df[, adjusted_columns, drop = FALSE])
}

portfolio_adjusted <- lapply(list_of_tickers, adjusted_price)


# *** Función | XTS TO DF *** ------------------------------------------------

# * Función para convertir listas xts a dataframes
xts_to_df <- function(xts_object) {
  df <- as.data.frame(xts_object)
  return(df)
}


# *** XTS a DF *** -----------------------------------------------------------

# * Adjusted
# Convertir cada objeto xts a un dataframe
portfolio_adjusted <- lapply(portfolio_adjusted, xts_to_df)

# Encontrar el número máximo de filas
max_rows <- max(sapply(portfolio_adjusted, nrow))

portfolio_adjusted <- lapply(portfolio_adjusted, function(df) {
  if (nrow(df) < max_rows) {
    # Crear un data frame con valores NA para las filas faltantes
    na_df <- data.frame(
      matrix(NA, ncol = ncol(df), nrow = max_rows - nrow(df))
    )
    names(na_df) <- names(df)

    # Asegurarse de que los NA estén al inicio y no al final
    df <- rbind(na_df, df)
  }
  return(df)
})

# Obtener el dataframe
portfolio_adjusted <- do.call(cbind, portfolio_adjusted)


# *** Función | Remover sufijos *** ------------------------------------------

# Para mejorar la lectura y usabilidades, quitamos los tipos de precio
remove_suffixes <- function(df) {
  colnames(df) <- gsub(
    "\\.Open$|\\.High$|\\.Low$|\\.Close$|\\.Volume$|\\.Adjusted$",
    "", colnames(df)
  )
  return(df)
}

# Aplicar a cada los precios ajustados
portfolio_adjusted <- remove_suffixes(portfolio_adjusted)

View(portfolio_adjusted)

source("./src/df_a_imagen.R")

# Convertir el dataframe a una imagen
df_a_imagen(
  portfolio_adjusted,
  ancho = 8100, alto = 2000, n_rows = 10, n_cols = 5, res = 600, version = "1"
)


# *** Descartar compañías con valores NA *** ----------------------------------
# Algunas compañías han estado menos de 5 años en el mercado, por lo que tienen
# NAs al principio. Esto implica menos datos con los que trabajar, por lo que
# serán eliminados

print(ncol(portfolio_adjusted)) # 502

# Guardar el nombre original de las columnas
original_colnames <- colnames(portfolio_adjusted)

portfolio_adjusted <- portfolio_adjusted[, colSums(
  is.na(portfolio_adjusted)
) == 0]

# Actualiza el número de tickers
print(ncol(portfolio_adjusted)) # 493, 9 fueron eliminados

# Encontrar cuáles son
removed_colnames <- setdiff(original_colnames, colnames(portfolio_adjusted))

# Filtrar sp500 para incluir solo los tickers eliminados
removed_tickers <- sp500[sp500$Tickers %in% removed_colnames, ]

# Seleccionar solo las columnas relevantes
removed_tickers <- removed_tickers[, c("Tickers", "Security", "Date_added")]

View(removed_tickers)

# ? README image
df_a_imagen(
  removed_tickers,
  ancho = 2600, alto = 1000
)

View(portfolio_adjusted)

# *** Obtener benchmark y T-Bills mensuales *** -------------------------------

# * Benchmark - SP500 o "^GSPC"
na.omit(getSymbols(
  "^GSPC",
  src = "yahoo",
  periodicity = "monthly",
  from = Sys.Date() - 1826, # 1826days = 5years
  to = Sys.Date()
))

benchmark <- list(GSPC)

# Separar los precios ajustados
benchmark_adjusted <- lapply(benchmark, adjusted_price)

# Convertir el benchmark a un dataframe
benchmark_adjusted <- lapply(benchmark_adjusted, xts_to_df)
benchmark_adjusted <- do.call(cbind, benchmark_adjusted)

# Remover los sufijos
benchmark_adjusted <- remove_suffixes(benchmark_adjusted)

View(benchmark_adjusted)

# * 3-Month Treasury Bill Secondary Market Rate, Discount Basis (TB3MS)
na.omit(getSymbols(
  "TB3MS", # Los diarios serían con DGS3M0
  src = "FRED", # Federal Reserve Economic Data
  from = Sys.Date() - 1826, # 1826days = 5years
  to = Sys.Date()
))

tbills <- list(TB3MS)

# Usar lapply para convertir cada objeto xts a un dataframe
tbills_df <- lapply(tbills, xts_to_df)
tbills_df <- do.call(cbind, tbills_df)

# El dato (%) no se exporta como decimal, por lo que se divide por 100
# Además, es una tasa anualizada, por lo que se divide por 12
tbills_df$TB3MS <- tbills_df$TB3MS / 100 / 12

class(tbills_df$TB3MS) # ! Tiene que ser numérico

View(tbills_df)


# *** Calcular métricas generales del portafolio potencial *** ----------------

# Función para calcular métricas generales del portafolio potencial
portfolio_metrics <- function(df, benchmark_adjusted, tbills_df) {
  # Dividir cada fila por la anterior aplicando el logaritmo natural
  df <- log(df / lag(df))
  benchmark_adjusted$GSPC <- log(
    benchmark_adjusted$GSPC /
      lag(benchmark_adjusted$GSPC)
  )

  # Reeplazar NA y valores Inf con 0
  df[is.na(df) | df == Inf] <- 0
  benchmark_adjusted$GSPC[
    is.na(benchmark_adjusted$GSPC) | benchmark_adjusted$GSPC == Inf
  ] <- 0

  # * Retorno promedio
  # ? R(i) = El retorno promedio del portafolio o inversión
  average <- colMeans(df, na.rm = TRUE) * 100

  market_average <- colMeans(benchmark_adjusted, na.rm = TRUE) * 100

  # * Tasa libre de riesgo
  # ? R(f) = La tasa libre de riesgo para el período de tiempo
  risk_free_rate <- mean(tbills_df$TB3MS, na.rm = TRUE) # Tasa promedio

  # * Varianza
  # ? Variance = El cuadrado de la desviación estándar,
  # ? una medida de la dispersión de los retornos
  variance <- apply(df, 2, var, na.rm = TRUE) * 100

  # * Desviación estándar
  # ? Std Deviation = Una medida de la cantidad de variación
  # ? o dispersión de los retornos
  std_deviation <- apply(df, 2, sd, na.rm = TRUE) * 100

  # * Beta:
  # ? B = El beta del portafolio o inversión, una medida
  # ? de la volatilidad de la inversión en comparación con el mercado
  betas <- sapply(names(df), function(col) {
    formula <- as.formula(paste(col, "~ GSPC"))
    regression_result <- lm(formula, data = cbind(
      df,
      GSPC = benchmark_adjusted$GSPC
    ))
    coef(regression_result)[2]
  })

  # * R Cuadrada
  # ? R^2 = La proporción de la varianza en la variable dependiente
  # ? que es predecible a partir de la variable independiente
  r_squared <- sapply(names(df), function(col) {
    formula <- as.formula(paste(col, "~ GSPC"))
    regression_result <- lm(formula, data = cbind(
      df,
      GSPC = benchmark_adjusted$GSPC
    ))
    summary(regression_result)$r.squared
  })

  # * Sharpe = (R(i) - R(f)) / Std Deviation
  # ? R(i) = El retorno promedio del portafolio o inversión
  # ? R(f) = La tasa libre de riesgo para el período de tiempo
  # ? Std Deviation = La cantidad de variación o dispersión de los retornos
  # Un valor más alto es mejor. Indica que los retornos son mejores para
  # el nivel de riesgo dado. Un Sharpe negativo indica que la tasa libre
  # de riesgo es mayor que el retorno del portafolio
  sharpe <- (average - risk_free_rate) / std_deviation

  # * Treynor = (R(i) - R(f)) / B
  # ? R(i) = El retorno promedio del portafolio o inversión
  # ? R(f) = La tasa libre de riesgo para el período de tiempo
  # ? B = El beta del portafolio o inversión
  # Un valor más alto es mejor. Indica que la inversión tiene un retorno
  # ajustado al riesgo más alto. Un Treynor negativo podría indicar que
  # la inversión tiene un retorno más bajo que la tasa libre de riesgo
  treynor <- (average - risk_free_rate) / betas

  # * Jensen's Alpha = R(i) - (R(f) + B * (R(m) - R(f)))
  # ? R(i) = El retorno promedio del portafolio o inversión
  # ? R(m) = El retorno promedio del índice de mercado apropiado
  # ? R(f) = La tasa libre de riesgo para el período de tiempo
  # ? B = El beta del portafolio o inversión
  # Un valor más alto es mejor. Indica que el portafolio o inversión
  # está superando el retorno esperado dado su beta (como superar al mercado
  # en una base ajustada al riesgo). Un valor negativo indica bajo rendimiento
  jensen_alpha <-
    average - (risk_free_rate + betas * (market_average - risk_free_rate))

  # Métricas para cada ticker
  metrics <- data.frame(
    average, variance, std_deviation, betas,
    r_squared, sharpe, treynor, jensen_alpha
  )

  # ? Usa tibble
  # Usa los tickers (nombres de las columnas) como nombres de las filas
  metrics <- rownames_to_column(metrics, "Tickers")

  return(metrics)
}


# *** Métricas generales *** --------------------------------------------------

portfolio_adjusted_metrics <- portfolio_metrics(
  portfolio_adjusted, benchmark_adjusted, tbills_df
)

portfolio_adjusted_metrics <- portfolio_adjusted_metrics[
  portfolio_adjusted_metrics$average > 0,
]

df_a_imagen(
  portfolio_adjusted_metrics,
  ancho = 4400, alto = 1150, n_rows = 10
)

View(portfolio_adjusted_metrics)

# Hacer que el df portfolio_adjusted solo tenga los precios de las 8 empresas
# con mayores rendimientos (average) en portfolio_adjusted_metrics

# Ordenar por rendimiento promedio
portfolio_adjusted_metrics <- portfolio_adjusted_metrics[
  order(portfolio_adjusted_metrics$average, decreasing = TRUE),
]

# Seleccionar los 8 primeros
top_8_tickers <- portfolio_adjusted_metrics$Tickers[1:8]

# Filtrar el dataframe original
portfolio_adjusted <- portfolio_adjusted[, top_8_tickers]

View(portfolio_adjusted)

# Convertir el dataframe a una imagen
df_a_imagen(
  portfolio_adjusted,
  ancho = 6800, alto = 1150, n_rows = 10, version = "top_8"
)
View(tbills_df)
# *** Exportar a un archivo Excel *** -----------------------------------------

# Crear un nuevo libro de Excel
wb <- createWorkbook()

# Añadir hojas al libro
addWorksheet(wb, "Portfolio Adjusted")
addWorksheet(wb, "Benchmark Adjusted")
addWorksheet(wb, "TBills")

# Escribir los datos en las hojas
writeData(wb, "Portfolio Adjusted", portfolio_adjusted, rowNames = TRUE)
writeData(wb, "Benchmark Adjusted", benchmark_adjusted, rowNames = TRUE)
writeData(wb, "TBills", tbills_df, rowNames = TRUE)

# Guardar el libro de Excel
saveWorkbook(wb, file = "./data/portfolio_data.xlsx", overwrite = TRUE)

# *** Frontera eficiente *** --------------------------------------------------

# Calcular los rendimientos mensuales
returns <- na.omit(ROC(portfolio_adjusted, type = "discrete"))

# Crear un objeto de cartera
port <- portfolio.spec(assets = colnames(returns))

# Añadir restricciones
port <- add.constraint(portfolio = port, type = "full_investment")
port <- add.constraint(portfolio = port, type = "long_only")

# Realizar la optimización de la cartera
optimized.portfolio <- optimize.portfolio(R = returns, portfolio = port, optimize_method = "ROI", trace = FALSE)

# Calcular la frontera eficiente
eff.frontier <- portfolioFrontier(returns, port)

# Extraer los datos de la frontera eficiente
frontier_data <- eff.frontier@portfolio

# Trazar la frontera eficiente con ggplot2
ggplot(frontier_data, aes(x = StdDev, y = Return)) +
  geom_line() +
  labs(x = "Standard Deviation", y = "Expected Return", title = "Efficient Frontier")

# *** Optimizar el portafolio *** ---------------------------------------------

library(PerformanceAnalytics)
library(PortfolioAnalytics)

# Calcular los rendimientos mensuales
returns <- na.omit(ROC(portfolio_adjusted, type = "discrete"))

# Crear un objeto de cartera
port <- portfolio.spec(assets = colnames(returns))

# Añadir restricciones
port <- add.constraint(portfolio = port, type = "full_investment")
port <- add.constraint(portfolio = port, type = "long_only")

# Realizar la optimización de la cartera
optimized.portfolio <- optimize.portfolio(
  R = returns, portfolio = port, optimize_method = "ROI", trace = FALSE
)

# Imprimir los resultados
print(optimized.portfolio)

# Optimal Weights:
# SMCI NVDA TSLA BLDR MRNA  LLY  PWR KLAC
#    1    0    0    0    0    0    0    0
library(readxl)

# Leer el archivo xlsx
portfolio_data <- read_excel("portfolio_data.xlsx")

# Ver las primeras filas de los datos
head(portfolio_data)

# Tomando como referencia "Portafolio Adjusted" de portfolio_data.xlsx, partiendo
# de la selección de 8 activos iniciales, creamos un portafolio de inversión en
# donde se muestre la proporción de cada activo dentro de la cartera buscando la
# mejor relación rendimiento/riesgo. Crea la gráfica de mínima varianza en donde
# se muestre claramente el punto del portafolio óptimo y la línea de colocación
# del capital (CAL) combinando con el activo libre de riesgo.

