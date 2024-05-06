# Victoria tiene un salario anual de $1000000.
# Tiene casa de su propiedad, y una cuenta con $200000 de ahorros.
# Espera retirarse a los 60 años.
# Tiene la capacidad de invertir $100000 anuales además de lo que ya tiene.
# Ella ha establecido un monto de $50000 de sus ahorros para emergencias
# Victoria estima que necesitará lo equivalente a $5000000 al valor actual del
# dinero. Se espera que la inflación anual en promedio sea del 4% por año.

# ¿Cuánto dinero necesita tener al finalizar su inversión (en valor al momento
# de retiro)?

# Definir las variables
n <- 30 # número de años hasta el retiro
PV <- 5000000 # inversión inicial
i <- 0.04 # tasa de inflación
cuenta_de_ahorros <- 200000 # cuenta de ahorros
inversion_anual <- 100000 # inversión anual
fondo_de_emergencia <- 50000 # fondo de emergencia
meta <- 5000000 # meta de inversión

# Calcular el valor futuro
VF <- PV * (1 + i)^n
VF

# Calcular tasa de rendimiento anual de su inversión para llegar a su meta
# El resultado debería ser 9.19%

library(jrvFinance)
library(tidyquant)

# Flujos de efectivo
flujos_de_efectivo <- c(
  -PV, rep(inversion_anual, n), -meta + fondo_de_emergencia + cuenta_de_ahorros
)

# Calculate the IRR
IRR(flujos_de_efectivo)

# Ejercicio 2

dia <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
variacion <- c(NA, 0.02, -0.02, 0.02, -0.02, 0.02, -0.02, 0.02, -0.02, 0.02, -0.02)
valor <- c(100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

df <- data.frame(dia, variacion, valor)

# Calcular el valor de la acción para cada día

for (i in 2:nrow(df)) {
  df$valor[i] <- df$valor[i - 1] * (1 + df$variacion[i])
}

View(df)

df$rendimiento_lineal <- c(NA, diff(df$valor))
mean(df$rendimiento_lineal, na.rm = TRUE)
df$rendimiento_logaritmico <- c(NA, diff(log(df$valor)))
mean(df$rendimiento_logaritmico, na.rm = TRUE)
View(df)

# Ejercicio 3
library(quantmod)
years5 <- 365 * 5
# Descargar datos de Yahoo Finance

# Coca Cola
getSymbols("KO",
  from = Sys.Date() - years5,
  to = Sys.Date(),
  periodicity = "monthly",
  src = "yahoo"
)

# Pepsi
getSymbols("PEP",
  from = Sys.Date() - years5,
  to = Sys.Date(),
  periodicity = "monthly",
  src = "yahoo"
)

# Calcular el rendimiento logarítmico de cada acción
KO <- Ad(KO)
PEP <- Ad(PEP)

KO_rendimiento_logaritmico <- diff(log(KO))
PEP_rendimiento_logaritmico <- diff(log(PEP))

df_stock <- data.frame(KO_rendimiento_logaritmico, PEP_rendimiento_logaritmico)
View(df_stock)
means <- colMeans(df_stock, na.rm = TRUE)
means

varianza <- apply(df_stock, 2, var, na.rm = TRUE)
varianza

desviaciones_estandar <- apply(df_stock, 2, sd, na.rm = TRUE)
desviaciones_estandar

# * Una empresa farmacéutica mexicana acaba de comprar una empresa privada china
# * de su misma industria. Como resultado, la desviación estándar de la empresa
# * baja de 50% a 30% y su correlación con el mercado baja de 0.95 a 0.75.
# * Asumiendo que la desviación estándar y el rendimiento del mercado son del
# * 25% y 10% respectivamente, y con una tasa libre de riesgo del 3%. Calcula
# * a beta y el rendimiento esperado antes y después de la adquisición.

# Definir las variables
desviacion_estandar_antes <- 0.50
desviacion_estandar_despues <- 0.30
correlacion_antes <- 0.95
correlacion_despues <- 0.75
desviacion_estandar_mercado <- 0.25
rendimiento_mercado <- 0.10
tasa_libre_riesgo <- 0.03

# ? Beta (β): Esta es la fórmula para calcular el beta de una acción, que mide
# ? su sensibilidad a los movimientos del mercado.

# ? β = (σ_i * ρ_im) / σ_m

# ? Donde:
# ? σ_i es la desviación estándar de la acción (desviacion_estandar_antes y
# ? desviacion_estandar_despues)

# ? ρ_im es la correlación de la acción con el mercado (correlacion_antes y
# ? correlacion_despues)

# ? σ_m es la desviación estándar del mercado (desviacion_estandar_mercado)

# * Calcular el beta antes y después de la adquisición
beta_antes <- desviacion_estandar_antes * correlacion_antes /
  desviacion_estandar_mercado
beta_despues <- desviacion_estandar_despues * correlacion_despues /
  desviacion_estandar_mercado

# ? Rendimiento esperado (E[R_i]): Esta es la fórmula para calcular el
# ? rendimiento esperado de una acción según el CAPM.

# ? E[R_i] = R_f + β_i * (E[R_m] - R_f)

# ? Donde:

# ? R_f es la tasa libre de riesgo (tasa_libre_riesgo)
# ? β_i es el beta de la acción (beta_antes y beta_despues)
# ? E[R_m] es el rendimiento esperado del mercado (rendimiento_mercado)

# * Calcular el rendimiento esperado antes y después de la adquisición
rendimiento_esperado_antes <- tasa_libre_riesgo + beta_antes *
  (rendimiento_mercado - tasa_libre_riesgo)
rendimiento_esperado_despues <- tasa_libre_riesgo + beta_despues *
  (rendimiento_mercado - tasa_libre_riesgo)

# Imprimir los resultados como porcentajes
print(paste(
  "Beta antes de la adquisición: ",
  beta_antes, "%"
))

print(paste(
  "Beta después de la adquisición: ",
  beta_despues, "%"
))

print(paste(
  "Rendimiento esperado antes de la adquisición: ",
  rendimiento_esperado_antes * 100, "%"
))

print(paste(
  "Rendimiento esperado después de la adquisición: ",
  rendimiento_esperado_despues * 100, "%"
))


# * MBW es una empres automotriz alemana que tiene una desviación estándar de
# * sus rendimientos del 50%, y una correlación con el mercado de 0.65. Con una
# * tasa libre de riesgo del 3%, un rendimiento esperado del mercado de 13%, y
# * una desviación estándar del 23%, calcula el rendimiento esperado de MBW.

# Definir las variables
desviacion_estandar_mbw <- 0.50
correlacion_mbw <- 0.65
desviacion_estandar_mercado <- 0.23
rendimiento_mercado <- 0.13
tasa_libre_riesgo <- 0.03

# Calcular el beta de MBW
beta_mbw <- desviacion_estandar_mbw * correlacion_mbw /
  desviacion_estandar_mercado

# Calcular el rendimiento esperado de MBW
rendimiento_esperado_mbw <- tasa_libre_riesgo + beta_mbw *
  (rendimiento_mercado - tasa_libre_riesgo)

# Imprimir el resultado como porcentaje
print(paste(
  "Rendimiento esperado de MBW: ",
  rendimiento_esperado_mbw * 100, "%"
))

library(quantmod)

# Descargar datos de Yahoo Finance de SP500 y KO
getSymbols("^GSPC",
  from = Sys.Date() - 365 * 5,
  to = Sys.Date(),
  periodicity = "monthly",
  src = "yahoo"
)

getSymbols("KO",
  from = Sys.Date() - 365 * 5,
  to = Sys.Date(),
  periodicity = "monthly",
  src = "yahoo"
)

# Ponerlas en el mismo dataframe
df <- data.frame(
  KO = Ad(KO),
  SP500 = Ad(GSPC)
)

# Convertir la columna a numérica
df$KO <- as.numeric(df$KO)

# Manejar los valores NA
df$KO[is.na(df$KO)] <- 0

# Convertir la columna a numérica
df$SP500 <- as.numeric(df$GSPC)

# Manejar los valores NA
df$SP500[is.na(df$SP500)] <- 0

# Calcular los rendimientos logarítmicos
df$KO_rendimiento_logaritmico <- c(NA, diff(log(df$KO)))
df$SP500_rendimiento_logaritmico <- c(NA, diff(log(df$SP500)))

# Calcular la varianza y desviación estándar de los rendimientos
varianza <- apply(df, 2, var, na.rm = TRUE)
varianza
desviaciones_estandar <- apply(df, 2, sd, na.rm = TRUE)
desviaciones_estandar

# T-Bill 0.44%
tasa_libre_riesgo <- 0.0044

# Calcular el beta de KO
beta_ko <- cov(df$KO_rendimiento_logaritmico, df$SP500_rendimiento_logaritmico, use = "complete.obs") /
  var(df$SP500_rendimiento_logaritmico, na.rm = TRUE)
beta_ko

# El departamento de finanzas de una empresa ha puesto en práctica el plan
# Navidad Feliz para sus empleados. Consiste en ahorrar quincenalmente $500,
# empezando el 15 de enero y terminando el 31 de noviembre. El 15 de abril, con
# el pago de utilidades por parte de la empresa, se ahorrarán $1 000
# adicionales. Pero, el 31 de agosto y 15 de septiembre, por el gasto de la
# escuela de los hijos, no se efectuará ningún depósito. Si la empresa, por esta
# serie de aportaciones, se compromete a dar $12860 a cada ahorrador el 15 de
# diciembre de cada año, ¿cuál es la tasa de interés quincenal que está pagando?

# Definir las variables
ahorro_quincenal <- 500
ahorro_adicional <- 1000
ahorro_escuela <- 0
meta <- 12860
n <- 11

# Calcular el valor futuro de los ahorros
VF <- ahorro_quincenal * 2 * 11 + ahorro_adicional
VF

# Calcular la tasa de interés quincenal
i <- (meta - VF) / VF
i

# Imprimir el resultado como porcentaje
print(paste(
  "Tasa de interés quincenal: ",
  i * 100, "%"
))

# Suponga que una acción pagará un dividendo de $2.00 el próximo año. Usted
# quiere un rendimiento del 12% y espera que los dividendos de la emisora
# crezcan al 8% anual. ¿Cuál es el precio esperado de la acción?

# Definir las variables
D <- 2.00  # El dividendo esperado en un año
r <- 0.12  # La tasa de rendimiento requerida
g <- 0.08  # La tasa a la que se espera que crezca el dividendo

# Calcular el precio de la acción
P <- D / (r - g)
P

# Qué le pasaría al precio de la acción del 