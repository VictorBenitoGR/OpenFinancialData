# * Cálculo de ahorro para el retiro con y sin Afore (México)
# * GitHub: OpenFinancialData | @VictorBenitoGR

# *** Librerías ---------------------------------------------------------------

library(ggplot2) # Visualización de datos
library(scales) # Manejo de escalas
library(ggtext) # Personalización de texto
library(ggpattern) # Patrones de relleno
library(ggthemes) # Temas alternativos para ggplot2
library(hrbrthemes) # Temas alternativos para ggplot2

# *** Datos iniciales ---------------------------------------------------------

# * Variables del usuario
edad_actual <- 25
edad_retiro <- 65
esperanza_vida <- 85

# * Variables de la Afore
rendimiento_anual <- 0.06 # 6.00% de rendimiento anual promedio
inflacion_anual <- 0.0469 # 4.69% Tasa de inflación anual promedio (INEGI, 2024)
comision_anual <- 0.0057 # 0.57% Comisión anual máxima (SCJN, 2024)

# * Cálculos iniciales
meses_retirado <- 12 * (esperanza_vida - edad_retiro)
anios_retirado <- esperanza_vida - edad_retiro
edad <- seq(edad_actual, edad_retiro)
anios <- seq(1, edad_retiro - edad_actual + 1)
aportaciones_mensuales <- 500
aportaciones_anuales <- aportaciones_mensuales * 12

# *** Saldo acumulado con y sin Afore -----------------------------------------

# Calcular saldo acumulado y ajustar por inflación y comisiones
saldo_acumulado <- numeric(length(anios))
for (i in seq_along(anios)) {
  if (i == 1) {
    saldo_acumulado[i] <- aportaciones_anuales *
      (1 + rendimiento_anual - comision_anual)
  } else {
    saldo_acumulado[i] <- (saldo_acumulado[i - 1] + aportaciones_anuales) *
      (1 + rendimiento_anual - comision_anual)
  }
}

saldo_acumulado_ajustado_inf <-
  saldo_acumulado / (1 + inflacion_anual)^(30 - anios)

# Saldo sin afore, ajustado por inflación
saldo_sin_afore <- cumsum(rep(aportaciones_anuales, length(anios)))
saldo_sin_afore_ajustado_inf <-
  saldo_sin_afore / (1 + inflacion_anual)^(30 - anios)

datos <- data.frame(
  anios, saldo_acumulado_ajustado_inf, saldo_sin_afore_ajustado_inf
)

# Modificar la secuencia
datos$anios <- edad

View(datos)

# *** Pensión a recibir -------------------------------------------------------

# Cantidad de pensión
pension_anual_afore <-
  datos$saldo_acumulado_ajustado_inf[nrow(datos)] / anios_retirado

pension_anual_sin_afore <-
  datos$saldo_sin_afore_ajustado_inf[nrow(datos)] / anios_retirado

pension_mensual_afore <-
  datos$saldo_acumulado_ajustado_inf[nrow(datos)] / meses_retirado

pension_mensual_sin_afore <-
  datos$saldo_sin_afore_ajustado_inf[nrow(datos)] / meses_retirado

# *** Evolución del saldo durante la jubilación -------------------------------

# Último saldo acumulado ajustado por inflación y comisiones
ultimo_saldo_afore <- tail(datos$saldo_acumulado_ajustado_inf, 1)
ultimo_saldo_sin_afore <- tail(datos$saldo_sin_afore_ajustado_inf, 1)

# Secuencia de años de retiro
anios_pensionado <- seq(edad_retiro + 1, esperanza_vida)

# Inicializar vectores para los saldos
saldo_acumulado_ajustado_inf <- numeric(length(anios_pensionado))
saldo_sin_afore_ajustado_inf <- numeric(length(anios_pensionado))

# Calcular saldos para cada año de retiro
for (i in seq_along(anios_pensionado)) {
  if (i == 1) {
    # Primer año después del retiro
    saldo_previo_afore <- ultimo_saldo_afore
    saldo_previo_sin_afore <- ultimo_saldo_sin_afore
  } else {
    # Años subsiguientes
    saldo_previo_afore <- saldo_acumulado_ajustado_inf[i - 1]
    saldo_previo_sin_afore <- saldo_sin_afore_ajustado_inf[i - 1]
  }

  # Ajustar por rendimiento, comisión e inflación (con Afore)
  saldo_acumulado_ajustado_inf[i] <-
    (saldo_previo_afore - pension_anual_afore) *
      (1 + rendimiento_anual - comision_anual) * (1 - inflacion_anual)

  # Ajustar solo por inflación (sin Afore) y dejar en 0 si es negativo
  saldo_sin_afore_ajustado_inf[i] <-
    max((saldo_previo_sin_afore - pension_anual_sin_afore) *
      (1 - inflacion_anual), 0)
}

# Crear el dataframe 'pensionado'
pensionado <- data.frame(
  anios = anios_pensionado,
  saldo_acumulado_ajustado_inf = saldo_acumulado_ajustado_inf,
  saldo_sin_afore_ajustado_inf = saldo_sin_afore_ajustado_inf
)

View(pensionado)

# Combinar los datos de ahorro y retiro
saldo_final <- rbind(datos, pensionado)

View(saldo_final)

# *** Visualización del ahorro con y sin Afore --------------------------------

# Simplificar eje Y
etiqueta_millones <- function(x) {
  ifelse(
    x == 0, "0", paste0(
      "$", format(round(x / 1e6, 1), big.mark = ",", nsmall = 1), "M"
    )
  )
}

# Calcular puntos intermedios del eje X
rango_total <- esperanza_vida - edad_actual
paso <- rango_total / 3
punto_medio1 <- edad_actual + paso
punto_medio2 <- edad_actual + (2 * paso)

# Visualización
afore_ajustado_inflacion <- ggplot(saldo_final, aes(x = anios)) +
  geom_area_pattern(aes(y = saldo_acumulado_ajustado_inf, fill = "Con Afore"),
    pattern = "gradient",
    fill = "00000000",
    pattern_fill = "#00000000",
    pattern_fill2 = "#6ED13F",
    color = "black", alpha = 0.5
  ) +
  geom_area_pattern(aes(y = saldo_sin_afore_ajustado_inf, fill = "Sin Afore"),
    pattern = "gradient",
    fill = "00000000",
    pattern_fill = "#00000000",
    pattern_fill2 = "#7BB1FF",
    color = "black", alpha = 0.5
  ) +
  labs(
    title = paste0(
      "Tus ahorros ",
      "<span style='color:#329f00;'>con AFORE</span> o ",
      "<span style='color:#0047ab;'>sin él</span>",
      " 👑"
    ),
    caption = "GitHub: OpenFinancialData | @VictorBenitoGR", x = "Edad", y = ""
  ) +
  scale_x_continuous(
    limits = c(edad_actual, esperanza_vida),
    breaks = c(
      edad_actual, round(punto_medio1), round(punto_medio2), esperanza_vida
    )
  ) +
  theme_ipsum() +
  theme(
    text = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black", face = "bold"),
    axis.text = element_text(color = "black", face = "bold"),
    legend.position = "none",
    plot.title = element_markdown()
  ) +
  scale_y_continuous(labels = etiqueta_millones) +
  annotate(
    geom = "richtext",
    x = edad_actual + 1, y = tail(datos$saldo_acumulado_ajustado_inf, 1) * .95,
    label = paste0(
      "<b>Edad:</b> ", edad_actual, " años<br>",
      "<b>Retiro:</b> ", edad_retiro, " años<br>",
      "<b>Aportación mensual:</b> $", aportaciones_mensuales, "<br>",
      "<b>Rendimiento anual:</b> ", rendimiento_anual * 100, "%<br>",
      "<b>Inflación anual:</b> ", inflacion_anual * 100, "%<br>",
      "<b>Comisión anual:</b> ", comision_anual * 100, "%<br>",
      "<b>Pensión mensual Afore:</b> $", round(pension_mensual_afore), "<br>",
      "<b>Pensión mensual sin él:</b> $", round(pension_mensual_sin_afore)
    ),
    hjust = 0, vjust = 1, size = 3,
    label.padding = unit(1, "lines"), label.r = unit(0, "lines"),
    fill = "#d3d3d356"
  )

# Exportación
ggsave(
  "assets/afore.jpg",
  plot = afore_ajustado_inflacion, width = 7.5, height = 4.5, dpi = 600
)
