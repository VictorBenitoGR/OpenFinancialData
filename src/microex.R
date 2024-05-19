K <- c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
L <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
PT <- c(0, 3, 7, 12, 16, 19, 21, 22, 22, 21, 15)

df <- data.frame(K, L, PT)
View(df)

df$PMe <- ifelse(L != 0, PT / L, 0)

df$PMg <- c(NA, diff(df$PT) / diff(df$L))

View(df)

# Plot
library(ggplot2)

plot1 <- ggplot(df, aes(x = L, y = PT)) +
  geom_point() +
  geom_line() +
  labs(x = "L", y = "PT") +
  ggtitle("L vs PT")

ggsave("plot1.jpg", plot1, width = 10, height = 10, units = "cm")

plot2 <- ggplot(df, aes(x = L)) +
  geom_line(aes(y = PMe), color = "blue") +
  geom_line(aes(y = PMg), color = "red") +
  labs(x = "L", y = "Value") +
  ggtitle("PMe Y PMg")

ggsave("plot2.jpg", plot2, width = 10, height = 10, units = "cm")

trabajadores_semana <- c(1, 2, 3, 4, 5, 6, 7)
producción_semana <- c(30, 70, 120, 160, 190, 210, 220)

df2 <- data.frame(trabajadores_semana, producción_semana)
View(df2)

df2$PMg <- c(df2$producción_semana[1] / df2$trabajadores_semana[1], diff(df2$producción_semana) / diff(df2$trabajadores_semana))

df2$PMe <- ifelse(trabajadores_semana != 0, producción_semana / trabajadores_semana, 0)

plot3 <- ggplot(df2, aes(x = trabajadores_semana, y = producción_semana)) +
  geom_point() +
  geom_line() +
  labs(x = "Trabajadores", y = "Producción") +
  ggtitle("Trabajadores vs Producción")

ggsave("plot3.jpg", plot3, width = 10, height = 10, units = "cm")

plot4 <- ggplot(df2, aes(x = trabajadores_semana)) +
  geom_line(aes(y = PMe), color = "blue") +
  geom_line(aes(y = PMg), color = "red") +
  labs(x = "Trabajadores", y = "Value") +
  ggtitle("PMe Y PMg")

ggsave("plot4.jpg", plot4, width = 10, height = 10, units = "cm")

View(df2)

trabajadores <- c(1, 2, 3, 4, 5, 6, 7)
surfboards <- c(30, 70, 120, 160, 190, 210, 220)

df3 <- data.frame(trabajadores, surfboards)

# Le vamos a pagar a cada trabajador $500 a la semana, y también enfrenta
# un costo fijo de $1000 por semana.

df3$CF <- 1000
df3$CV <- 500 * df3$trabajadores
df3$CT <- df3$CF + df3$CV
# df3$CMg has to be the value (CT n+1 - CT n) / surfboards
df3$CMg <- c(NA, diff(df3$CT) / diff(df3$surfboards))
View(df3)
# Grafiquemos CF, CV y CT

plot5 <- ggplot(df3, aes(x = trabajadores)) +
  geom_line(aes(y = CF), color = "blue") +
  geom_line(aes(y = CV), color = "red") +
  geom_line(aes(y = CT), color = "green") +
  labs(x = "Trabajadores", y = "Value") +
  ggtitle("CF, CV y CT")

ggsave("plot5.jpg", plot5, width = 10, height = 10, units = "cm")
