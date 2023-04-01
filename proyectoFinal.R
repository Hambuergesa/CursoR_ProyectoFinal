library(readr)
Gender_Inequality_Index <- read_csv("Curso R/Gender_Inequality_Index.csv")
View(Gender_Inequality_Index)

install.packages("testthat")

library(ggplot2)
library(testthat)


# Cargar los datos
datos <- read.csv("C:/Users/USUARIO/Documents/Curso R/Gender_Inequality_Index.csv")

# Filtrar los datos para excluir valores faltantes y extremadamente grandes
datos_filtrados <- datos[complete.cases(datos), ]
rango <- range(datos_filtrados$F_secondary_educ, datos_filtrados$M_secondary_educ)

# Calcular la media y la desviación estándar de los datos filtrados
media_f <- mean(datos_filtrados$F_secondary_educ)
desv_est_f <- sd(datos_filtrados$F_secondary_educ)
media_m <- mean(datos_filtrados$M_secondary_educ)
desv_est_m <- sd(datos_filtrados$M_secondary_educ)

# Establecer el rango de valores para el eje x
rango <- seq(min(datos_filtrados$F_secondary_educ, datos_filtrados$M_secondary_educ), 
             max(datos_filtrados$F_secondary_educ, datos_filtrados$M_secondary_educ), 
             length.out = 100)

# Crear la secuencia de valores para el eje x
x <- seq(rango[1], rango[2], length.out = 100)

# Crear vector de densidades de probabilidad normal
density_m <- function(x) dnorm(x, mean = media_m, sd = desv_est_m)
density_f <- function(x) dnorm(x, mean = media_f, sd = desv_est_f)

ggplot(data = datos_filtrados, aes(x = M_secondary_educ)) +
  geom_density(aes(fill = "M_secondary_educ"), alpha = 0.5, show.legend = TRUE) +
  stat_function(fun = density_m, aes(fill = "M_secondary_educ"), alpha = 0.5, color = "blue", size = 1) +
  geom_density(aes(x = F_secondary_educ, fill = "F_secondary_educ"), alpha = 0.5, show.legend = TRUE) +
  stat_function(fun = density_f, aes(fill = "F_secondary_educ"), alpha = 0.5, color = "red", size = 1) +
  scale_fill_manual(values = c("M_secondary_educ" = "blue", "F_secondary_educ" = "red")) +
  labs(x = "Índice de desigualdad de género en educación secundaria", y = "Densidad") +
  theme_classic()

# Agregar leyenda
legend("topright", legend = c("M_secondary_educ", "F_secondary_educ"), col = c("blue", "red"), lwd = 2)


# Prueba para verificar si la llamada a dnorm para y_m produce un error
expect_error({
  y_m <- dnorm(x, mean = media_m, sd = "desv_est_m")
})
