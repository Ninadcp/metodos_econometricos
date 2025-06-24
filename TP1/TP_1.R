library(tidyverse)
library(dplyr)

rm(list = ls())

#setwd("C:\\LUCA\\UdeSA\\Métodos Econométricos y Organización Industrial Aplicada\\TPs\\TP 1")
setwd("/Users/ninadicostanzopereira/Desktop/metodos_econometricos/TP1")
#setwd("lujan")
datos <- read.csv("Base de datos TP 1.csv")

#1. Estimen y grafiquen la función de distribución acumulada para toda la muestra (ecdf), sin distinguir entre diferentes 
#grupos de personas. Interpreten el valor de la ECDF cuando el tiempo de supervivencia es 50 unidades de tiempo.

ggplot(datos, aes(x = time)) +
  stat_ecdf(color = "blue", pad = F, linewidth = 1) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "red") +
  labs(title = "Función de Distribución Acumulada Empírica (ECDF)",
       x = "Tiempo de supervivencia (días)",
       y = "Probabilidad acumulada") +
  theme_classic()

#Tiempo de supervivencia es 50 días
ecdf_fun <- ecdf(datos$time)
ecdf_fun(50)

#2. Estimen y grafiquen la función de supervivencia e interpreten los resultados obtenidos, ¿cómo describirían los resultados 
#del tratamiento sobre la supervivencia? (mayores momentos de mortalidad, tiempo mediano de supervivencia, etc). 1 Bonus: pue
#-den agregar intervalos de confianza.

t_values <- 0:212
densidad <- numeric(length(t_values))

# Calcular 1 - ECDF(t)
for (i in seq_along(t_values)) {
  t <- t_values[i]
  densidad[i] <- 1 - ecdf_fun(t)
}

# Graficamos
plot(t_values, densidad, type = "l", col = "blue",
     xlab = "t", ylab = "1 - ECDF(t)", main = "Función de densidad")

set.seed(123)  # Para reproducibilidad

t_values <- 0:212
n_boot <- 1000  # Número de muestras bootstrap
densidad <- numeric(length(t_values))

# ECDF original
ecdf_fun <- ecdf(datos$time)

# Calcular 1 - ECDF(t)
for (i in seq_along(t_values)) {
  t <- t_values[i]
  densidad[i] <- 1 - ecdf_fun(t)
}

# Bootstrap: generar matriz donde cada columna es una muestra bootstrap
supervivencia_boot <- matrix(NA, nrow = length(t_values), ncol = n_boot)

for (b in 1:n_boot) {
  muestra <- sample(datos$time, replace = TRUE)
  ecdf_boot <- ecdf(muestra)
  for (i in seq_along(t_values)) {
    t <- t_values[i]
    supervivencia_boot[i, b] <- 1 - ecdf_boot(t)
  }
}

# Calcular intervalos de confianza al 95% para cada t
lower_ic <- apply(supervivencia_boot, 1, quantile, probs = 0.025)
upper_ic <- apply(supervivencia_boot, 1, quantile, probs = 0.975)

# Graficar con IC
png("superviviencia_IC.png", width=800, height=400)
plot(t_values, densidad, type = "l", col = "red", lwd = 2,
     xlab = "Tiempo de supervivencia (días)",
     ylab = expression("Probabilidad de sobrevivir más allá de t: 1 - ECDF(t)"),
     main = "Estimación de la función de supervivencia con IC")

lines(t_values, lower_ic, col = "pink", lty = 2)
lines(t_values, upper_ic, col = "pink", lty = 2)

mtext("Intervalos de confianza calculados vía booststrap (R = 1000)")
dev.off()

#3. Estimen la densidad de los tiempos de supervivencia utilizando alguno de los kernels vistos en clase (definan el bandwith 
#con la Regla de Oro de Silverman) y grafiquen. ¿Cuáles son los tiempos de supervivencia más comunes? ¿Qué pueden decir sobre 
#la variabilidad de los tiempos de supervivencia? Comparen la densidad estimada en el tiempo 50 y 10 e interpreten.

#Por simplicidad, usamos el kernel gaussiano de segundo órden, donde el bandwith óptimo según la regla de oro tiene la siguien
#-te expresión: h = 1.06 * sigma_hat * n ^ {-1 / 5}. Luego:

n <- length(datos$time)
sigma_hat <- sd(datos$time)
bw_gr <- 1.06 * sigma_hat * n ^ (-1/5)

densidad <- density(datos$time, kernel = "gaussian", bw = bw_gr, adjust = 1)

#10 tiempos más comunes
top_10 <- densidad$x[order(densidad$y, decreasing = T)[1:10]]
print(top_10)

#Densidad estimada en time = 50 vs time = 10
dens_10 <- approx(densidad$x, densidad$y, xout = 10)$y
dens_50 <- approx(densidad$x, densidad$y, xout = 50)$y

cat("Densidad estimada en el tiempo 10:", dens_10, "Densidad estimada en el tiempo 50:", dens_50, "\n")

#Graficamos
ggplot(datos, aes(x = time)) +
  geom_density(kernel = "gaussian", bw = bw_gr, adjust = 1, fill = "blue") +
  geom_vline(xintercept = c(10, 50), linetype = "dashed", color = "red") +
  labs(title = "Estimación de densidad de los tiempos de supervivencia",
       x = "Tiempo de supervivencia (días)",
       y = "Densidad estimada") +
  theme_classic()

#4. Generen un grupo de gráficos que muestre cómo cambia la estimacion de la función de distribución acumulada empírica a medi
#-da que aumenta la cantidad de observaciones. Para esto pueden sacar muestras aleatorias de la base con diferente cantidad de 
#observaciones. Expliquen lo que observan.

tamaño <- c(100, 500, 1000, 5000, 10000)

df_total <- data.frame()

for (n in tamaño) {
  muestra <- sample(datos$time, size = n, replace = FALSE)
  df <- data.frame(time = muestra, n = as.factor(n))
  df_total <- rbind(df_total, df)
}

ggplot(df_total, aes(x = time, color = n)) +
  stat_ecdf(linewidth = 1, pad = F) +
  labs(title = "Funciones ECDF para diferentes tamaños de muestra",
       x = "Tiempo de supervivencia (días)",
       y = "Probabilidad acumulada",
       color = "Tamaño de muestra (n)") +
  theme_classic()

#5. Generen un grupo de gráficos que muestre cómo cambia la estimación de densidad de los tiempos de supervivencia para dife
#-rentes valores del bandwith (utilice un kernel Gaussiano). Expliquen lo que observan.

output_folder <- "graficos_density"
if (!dir.exists(output_folder)) dir.create(output_folder)

adjust <- c(0.25, 0.5, 1, 1.25, 1.5, 2)

for (i in adjust) {
  plot <- ggplot(datos, aes(x = time)) +
    geom_density(kernel = "gaussian", bw = bw_gr, adjust = i, fill = "blue") +
    scale_y_continuous(limits = c(0, 0.07), breaks = seq(0, 0.07, 0.01)) +
    labs(title = paste("Estimación de densidad (adjust =", i, ")"),
         x = "Tiempo de supervivencia (días)", y = "Densidad estimada") +
    theme_classic()
  
  print(plot)
  
  file_name <- paste0("grafico_density_", i, ".png")
  ggsave(file.path(output_folder, file_name), plot, width = 11, height = 11, dpi = 300)
}
#6
set.seed(123)  
n_boot <- 1000
t_values <- 0:212

# Subgrupos, mujer enferma vs mujer no enferma vs hombre enfermo vs hombre no enfermo

# Agrupo
datos_agrup <- datos %>% group_by(sex, disease_state)

# Subgrupos divididos
subgrupos <- datos_agrup %>% group_split()

# Nombres 
nombres_subgrupos <- datos_agrup %>%
  group_keys() %>%
  mutate(nombre = paste0("supervivencia_", sex, "_", disease_state)) %>%
  pull(nombre)

for (i in seq_along(subgrupos)) {
  sub <- subgrupos[[i]]
  nombre <- nombres_subgrupos[i]
  
  # --- 1. Graficar ECDF ---
  p_ecdf <- ggplot(sub, aes(x = time)) +
    stat_ecdf(color = "blue", pad = FALSE, linewidth = 1) +
    geom_vline(xintercept = 50, linetype = "dashed", color = "red") +
    labs(
      title = paste("ECDF -", nombre),
      x = "Tiempo de supervivencia (días)",
      y = "Probabilidad acumulada"
    ) +
    theme_classic()
  
  ggsave(paste0("ecdf_", nombre, ".png"), plot = p_ecdf, width = 8, height = 4, dpi = 300)
  
  # --- 2. Función de supervivencia + IC bootstrap ---
  
  ecdf_fun <- ecdf(sub$time)
  densidad <- sapply(t_values, function(t) 1 - ecdf_fun(t))
  
  # Bootstrap
  supervivencia_boot <- matrix(NA, nrow = length(t_values), ncol = n_boot)
  
  for (b in 1:n_boot) {
    muestra <- sample(sub$time, replace = TRUE)
    ecdf_boot <- ecdf(muestra)
    supervivencia_boot[, b] <- sapply(t_values, function(t) 1 - ecdf_boot(t))
  }
  
  lower_ic <- apply(supervivencia_boot, 1, quantile, probs = 0.025)
  upper_ic <- apply(supervivencia_boot, 1, quantile, probs = 0.975)
  
  # Graficar y guardar con base R
  png(filename = paste0("supervivencia_", nombre, ".png"), width = 800, height = 400)
  plot(t_values, densidad, type = "l", col = "red", lwd = 2,
       xlab = "Tiempo de supervivencia (días)",
       ylab = expression("Probabilidad de sobrevivir más allá de t: 1 - ECDF(t)"),
       main = paste("Función de Supervivencia con IC -", nombre))
  
  lines(t_values, lower_ic, col = "pink", lty = 2)
  lines(t_values, upper_ic, col = "pink", lty = 2)
  
  mtext("Intervalos de confianza calculados vía bootstrap (R = 1000)")
  dev.off()
}
