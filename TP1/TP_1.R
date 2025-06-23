library(tidyverse)

rm(list = ls())

#setwd("C:\\LUCA\\UdeSA\\Métodos Econométricos y Organización Industrial Aplicada\\TPs\\TP 1")
#setwd("nina")
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
