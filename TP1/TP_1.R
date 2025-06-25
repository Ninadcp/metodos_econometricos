#-------------------------------------------------------------------------------  
#         Métodos Econométricos y Organización Industrial Aplicada 
#
#                         Ma. Florencia Gabrielli
#                          Diego Fernández Meijide 
#                           Trabajo Práctico N°1
#
#      Luca Bentivenga, Nina Di Costanzo Pereira, María Luján Puchot
#-------------------------------------------------------------------------------  
# Limpiar el entorno
rm(list = ls())

# Importamos librerías
library(tidyverse)
library(dplyr)
library(tidyr)

# Setear el directorio

#setwd("C:\\LUCA\\UdeSA\\Métodos Econométricos y Organización Industrial Aplicada\\TPs\\TP 1")
#setwd("/Users/ninadicostanzopereira/Desktop/Métodos/metodos_econometricos/TP1")
#setwd("C:/Users/luli_/OneDrive/Documentos/Maestría en Economía - UdeSA\Materias/Materias Optativas/Métodos Econométricos y Organización Industrial/Tutoriales/TP 1")

# Abrir la base de datos
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

#2. Estimen y grafiquen la función de supervivencia e interpreten los resultados 
#obtenidos, ¿cómo describirían los resultados del tratamiento sobre la supervivencia? 
#(mayores momentos de mortalidad, tiempo mediano de supervivencia, etc). 1 Bonus: pue
#-den agregar intervalos de confianza.

t_values <- 0:212
densidad <- sapply(t_values, function(t) 1 - ecdf_fun(t))

set.seed(123)
n_boot <- 1000
supervivencia_boot <- replicate(n_boot, {
  muestra <- sample(datos$time, replace = TRUE)
  ecdf_boot <- ecdf(muestra)
  sapply(t_values, function(t) 1 - ecdf_boot(t))
})

# Calculamos los IC al 95% (2.5% y 97.5%) para cada t
lower_ic <- apply(supervivencia_boot, 1, quantile, probs = 0.025)
upper_ic <- apply(supervivencia_boot, 1, quantile, probs = 0.975)

df_surv <- data.frame(
  t = t_values,
  supervivencia = densidad,
  lower_ic = lower_ic,
  upper_ic = upper_ic
)

ggplot(df_surv, aes(x = t, y = supervivencia)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower_ic, ymax = upper_ic), fill = "pink", alpha = 0.2) +
  labs(title = "Función de Supervivencia con Intervalos de Confianza",
       x = "Tiempo de supervivencia (días)",
       y = expression("1 - ECDF(t)")) +
  theme_classic()
  
#3. Estimen la densidad de los tiempos de supervivencia utilizando alguno de los kernels vistos en clase (definan el bandwith 
#con la Regla de Oro de Silverman) y grafiquen. ¿Cuáles son los tiempos de supervivencia más comunes? ¿Qué pueden decir sobre 
#la variabilidad de los tiempos de supervivencia? Comparen la densidad estimada en el tiempo 50 y 10 e interpreten.

#Por simplicidad, usamos el kernel gaussiano de segundo órden, donde el bandwith óptimo según la regla de oro tiene la siguien
#-te expresión: h = 1.06 * sigma_hat * n ^ {-1 / 5}. Luego:

n <- length(datos$time)
sigma_hat <- sd(datos$time)
bw_gr <- 1.06 * sigma_hat * n ^ (-1/5)

densidad <- density(datos$time, kernel = "gaussian", bw = bw_gr, adjust = 1)

# 10 tiempos más comunes
top_10 <- densidad$x[order(densidad$y, decreasing = T)[1:10]]
print(top_10)

# Densidad estimada en time = 50 vs time = 10
dens_10 <- approx(densidad$x, densidad$y, xout = 10)$y
dens_50 <- approx(densidad$x, densidad$y, xout = 50)$y

cat("Densidad estimada en el tiempo 10:", dens_10, "Densidad estimada en el tiempo 50:", dens_50, "\n")

# Graficamos
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

#6.Consideren al menos dos variables para crear subgrupos de pacientes y repitan 
# los puntos 1 y 2

set.seed(123)  
n_boot <- 1000
t_values <- 0:212

# Agrupamos los datos por sexo y estado de enfermedad
datos_agrup <- datos %>% group_by(sex, disease_state)

subgrupos <- datos_agrup %>% group_split()
nombres_subgrupos <- datos_agrup %>%
  group_keys() %>%
  mutate(nombre = paste0("supervivencia_", sex, "_", disease_state)) %>%
  pull(nombre)

# Agregamos columna con nombre de grupo al dataset original (para graficar ECDF juntos)
datos <- datos %>%
  mutate(grupo = paste0(sex, "_", disease_state))

# ---------------------------- gráfico de ECDF ----------------------------
p_ecdf <- ggplot(datos, aes(x = time, color = grupo)) +
  stat_ecdf(linewidth = 1) +
  labs(
    title = "ECDF por subgrupo",
    x = "Tiempo de supervivencia (días)",
    y = "Probabilidad acumulada"
  ) +
  theme_classic()

# Guardamos el gráfico de ECDF
ggsave("ecdf_todos.png", plot = p_ecdf, width = 8, height = 4, dpi = 300)

# ---------------------- Función de supervivencia c/ IC bootstrap ----------------------

resultados <- lapply(seq_along(subgrupos), function(i) {
  
  sub <- subgrupos[[i]]
  grupo <- nombres_subgrupos[i]
  ecdf_fun <- ecdf(sub$time)
  
  # Para cada t calculamos 1 - ECDF(t), o sea la prob de sobrevivir más allá de t
  densidad <- sapply(t_values, function(t) 1 - ecdf_fun(t))  
  
  supervivencia_boot <- replicate(n_boot, {
    muestra <- sample(sub$time, replace = TRUE) 
    ecdf_boot <- ecdf(muestra)  
    sapply(t_values, function(t) 1 - ecdf_boot(t))
  })
  
  # Calculamos los IC al 95% (2.5% y 97.5%) para cada t
  lower_ic <- apply(supervivencia_boot, 1, quantile, probs = 0.025)
  upper_ic <- apply(supervivencia_boot, 1, quantile, probs = 0.975)
  
  data.frame(
    t = t_values,
    supervivencia = densidad,
    lower_ic = lower_ic,
    upper_ic = upper_ic,
    grupo = grupo
  )
})

datos_plot <- bind_rows(resultados)

# Grafico función de supervivencia
p_supervivencia <- ggplot(datos_plot, aes(x = t, y = supervivencia, color = grupo)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower_ic, ymax = upper_ic, fill = grupo), alpha = 0.2, color = NA) +  
  labs(
    title = "Función de Supervivencia por Subgrupo con IC (bootstrap)",
    x = "Tiempo de supervivencia (días)",
    y = "Probabilidad de sobrevivir más allá de t"
  ) +
  theme_classic()

# Guardo el gráfico
ggsave("supervivencia_todos.png", plot = p_supervivencia, width = 8, height = 4, dpi = 300)
