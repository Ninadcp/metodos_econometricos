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
#setwd("C:/Users/luli_/OneDrive/Documentos/Maestría en Economía - UdeSA/Materias/Materias Optativas/Métodos Econométricos y Organización Industrial/Tutoriales/TP 1")

# Abrir la base de datos
datos <- read.csv("Base de datos TP 1.csv")

#1. Estimen y grafiquen la función de distribución acumulada para toda la muestra (ecdf), sin distinguir entre diferentes 
#grupos de personas. Interpreten el valor de la ECDF cuando el tiempo de supervivencia es 50 unidades de tiempo.

ECDE_plot <- ggplot(datos, aes(x = time)) +
  stat_ecdf(color = "blue", pad = FALSE, linewidth = 1) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "red") +
  labs(
    title = "Función de Distribución Acumulada Empírica (ECDF)",
    x = "Tiempo de supervivencia (días)",
    y = "Probabilidad acumulada"
  ) +
  scale_x_continuous(breaks = seq(0, max(datos$time), by = 25)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.margin = margin(t = 25, r = 15, b = 15, l = 15)
  )

ggsave("ECDE1.png", plot = ECDE_plot, width = 7, height = 5)

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


# Sin intervalos de confianza


df_surv_solo <- data.frame(
  t = t_values,
  supervivencia = densidad
)

ECDE_plot2 <- ggplot(df_surv_solo, aes(x = t, y = supervivencia)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(
    title = "Función de Supervivencia",
    x = "Tiempo de supervivencia (días)",
    y = expression("1 - ECDF(t)")
  ) +
  scale_x_continuous(breaks = seq(0, max(df_surv_solo$t), by = 25)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.margin = margin(t = 25, r = 15, b = 15, l = 15)
  )

ggsave("ECDE2.png", plot = ECDE_plot2, width = 7, height = 5)


  
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

f_dens <- ggplot(datos, aes(x = time)) +
  geom_density(kernel = "gaussian", bw = bw_gr, adjust = 1, fill = "blue", alpha = 0.7) +
  geom_vline(xintercept = c(10, 50), linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Estimación de densidad de los tiempos de supervivencia",
    x = "Tiempo de supervivencia (días)",
    y = "Densidad estimada"
  ) +
  scale_x_continuous(
    breaks = seq(0, 175, by = 25),
    limits = c(0, 175)
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 15)),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 12),
    plot.margin = margin(t = 30, r = 15, b = 15, l = 15)
  )

ggsave("f_dens.png", plot = f_dens, width = 7, height = 5)

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

'ggplot(df_total, aes(x = time, color = n)) +
  stat_ecdf(linewidth = 1, pad = F) +
  labs(title = "Funciones ECDF para diferentes tamaños de muestra",
       x = "Tiempo de supervivencia (días)",
       y = "Probabilidad acumulada",
       color = "Tamaño de muestra (n)") +
  theme_classic()' # lo dejo como comentario (gráfico anterior)


p <- ggplot(df_total, aes(x = time, color = as.factor(n))) +
  stat_ecdf(linewidth = 0.7, pad = FALSE) +
  labs(
    title = "ECDF para diferentes tamaños de muestra",
    x = "Tiempo de supervivencia (días)",
    y = "Probabilidad acumulada",
    color = "Tamaño de muestra (n)"
  ) +
  scale_x_continuous(
    breaks = seq(0, 175, by = 25),
    limits = c(0, 175)
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.95),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "right",
    legend.key.height = unit(0.8, "lines"),
    legend.key.width = unit(0.8, "lines"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    aspect.ratio = 0.7
  ) +
  scale_color_viridis_d(option = "G", end = 0.85)

print(p)

ggsave("ecdf_grande.png", p, width = 10, height = 7, dpi = 300)

#5. Generen un grupo de gráficos que muestre cómo cambia la estimación de densidad de los tiempos de supervivencia para dife
#-rentes valores del bandwith (utilice un kernel Gaussiano). Expliquen lo que observan.

output_folder <- "graficos_density"
if (!dir.exists(output_folder)) dir.create(output_folder)

adjust <- c(0.25, 0.5, 1, 1.25, 1.5, 2)

'for (i in adjust) {
  plot <- ggplot(datos, aes(x = time)) +
    geom_density(kernel = "gaussian", bw = bw_gr, adjust = i, fill = "blue") +
    scale_y_continuous(limits = c(0, 0.07), breaks = seq(0, 0.07, 0.01)) +
    labs(title = paste("Estimación de densidad (adjust =", i, ")"),
         x = "Tiempo de supervivencia (días)", y = "Densidad estimada") +
    theme_classic()
  
  print(plot)
  
  file_name <- paste0("grafico_density_", i, ".png")
  ggsave(file.path(output_folder, file_name), plot, width = 11, height = 11, dpi = 300)
}'


output_folder <- "graficos_density_mlp"
if (!dir.exists(output_folder)) dir.create(output_folder)

adjust <- c(0.25, 0.5, 1, 1.25, 1.5, 3)

for (i in adjust) {
  plot <- ggplot(datos, aes(x = time)) +
    geom_density(kernel = "gaussian", bw = bw_gr, adjust = i, fill = "blue") +
    scale_x_continuous(
      limits = c(0, 175),
      breaks = seq(0, 175, by = 25)
    ) +
    scale_y_continuous(
      limits = c(0, 0.07),
      breaks = seq(0, 0.07, 0.01)
    ) +
    labs(
      title = paste("Estimación de densidad (adjust =", i, ")"),
      x = "Tiempo de supervivencia (días)",
      y = "Densidad estimada"
    ) +
    theme_classic(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 24, margin = margin(b = 12)),
      axis.title.x = element_text(size = 18, face = "plain", margin = margin(t = 10)),
      axis.title.y = element_text(size = 18, face = "plain", margin = margin(r = 10)))
  
  print(plot)
  
  file_name <- paste0("grafico_density_mlp", i, ".png")
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
  mutate(Grupo = paste0(sex, "_", disease_state))

# ---------------------------- gráfico de ECDF ----------------------------

  
datos <- datos %>%
  mutate(Grupo = paste0(sex, "_", disease_state))

p_ecdf <- ggplot(datos, aes(x = time, color = Grupo)) +
  stat_ecdf(linewidth = 0.8, pad = FALSE) +
  labs(
    title = "ECDF por subgrupo",
    x = "Tiempo de supervivencia (días)",
    y = "Probabilidad acumulada"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

ggsave("ecdf_todos.png", plot = p_ecdf, width = 8, height = 4, dpi = 300)



# ---------------------- Función de supervivencia c/ IC bootstrap ----------------------

resultados <- lapply(seq_along(subgrupos), function(i) {
  
  sub <- subgrupos[[i]]
  Grupo <- nombres_subgrupos[i]
  ecdf_fun <- ecdf(sub$time)  # Calculo la ECDF 
  
  # Para cada t calculo 1 - ECDF(t), o sea la prob de sobrevivir más allá de t
  densidad <- sapply(t_values, function(t) 1 - ecdf_fun(t))  
  
  # Ahora hago bootstrap
  supervivencia_boot <- replicate(n_boot, {
    muestra <- sample(sub$time, replace = TRUE) 
    ecdf_boot <- ecdf(muestra)  
    sapply(t_values, function(t) 1 - ecdf_boot(t))
  })
  
  # Calculo los IC al 95% (2.5% y 97.5%) para cada t
  lower_ic <- apply(supervivencia_boot, 1, quantile, probs = 0.025)
  upper_ic <- apply(supervivencia_boot, 1, quantile, probs = 0.975)
  
  # Armo un data.frame 
  data.frame(
    t = t_values,
    supervivencia = densidad,
    lower_ic = lower_ic,
    upper_ic = upper_ic,
    Grupo = Grupo
  )
})

# Uno todos los dataframes en uno solo
datos_plot <- bind_rows(resultados)

p_supervivencia <- ggplot(datos_plot, aes(x = t, y = supervivencia, color = Grupo)) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(aes(ymin = lower_ic, ymax = upper_ic, fill = Grupo), alpha = 0.2, color = NA) +  
  labs(
    title = "Función de Supervivencia por Subgrupo con IC (bootstrap)",
    x = "Tiempo de supervivencia (días)",
    y = "Probabilidad de sobrevivir más allá de t"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # título centrado y en negrita
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1) 
  )

ggsave("supervivencia_todos.png", plot = p_supervivencia, width = 8, height = 4, dpi = 300)
