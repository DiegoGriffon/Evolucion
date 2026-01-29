
##################################################
#          Teoría de juegos evolutivos           #
#         Juego del Halcón y la Paloma           #
#          (Agresivos y Colaboradores)           #
##################################################


#Supuestos: 
#  Dos tipos genéticos
#  Fitnesses dependientes de las frecuencias


# Parámetros del modelo
V <- 6  # Valor del recurso
C <- 2  # Costo de la pelea

# Condiciones iniciales:
Frecuencia_Inicial_Halcones<- 0.1  # Frecuencia inicial de halcones

# Tiempo a simular:
Generaciones <- 10  # Número de generaciones

# Función para calcular el pago esperado (Fitness):
Pagos_Esperados <- function(Frecuencia_Halcones, V, C) {
  Pago_Halcones <- Frecuencia_Halcones * (V - C) / 2 + (1 - Frecuencia_Halcones) * V
  Pago_Palomas <- Frecuencia_Halcones * 0 + (1 - Frecuencia_Halcones) * V / 2
  return(c(Pago_Halcones, Pago_Palomas))
}

# Simulación de la dinámica de la población:
Simulacion_Juego <- function(Frecuencia_Inicial_Halcones, V, C, Generaciones) {
  Frecuencia_Halcones <- numeric(Generaciones)
  Frecuencia_Halcones[1] <- Frecuencia_Inicial_Halcones
  
  for (gen in 2:Generaciones) {
    Pagos <- Pagos_Esperados(Frecuencia_Halcones[gen - 1], V, C)
    Pago_Promedio <- Frecuencia_Halcones[gen - 1] * Pagos[1] + (1 - Frecuencia_Halcones[gen - 1]) * Pagos[2]
    Frecuencia_Halcones[gen] <- Frecuencia_Halcones[gen - 1] * Pagos[1] / Pago_Promedio
  }
  
  return(Frecuencia_Halcones)
}

# Ejecutar la simulación:
Frecuencia_Halcones <- Simulacion_Juego(Frecuencia_Inicial_Halcones, V, C, Generaciones)

# Crear un data frame para graficar:
Datos <- data.frame(Generacion = 1:Generaciones, Frecuencia_Halcones = Frecuencia_Halcones)

# Graficar los resultados:

library(ggplot2)

ggplot(Datos, aes(x = Generacion, y = Frecuencia_Halcones)) +
  geom_line(color = "blue") +
  labs(title = "Dinámica del Juego del \n   Halcón y la Paloma",
       x = "Generación",
       y = "Frecuencia de Halcones") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1))

# John Maynard Smith. 1982. Evolution and the Theory 
# of Games. Cambridge University Press.

# Deducción de la ecuación dinámica para 
# haploides en tiempo discreto:

# https://n9.cl/ds6qa


