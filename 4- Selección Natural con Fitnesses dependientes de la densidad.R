

##################################################
#    Selección Natural en diploides sexuales     #
#   Fitnesses dependientes de la densidad        #
##################################################


#Supuestos: 
#  Organismos diploides
#  Un locus (2 alelos)
#  Fitnesses dependientes de la densidad
#  Densodependencia negativa


# Parámetros del modelo:
K_AA <- 10000  # Capacidad de carga para el genotipo AA
K_Aa <- 9000  # Capacidad de carga para el genotipo Aa
K_aa <- 8000 # Capacidad de carga para el genotipo aa

R_AA <- 0.2   # Tasa de crecimiento para el genotipo AA
R_Aa <- 0.25  # Tasa de crecimiento para el genotipo Aa
R_aa <- 0.3   # Tasa de crecimiento para el genotipo aa

# Tamaño inicial de la población:
N_AA <- 1000  # Individuos iniciales del genotipo AA
N_Aa <- 1000  # Individuos iniciales del genotipo Aa
N_aa <- 1000  # Individuos iniciales del genotipo aa

# Número de generaciones:
Generaciones <- 300

# Vecto para almacenar resultados:
Resultados <- data.frame(generacion = 1:Generaciones,N_total = NA, N_AA = NA, N_Aa = NA, N_aa = NA)

# Simulación:
for (t in 1:Generaciones) {
  
  # Calcular el tamaño total de la población:
  N_total <- N_AA + N_Aa + N_aa
  
  # Calcular las tasas de crecimiento ajustadas:
  Lambda_AA <- 1 + R_AA * (1 - N_total / K_AA)
  Lambda_Aa <- 1 + R_Aa * (1 - N_total / K_Aa)
  Lambda_aa <- 1 + R_aa * (1 - N_total / K_Aa)
  
  # Actualizar el tamaño de la población para cada genotipo:
  N_AA <-  Lambda_AA * N_AA
  N_Aa <-  Lambda_Aa * N_Aa
  N_aa <-  Lambda_aa * N_aa
  
  #N_AA <- N_AA + Lambda_AA * N_AA
  #N_Aa <- N_Aa + Lambda_Aa * N_Aa
  #N_aa <- N_aa + Lambda_aa * N_aa
  
  # Almacenar los resultados:
  Resultados[t, ] <- c(t, N_total, N_AA, N_Aa, N_aa)
}

# Graficar resultados

library(reshape2)
# Convertir el objeto resultados en un data frame
?melt
resultados_long <- melt(Resultados, id.vars = "generacion")

library(ggplot2)

ggplot(resultados_long, aes(x = generacion, y = value, color = variable)) +
  geom_line() +
  labs(title = "Simulación de Selección Natural \n   Dependiente de la Densidad",
       x = "Generación",
       y = "Número de Individuos") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Ojo: Si se va a correr de nuevo, es importante asegurarse
# de actualizar los tamaños iniciales de las poblaciones.

