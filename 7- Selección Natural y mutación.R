
##################################################
#         Selección Natural y mutación           #
##################################################


#Supuestos: 
#  Organismos diploides
#  Un locus (2 alelos)
#  Fitnesses constantes
#  Mutación en sentido del alelo deletéreo

# Parámetros del modelo:
mu <- 1e-6  # Tasa de mutación, de "A" hacia "a", irreversible
s <- 0.1    # Coeficiente de selección contra el alelo deletéreo ("a")
R_AA <- 1    # Fitness de AA
R_Aa <- 1    # Fitness de Aa, "A" es completamente dominante sobre "a"
R_aa <- 1-s  # Fitness de aa

# Condición inicial:
q0 <- 0.3  # Frecuencia inicial del alelo deletéreo

# Tiempo a simular:
Generaciones <- 7000  # Número de generaciones

# Función para calcular la frecuencia 
# del alelo deletéreo en cada generación:
Balance_Mutacion_Seleccion <- function(q, mu, R_AA, R_Aa, R_aa) {
  p <- 1 - q
  Delta_q_M <- mu * p
  Delta_q_S <- p*q*(q*R_aa - q*R_Aa + p*R_Aa - p*R_AA ) / (p^2 * R_AA + 2*p*q * R_Aa + q^2 * R_aa)
  Delta_q_Total <- q + Delta_q_M + Delta_q_S
  return(Delta_q_Total)
}

# Simulación:
q <- numeric(Generaciones)
q[1] <- q0 # Se asigna el valor de la frecuencia inicial

for (gen in 2:Generaciones) {
  q[gen] <- Balance_Mutacion_Seleccion(q[gen-1], mu, R_AA, R_Aa, R_aa)
}

# Graficar los resultados:
library(ggplot2)

Resultado <- data.frame(Generacion = 1:Generaciones, 
                        Frecuencia  = q)

ggplot(Resultado, aes(x = Generacion, y = Frecuencia )) +
  geom_line(color = "blue") +
  labs(title = "Equilibrio Mutación-Selección",
       x = "Generación",
       y = "Frecuencia del Alelo Deleterio") +
  theme_minimal()

###### Frecuencia final ######

# Predicción teórica:
round(sqrt(mu/s), 4)

# Frecuencia al final de la simulación:
round(q[Generaciones], 4)

