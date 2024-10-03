
##################################################
#         Selección Natural y mutación           #
##################################################


#Supuestos: 
#  Organismos diploides
#  Un locus (2 alelos)
#  Fitnesses constantes
#  Mutación en sentido del alelo deletéreo

# Parámetros del modelo:
mu <- 1e-6  # Tasa de mutación
s <- 0.1  # Coeficiente de selección contra el alelo deletéreo
wAA <- 1
wAa <- 1
waa <- 1-s

# Condición inicial:
q0 <- 0.3  # Frecuencia inicial del alelo deletéreo

# Tiempo a simular:
Generaciones <- 7000  # Número de generaciones

# Función para calcular la frecuencia 
# del alelo deletéreo en cada generación:
Balance_Mutacion_Seleccion <- function(q, mu, wAA, wAa, waa) {
  p <- 1 - q
  q_mutacion <- mu * p
  q_seleccion <- p*q*(q*waa - q*wAa + p*wAA - p*wAa ) / (p^2 * wAA + 2*p*q * wAa + q^2 * waa)
  q_luego <- q + q_mutacion + q_seleccion
  return(q_luego)
}

# Simulación:
q <- numeric(Generaciones)
q[1] <- q0 # Se asigna el valor de la frecuencia incial

for (gen in 2:Generaciones) {
  q[gen] <- Balance_Mutacion_Seleccion(q[gen-1], mu, wAA, wAa, waa)
}

# Graficar los resultados:
library(ggplot2)

data <- data.frame(Generacion = 1:Generaciones, frequency = q)

ggplot(data, aes(x = Generacion, y = frequency)) +
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

