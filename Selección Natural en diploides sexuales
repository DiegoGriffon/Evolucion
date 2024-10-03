

##################################################
#    Selección Natural en diploides sexuales     #
##################################################


# Supuestos: 
#  Organismos diploides
#  Sexuales
#  Un locus (2 alelos)
#  Fitnesses constantes


# Condiciones iniciales:
p <- 0.1  # Frecuencia inicial del alelo A
q <- 0.9  # Frecuencia inicial del alelo a

# Parámetros:
R11 <- 3    # Fitness de AA
R12 <- 2.5  # Fitness de Aa
R22 <- 2    # Finess de aa

# Tiempo a simular:
Generaciones <- 100  # Número de generaciones a simular

# Función para calcular el fitness media:
Fitness_Promedio <- function(p, q, s, t) {
  p^2 * R11 + 2 * p * q * R12 + q^2 * R22
}

# Vectores para almacenar las frecuencias a 
# lo largo de las generaciones:
p_valores <- numeric(Generaciones)
q_valores <- numeric(Generaciones)
p_valores[1] <- p
q_valores[1] <- q


# Simulación a lo largo de las generaciones:
for (gen in 2:Generaciones) {
  w_bar <- Fitness_Promedio(p, q, s, t)
  f_AA <- p^2*R11 / w_bar
  f_Aa <- 2 * p * q * R12 / w_bar
  f_aa <- q^2 * R22 / w_bar
  
  p <- f_AA + 0.5 * f_Aa
  q <- f_aa + 0.5 * f_Aa
  
  p_valores[gen] <- p
  q_valores[gen] <- q
}

# Resultados:
data <- data.frame(Generación = 1:Generaciones, Frecuencia_A = p_valores, Frecuencia_a = q_valores)

print(data)

# Gráfico de las frecuencias alélicas:
library(ggplot2)

ggplot(data, aes(x = Generación)) +
  geom_line(aes(y = Frecuencia_A, color = "Frecuencia A")) +
  geom_line(aes(y = Frecuencia_a, color = "Frecuencia a")) +
  labs(title = "Evolución de las Frecuencias Alélicas", y = "Frecuencia", color = "Alelo") +
  theme_minimal()

# Ojo: Si se va a correr de nuevo, es importante asegurarse
# de dar a "p" y "q" sus valores iniciales. 

# Ejercicio: conseguir una configuración de Fitnesses que conduzca
# a un polimorfismo estale (equilibrio interior estable)
