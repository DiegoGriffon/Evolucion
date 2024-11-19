

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
RAA <- 3    # Fitness de AA
RAa <- 2.5  # Fitness de Aa
Raa <- 2    # Finess de aa

# Tiempo a simular:
Generaciones <- 100  # Número de generaciones a simular

# Función para calcular el fitness media:
Fitness_Promedio <- function(p, q) {
  p^2 * RAA + 2 * p * q * RAa + q^2 * Raa
}

# Vectores para almacenar las frecuencias a 
# lo largo de las generaciones:
p_valores <- numeric(Generaciones)
q_valores <- numeric(Generaciones)
p_valores[1] <- p
q_valores[1] <- q

# Simulación a lo largo de las generaciones:
for (gen in 2:Generaciones) {
  w_barra <- Fitness_Promedio(p, q)
  f_AA <- p^2 * RAA / w_barra
  f_Aa <- 2 * p * q * RAa / w_barra
  f_aa <- q^2 * Raa / w_barra
  
  p <- f_AA + 0.5 * f_Aa
  q <- f_aa + 0.5 * f_Aa
  
  p_valores[gen] <- p
  q_valores[gen] <- q
}

# Resultados:
Resultados <- data.frame(Generación = 1:Generaciones, 
                         Frecuencia_A = p_valores, 
                         Frecuencia_a = q_valores)

print(Resultados)

# Gráfico de las frecuencias alélicas:
library(ggplot2)

ggplot(Resultados, aes(x = Generación)) +
  geom_line(aes(y = Frecuencia_A, color = "Frecuencia A")) +
  geom_line(aes(y = Frecuencia_a, color = "Frecuencia a")) +
  labs(title = "Evolución de las Frecuencias Alélicas", 
       y = "Frecuencia", 
       color = "Alelo") +
  theme_minimal()

# Ojo: Si se va a correr de nuevo, es importante asegurarse
# de dar a "p" y "q" sus valores iniciales. 

# Ejercicio: conseguir una configuración de Fitnesses que conduzca
# a un polimorfismo estale (equilibrio interior estable)

