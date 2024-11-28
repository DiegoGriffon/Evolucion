
##################################################
#    Selección Natural en diploides sexuales     #
#   Fitnesses dependientes de las frecuencias    #
##################################################


#Supuestos: 
#  Organismos diploides
#  Sexuales
#  Un locus (2 alelos)
#  Fitnesses dependientes de las frecuencias

# Condiciones iniciales:
p <- 0.6  # Frecuencia inicial del alelo A
q <- 0.4  # Frecuencia inicial del alelo a

# Parámetros:
sAA <- 1  # Coeficiente de selección dependiente de la frecuencia genotipo AA
sAa <- 1  # Coeficiente de selección dependiente de la frecuencia genotipo Aa
saa <- 1  # Coeficiente de selección dependiente de la frecuencia genotipo aa

# Tiempo a simular:
Generaciones <- 50  # Número de generaciones a simular

# Función para calcular el fitness medio:
Fitness_Promedio <- function(p, q) {
  f_AA <- p^2
  f_Aa <- 2 * p * q
  f_aa <- q^2
  w_AA <- 1 - sAA * f_AA
  w_Aa <- 1 - sAa * f_Aa
  w_aa <- 1 - saa * f_aa
  f_AA * w_AA + f_Aa * w_Aa + f_aa * w_aa
}

# Vectores para almacenar las frecuencias 
# a lo largo de las generaciones:
p_valores <- numeric(Generaciones)
q_valores <- numeric(Generaciones)
p_valores[1] <- p
q_valores[1] <- q

# Simulación a lo largo de las generaciones:
for (gen in 2:Generaciones) {
  f_AA <- p^2
  f_Aa <- 2 * p * q
  f_aa <- q^2
  w_AA <- 1 - sAA * f_AA
  w_Aa <- 1 - sAa * f_Aa
  w_aa <- 1 - saa * f_aa
  w_bar <- Fitness_Promedio(p, q)
  
  f_AA <- f_AA * w_AA / w_bar
  f_Aa <- f_Aa * w_Aa / w_bar
  f_aa <- f_aa * w_aa / w_bar
  
  p <- f_AA + 0.5 * f_Aa
  q <- f_aa + 0.5 * f_Aa
  
  p_valores[gen] <- p
  q_valores[gen] <- q
}

# Resultados:

data <- data.frame(Generación = 1:Generaciones, 
                   Frecuencia_A = p_valores, 
                   Frecuencia_a = q_valores)

print(data)

# Gráfico de las frecuencias alélicas:

library(ggplot2)

ggplot(data, aes(x = Generación)) +
  geom_line(aes(y = Frecuencia_A, color = "Frecuencia A")) +
  geom_line(aes(y = Frecuencia_a, color = "Frecuencia a")) +
  labs(title = "      Evolución de las Frecuencias Alélicas \n con Selección Dependiente de la Frecuencia", y = "Frecuencia", color = "Alelo") +
  theme_minimal()

# Ojo: Si se va a correr de nuevo, es importante asegurarse
# de dar a "p" y "q" sus valores iniciales. 

