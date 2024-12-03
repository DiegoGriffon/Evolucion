

##################################################
#              Deriva genética                   #
##################################################


#Supuestos: 
#  Un locus (2 alelos)
#  No existen diferencias en términos de 
#  Fitness entre los alelos A y a

# Función para simular un paso generacional 
# en la población:

#Comando para hacer el muestreo intergeneracional
?sample

Deriva_Una_Generacion <- function(p, N) {
  # p: frecuencia del alelo A en la generación actual
  # N: tamaño de la población
  
  # Muestreo aleatorio de 2N alelos: 
  Alelos <- sample(c("A", "a"), 2*N, replace = TRUE, prob = c(p, 1-p))
  
  # Contar el número de alelos A en la 
  # siguiente generación:
  A_siguiente_Generacion <- sum(Alelos == "A")
  
  # Calcular la nueva frecuencia de A:
  p_nuevo <- A_siguiente_Generacion / (2*N)
  
  return(p_nuevo)
}

# Condiciones iniciales de la simulación:
N <- 10  # Tamaño efectivo de la población
p0 <- 0.5 # Frecuencia inicial del alelo A

# Tiempo a simular:
Generaciones <- 100

# Vector para almacenar las frecuencias de A 
# en cada generación:
p <- numeric(Generaciones + 1)
p[1] <- p0

# Simulación:
for (t in 2:(Generaciones+1)) {
  p[t] <- Deriva_Una_Generacion(p[t-1], N)
}

# Visualizar los resultados:
plot(0:Generaciones, p, 
     type = "l", 
     xlab = "Generación", 
     ylab = "Frecuencia del alelo A",
     main = "Simulación de Deriva Genética")

# Ejercicio: correr la simulación para diferentes valores del
# tamaño de la población (N).

