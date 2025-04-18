
###########################################################
# Simulación de un juego evolutivo tipo Piedra-Papel-Tijera 
###########################################################

# Condiciones iniciales: 

# Frecuencias iniciales definidas por usuario:
Frecuencias <- c(0.5,0.3,0.2) # Piedra, Papel, Tijera

# Frecuencias iniciales  aleatorias:
# set.seed(123)  # Para reproducibilidad
# Frecuencias <- runif(3) # se asigna de forma aleatoria 
# las Frecuencias iniciales de las 3 estrategias

Frecuencias <- Frecuencias / sum(Frecuencias)  # Normalizar para que las
# Frecuencias sumen 1

# Parámetros de la simulación:
Generaciones <- 1000  # Número de pasos temporales
dt <- 0.01            # Tamaño del paso temporal

# Matriz de pagos:
Matriz_Pagos <- matrix(c( 0, -1,  1,    # Piedra vs Piedra, Papel, Tijera
                          1,  0, -1,   # Papel vs Piedra, Papel, Tijera
                         -1,  1,  0),  # Tijera vs Piedra, Papel, Tijera
                       nrow = 3, byrow = TRUE)
rownames(Matriz_Pagos) <- c("Piedra", "Papel", "Tijera")
colnames(Matriz_Pagos) <- c("Piedra", "Papel", "Tijera")

Matriz_Pagos

# Función para calcular el fitness de cada estrategia:
Calcular_Fitness <- function(Frecuencias, Matriz_Pagos) {
  Fitness <- Matriz_Pagos %*% Frecuencias  # Multiplicación matricial (fitness esperado)
  return(Fitness)
}

# Ecuaciones de los Organismos (ecuaciones dinámicas):
Organismos <- function(Frecuencias, Fitness) {
  Fitness_Promedio <- sum(Frecuencias * Fitness)  # Aptitud promedio de la población
  d_Frecuencias <- Frecuencias * (Fitness - Fitness_Promedio)  # Cambio en Frecuenciass
  return(d_Frecuencias)
}

# Matriz para almacenar las Frecuencias a lo largo del tiempo:
Resultado <- matrix(NA, nrow = Generaciones, ncol = 3)
Resultado[1, ] <- Frecuencias

# Simulación:
for (t in 2:Generaciones) {
  Fitness <- Calcular_Fitness(Resultado[t-1, ], Matriz_Pagos)
  d_Frecuencias <- Organismos(Resultado[t-1, ], Fitness)
  Resultado[t, ] <- Resultado[t-1, ] + dt * d_Frecuencias
  
  # Asegurar que las Frecuencias permanezcan entre 0 y 1 y sumen 1
  Resultado[t, ] <- pmax(Resultado[t, ], 0)  # Evitar negativos
  Resultado[t, ] <- Resultado[t, ] / sum(Resultado[t, ])  # Renormalizar
}

# Graficar los resultados:
plot(1:Generaciones, Resultado[, 1], 
     type = "l", 
     col = "blue", 
     ylim = c(0, 1),
     xlab = "Generaciones", ylab = "Frecuencias de las estrategias", 
     main = "Dinámica evolutiva tipo Piedra-Papel-Tijera")
lines(1:Generaciones, Resultado[, 2], col = "red")
lines(1:Generaciones, Resultado[, 3], col = "green")
legend("topright", legend = c("Piedra", "Papel", "Tijera"), 
       col = c("blue", "red", "green"), lty = 1)

# Inspirado en: Alonzo, Suzanne H.; Sinervo, Barry. 2001. 
# Mate choice games, context-dependent good genes, and 
# genetic cycles in the side-blotched lizard, Uta stansburiana. 
# Behavioral Ecology and Sociobiology. 49 (2–3): 176–186.

