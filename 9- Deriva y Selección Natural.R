
##################################################
#          Selección Natural y Deriva            #
##################################################

# Valores de los parámetros
Fitness_AA = 1
Fitness_Aa = 0.95
Fitness_aa = 0.9
Tamaño_Efectivo = 100
Repeticiones = 10

# Condiciones iniciales
p_inicial = 0.5

# Duracion de la simulación
Duracion = 120

# Definición de la función Deriva_SN
Deriva_SN <- function(w11, w12, w22, po, N, Generaciones) {
  p <- c(po)
  for (t in 1:Generaciones) {
    A <- 0   # Simula efecto de la deriva
    for (i in 1:(2 * N)) {
      if (runif(1) < p[t]) {
        A <- A + 1
      }
    } # Termina efecto de la deriva
    pd <- A / (2 * N) # Simula efecto de la Selección, sobre el resultado de la Deriva
    w_barra <- pd^2 * w11 + 2 * pd * (1 - pd) * w12 + (1 - pd)^2 * w22
    pprima <- (pd^2 * w11 +  pd * (1 - pd) * w12 ) / w_barra # Termina efecto de la Selección
    p <- c(p, pprima)
  }
  return(p)
}

# Configuración de la semilla aleatoria
set.seed(73)

# Ejecución de la función Deriva_SN para el número de repeticiones estipulado
Resultados <- matrix(nrow = Duracion+1, ncol = Repeticiones)
for (i in 1:Repeticiones) {
  Resultados[, i] <- Deriva_SN(Fitness_AA, Fitness_Aa, Fitness_aa, p_inicial, Tamaño_Efectivo, Duracion)
}

# Gráfico de los resultados
plot(0:Duracion, Resultados[, 1], 
     type = "l", 
     col = "blue", 
     ylim = c(0, 1),
     xlab = "Generaciones", 
     ylab = "Frecuencia del alelo A", 
     main = "Modelo de Deriva Genética \n y Selección Natural")
for (i in 2:Repeticiones) {
  lines(0:Duracion, Resultados[, i], col = "blue")
}

# Tomado y modificado de: Rougharden, J.1998. Primer of ecological theory. Prentice Hall


