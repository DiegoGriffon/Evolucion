

##################################################
#//////// Simulación de Selección Natural ////////
##################################################


# Caso (supuestos):
# Organismos Asexuales
# Haploides
# Uníparos (Semelparos)
# Dos tipos genéticos
# Tiempo Discreto

# Condiciones iniciales:
p0 <- 0.01  # Frecuencia inicial del alelo 1
q0 <- 1 - p0

# Valores de los parámetros:
R1 <- 3  # Aptitud del alelo 1
R2 <- 2.5

# Generaciones a simular:
Num_Generaciones <- 100

# Vectores para almacenar las frecuencias:
p <- numeric(Num_Generaciones)
q <- numeric(Num_Generaciones)
p[1] <- p0 # Se asigna en la primera entrada 
# del vector, el valor de la frecuencia inicial 
# del Alelo 1
q[1] <- q0

# Bucle para simular las generaciones:
for (t in 2:Num_Generaciones) {
  p[t] <- (R1 * p[t-1]) / (R1 * p[t-1] + R2 * q[t-1]) 
  # Modelo para la dinámica de la frecuencia del Alelo 1
  q[t] <- 1 - p[t]
}

# Graficar los resultados:
plot(1:Num_Generaciones, p, type = "l", 
     xlab = "Generación", 
     ylab = "Frecuencia de los alelos",
     ylim = c(0, 1), main = "Simulación de Selección Natural")
# Se grafica la frecuencia del alelo 1 (p)

lines(1:Num_Generaciones, q, col = "blue")
# Se grafica la frecuencia del alelo 2 (q)

legend("right", legend = c("Alelo 1 (p)", "Alelo 2 (q)"), 
       col = c("black", "blue"), lty = 1)
# Se agrega una leyenda

# Ejercicio: cambiar los valores de los 
# parámetros



