

##################################################
#    Selección Natural en haploides asexuales    #
#               Tiempo Continuo                  #
##################################################


# Supuestos:
# Organismos Asexuales
# Haploides
# Uníparos (Semelparos)
# Dos tipos genéticos
# Tiempo Continuo


library(deSolve) # Librería para trabajar 
# con ecuaciones diferenciales ordinarias
?deSolve

# Función que define el modelo:
Modelo <- function(Tiempo, FrecInci, Parametros) {
  with(as.list(c(FrecInci,Parametros)), {
    dp <- p *(r1 - (r1 * p + r2 * (1 - p)))
    list(c(dp))
  })
}

# Condiciones iniciales
p0 <- 0.01  # Frecuencia inicial del alelo 1

# Parámetros:
r1 <- 1.1
r2 <- 1

# Tiempo a simular:
Tiempo_Final <- 200
Tiempo <- seq(0, Tiempo_Final, by = 0.01)

# Condiciones iniciales:
FrecInci <- c(p = p0)
Parametros <- c(r1 = r1, r2 = r2)

# Resolver la ecuación diferencial (modelo):
Resultado <- ode(y = FrecInci, 
           times = Tiempo, 
           func = Modelo, 
           parms = Parametros)
# Se utiliza el comando “ode” de la 
# librería “deSolve para avaluar el Modelo
?ode

# Graficar los resultados:
plot(Resultado, xlab = "Tiempo", 
     ylab = "Frecuencia de los alelos",
     ylim = c(0, 1), 
     main = "Simulación de Selección Natural")
# Se grafica la frecuencia del alelo 1 (p)

Resultado[1:10,] 

# Ojo: 

# La primera columna de “Resultado” contiene el 
# valor del “time”.
# Esto es: Resultado[,1]

# La segunda columna de “Resultado” contiene 
# el valor de “p”.
# Esto es: Resultado[,2]
# Por lo que: 1-Resultado[,2] es igual al 
# valor de “q”

lines(Resultado[,1], 1-Resultado[,2], col = "blue")
# Se grafica la frecuencia del alelo 2 (q)


legend("right", legend = c("Alelo 1 (p)", "Alelo 2 (q)"), 
       col = c("black", "blue"), lty = 1)
# Se agrega una leyenda

# Ejercicio: cambiar los valores de los 
# parámetros



