

##################################################
#           Ecuación del mejorador               #
#  Avance por selección en varias generaciones   #              #
#         (Heredabilidad Constante)              #
##################################################


# Parámetros del modelo:
h2 <- 0.5  # Heredabilidad en sentido estricto
S <- 10  # Diferencial de selección

# Condiciones iniciales:
Media_Inicial <- 100  # Media inicial del rasgo

# Tiempo a simular:
Generaciones <- 20  # Número de generaciones

# Vector para almacenar la media del rasgo 
# en cada generación:
Media_del_fenotipo <- numeric(Generaciones)
Media_del_fenotipo[1] <- Media_Inicial

# Calcular la respuesta a la selección en 
# cada generación:
for (gen in 2:Generaciones) {
  R <- h2 * S
  Media_del_fenotipo[gen] <- Media_del_fenotipo[gen - 1] + R
}

# Crear un data frame para graficar:
data <- data.frame(Generacion = 1:Generaciones, 
                   Media_caracteristica = Media_del_fenotipo)

# Graficar los resultados:
library(ggplot2)
ggplot(data, aes(x = Generacion, y = Media_caracteristica)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = " Avance por Selección \n (Varias Generaciones)",
       x = "Generación",
       y = "Media del Rasgo") +
  theme_minimal()

##############################################

# Aumento en la media del fenotipo
# en el ciclo total de selección:
Media_del_fenotipo[Generaciones] - Media_Inicial
