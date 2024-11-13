##################################################
#           Ecuación del mejorador               #
#  Avance por selección en varias generaciones   #              #
#         (Heredabilidad Decreciente)              #
##################################################

# Parámetros del modelo:
S <- 10  # Diferencial de selección
D <- 0.9  # Descuento de la heredabilidad.
# La heredabilidad va a disminuir un 1 - D en cada generación.

# Condiciones iniciales:
h2_inicial <- 0.5  # Heredabilidad inicial 
Media_Inicial <- 100  # Media inicial del rasgo

# Tiempo a simular:
Generaciones <- 55  # Número de generaciones

# Vector para almacenar la media del rasgo 
# en cada generación:
Media_del_fenotipo <- numeric(Generaciones)
Media_del_fenotipo[1] <- Media_Inicial

# Vector para almacenar la heredabilidad 
# en cada generación:
heredabilidad <- numeric(Generaciones)
heredabilidad[1] <- h2_inicial

# Calcular la respuesta a la selección 
# en cada generación:
for (gen in 2:Generaciones) {
  # Disminuir la heredabilidad en cada generación
  heredabilidad[gen] <- heredabilidad[gen - 1] * D  
  # La Heredabilidad disminuye un 1 - D en cada generación
  
  # Calcular la respuesta a la selección
  R <- heredabilidad[gen] * S
  
  # Actualizar la media del fenotipo
  Media_del_fenotipo[gen] <- Media_del_fenotipo[gen - 1] + R
}

# Crear un data frame para graficar:
data <- data.frame(Generacion = 1:Generaciones, 
                   Media_caracteristica = Media_del_fenotipo,
                   Heredabilidad = heredabilidad)

# Graficar los resultados:
library(ggplot2)
ggplot(data, aes(x = Generacion)) +
  geom_line(aes(y = Media_caracteristica, color = "Media del Rasgo")) +
  geom_point(aes(y = Media_caracteristica, color = "Media del Rasgo")) +
  geom_line(aes(y = Heredabilidad * 100, color = "Heredabilidad (%)")) +
  geom_point(aes(y = Heredabilidad * 100, color = "Heredabilidad (%)")) +
  scale_y_continuous(
    name = "Media del Rasgo",
    sec.axis = sec_axis(~./100, name = "Heredabilidad (%)")
  ) +
  labs(title = "         Avance por Selección \n con Disminución de Heredabilidad",
       x = "Generación") +
  scale_color_manual(values = c("Media del Rasgo" = "blue", "Heredabilidad (%)" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())



##################################################
# Aumento en la media del fenotipo 
# en el ciclo total de selección:
Media_del_fenotipo[Generaciones] - Media_Inicial

# Ejercicio: Probar diferentes valores 
# de descuento (D).


