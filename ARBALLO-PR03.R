install.packages("ggplot2")

library(ggplot2)

separador = function (ejercicio) {
  cat("\n\n=------------------------- Ejercicio Nº",ejercicio," -------------------------->\n\n")
}

separador(1)
puntuaciones = c(13, 13, 14, 15, 16, 18, 19, 20, 22, 22)
frecuencias = c(0, 3, 1, 5, 4, 3, 1, 2, 1, 0)
puntuacionesDataFrame = data.frame(puntuaciones = puntuaciones, frecuencias = frecuencias)
ggplot(puntuacionesDataFrame, aes(x = puntuaciones, y = frecuencias)) +
  geom_point(na.rm = FALSE, stat = "identity") +
  geom_line(na.rm = FALSE, stat = "identity") +
  xlab("Puntiaciones") +
  ylab("Frecuencias") + 
  ggtitle("Calificaciones de un grupo de alumnos") +
  scale_x_continuous(limits = c(10, 24), breaks = seq(10, 24, by = 1)) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 1))

separador(2)


rm(list = ls())

