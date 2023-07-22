install.packages("ggplot2")

library(ggplot2)

separador = function (ejercicio) {
  cat("\n\n=------------------------- Ejercicio Nº",ejercicio," -------------------------->\n\n")
}

separador(1)
puntuaciones = c(0,13, 14, 15, 16, 18, 19, 20, 22,0)
frecuencias = c(0,3, 1, 5, 4, 3, 1, 2, 1,0)
puntuacionesDataFrame = data.frame(puntuaciones = puntuaciones, frecuencias = frecuencias)

puntuacionesDataFrame_extended <- rbind(puntuacionesDataFrame, data.frame(puntuaciones = max(puntuacionesDataFrame$puntuaciones), frecuencias = 0))
primer_valor_x <- min(puntuacionesDataFrame$puntuaciones)



ggplot(puntuacionesDataFrame_extended, aes(x = puntuaciones, y = frecuencias)) +
  geom_point(na.rm = FALSE, stat = "identity") +
  geom_line(na.rm = FALSE, stat = "identity") +
  ylim(0, max(puntuacionesDataFrame$frecuencias)) +
  xlim(12, max(puntuacionesDataFrame$puntuaciones))
  #scale_x_continuous(limits = c(10, 24), breaks = seq(10, 24, by = 1)) +
  #scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 1))

ggplot(puntuacionesDataFrame, aes(x = puntuaciones, y = frecuencias)) +
  geom_point(na.rm = FALSE, stat = "identity") +
  geom_line(na.rm = FALSE, stat = "identity") +
  xlab("Puntiaciones") +
  ylab("Frecuencias") + 
  ggtitle("Calificaciones de un grupo de alumnos") +
  
  scale_x_continuous(limits = c(10, 24), breaks = seq(10, 24, by = 1)) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 1))

rm(list = ls())

