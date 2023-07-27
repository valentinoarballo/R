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
numeroEstrellas = c(1, 2, 3, 4)
frecuencias2 = c(6, 12, 16, 4)

estrellasDataFrame = data.frame(numeroEstrellas = numeroEstrellas, frecuencias2 = frecuencias2)

ggplot(estrellasDataFrame, aes(x = numeroEstrellas, y = frecuencias2)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, y = 1)) +
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 16, by = 1))

separador(3)
calificaciones = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
frecuencias3 = c(1, 1, 2, 3, 6, 11, 12, 7, 4, 2, 1)

notasDataFrame = data.frame(calificaciones = calificaciones, frecuencias3 = frecuencias3)

ggplot(notasDataFrame, aes(x = calificaciones, y = frecuencias3)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, y = 1)) +
  scale_y_continuous(limits = c(0, 13), breaks = seq(0, 13, by = 1))
 

separador(4)
limites_inferiores4 = c(50, 60, 70, 80, 90, 100, 110)
limites_superiores4 = c(60, 70, 80, 90, 100, 110, 120)
frecuencias4 = c(8, 10, 16, 14, 10, 5, 2)

intervalos4 = data.frame(limites_inferiores4 = limites_inferiores4,
                         limites_superiores4 = limites_superiores4,
                         frecuencias4 = frecuencias4)

puntosMedios4 = (intervalos4$limites_inferiores4 + intervalos4$limites_superiores4) / 2

empleadosDataFrame = data.frame(puntosMedios4 = puntosMedios4, frecuencias4 = frecuencias4)

ggplot(empleadosDataFrame, aes(x = puntosMedios4, y = frecuencias4)) +
  geom_bar(stat = "identity") +
  geom_line(na.rm = FALSE, stat = "identity") +
  scale_x_continuous(limits = c(40, 125), breaks = seq(40, 125, by = 10)) +
  scale_y_continuous(limits = c(0, 17), breaks = seq(0, 17, by = 1)) 


separador(5)
limites_inferiores5 = c(3, 11, 19, 27, 35, 43)
limites_superiores5 = c(11, 19, 27, 35, 43, 51)
frecuencias5 = c(2, 6, 6, 11, 12, 3)

intervalos5 = data.frame(limites_inferiores5 = limites_inferiores5,
                         limites_superiores5 = limites_superiores5,
                         frecuencias5 = frecuencias5)

puntosMedios5 = (intervalos5$limites_inferiores5 + intervalos5$limites_superiores5) / 2

alumnnosDataFrame = data.frame(puntosMedios5 = puntosMedios5, frecuencias5 = frecuencias5)

ggplot(alumnnosDataFrame, aes(x = puntosMedios5, y = frecuencias5)) +
  geom_bar(stat = "identity") +
  geom_line(na.rm = FALSE, stat = "identity") +
  scale_x_continuous(limits = c(0, 52), breaks = seq(3, 52, by = 8)) +
  scale_y_continuous(limits = c(0, 13), breaks = seq(0, 13, by = 1)) 
# rm(list = ls())

