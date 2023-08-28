# > Installo la libreria con la que voy a trabajar, se llama ggplot2
install.packages("ggplot2")
# > Elegi esta libreria principalmente por su popularidad, note que es una de las mas utilizadas
# > Tiene una comunidad grande y muchos recursos educativos en internet
library(ggplot2)

separador = function (ejercicio) {
  cat("<------------------------- Ejercicio Nº",ejercicio," -------------------------->")
}


separador(1)
# > Defino las puntuaciomes y las frecuencias como vectores.
puntuaciones = c(13, 13, 14, 15, 16, 18, 19, 20, 22, 22)
frecuencias = c(0, 3, 1, 5, 4, 3, 1, 2, 1, 0)

# > Aca creo un marco de datos que relaciona las puntuaciones con las frecuencias.
# > Cada fila en el marco de datos representa una puntuacion y su frecuencia.
puntuacionesDataFrame = data.frame(puntuaciones = puntuaciones, frecuencias = frecuencias)

# > Esto crea un objeto de grafico base con los datos del data frame "puntuacionesDataFrame".
# > Ademas establece como se mapearan las variables puntuaciones y frecuencias en los ejes x e y respectivamente.
ggplot(puntuacionesDataFrame, aes(x = puntuaciones, y = frecuencias)) +

# > Con la base creada, esta linea lo que hace es agregar puntos al grafico
  geom_point(na.rm = FALSE, stat = "identity") +
# > Esta linea agrega una linea al grafico, tanto esta como la anterior tienen el parametro na.rm = FALSE
#   esto para que no borre los valores "NA".
  geom_line(na.rm = FALSE, stat = "identity") +
# > Esto solo agrega etiquetas.
  xlab("Puntiaciones") +
  ylab("Frecuencias") + 
# > Le doy un titulo al grafico.
  ggtitle("Calificaciones de un grupo de alumnos") +
# > Aca ajusto los limites y marcas del eje X e Y. 
  scale_x_continuous(limits = c(10, 24), breaks = seq(10, 24, by = 1)) +
# > Con limits() establezco los limites del grafico .
# > Con break = seq(0, 6, by = 1) le estoy diciendo que las marcas vayan de 1 en 1 del 0 al 6.
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 1))

separador(2)
# > Defino las puntuaciomes y las frecuencias como vectores.
numeroEstrellas = c(1, 2, 3, 4)
frecuencias2 = c(6, 12, 16, 4)

# > Creo un marco de datos que relaciona las puntuaciones con las frecuencias.
estrellasDataFrame = data.frame(numeroEstrellas = numeroEstrellas, frecuencias2 = frecuencias2)

# > Creo la base del grafico
ggplot(estrellasDataFrame, aes(x = numeroEstrellas, y = frecuencias2)) +
# > Con geom_bar() le agrego barras a la base
# > Con el parametro stat = "identity" para representar directamente las alturas de las barras 
#   segun los valores proporcionados en los datos
  geom_bar(stat = "identity") +
  xlab("Estrellas") +
  ylab("Cantidad de Hoteles") + 
  
# > De nuevo ajusto los limites y las marcas de los ejes
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, y = 1)) +
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 16, by = 1))

separador(3)
calificaciones = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
frecuencias3 = c(1, 1, 2, 3, 6, 11, 12, 7, 4, 2, 1)

notasDataFrame = data.frame(calificaciones = calificaciones, frecuencias3 = frecuencias3)

ggplot(notasDataFrame, aes(x = calificaciones, y = frecuencias3)) +
  geom_bar(na.rm = FALSE, stat = "identity") +
  xlab("Calificaciones") +
  ylab("Alumnos") + 
  scale_x_continuous(limits = c(-1, 11), breaks = seq(-1, 11, by = 1)) +
  scale_y_continuous(limits = c(0, 13), breaks = seq(0, 13, by = 1))
 

separador(4)
# > Aca ingreso los limites superiores y inferiores a mano, es la forma mas sencilla que encontre de hacer esto
limites_inferiores4 = c(50, 60, 70, 80, 90, 100, 110)
limites_superiores4 = c(60, 70, 80, 90, 100, 110, 120)
frecuencias4 = c(8, 10, 16, 14, 10, 5, 2)

# > Creo un data frame donde cada fila va a tener 3 valores, el limite superior, el inferior y su frecuencia
intervalos4 = data.frame(limites_inferiores4 = limites_inferiores4,
                         limites_superiores4 = limites_superiores4,
                         frecuencias4 = frecuencias4)
print(intervalos4)

# > Aca estoy diciendole que sume las variables limite del data frame intervalos4 y divida por 2 el resultado
# > Con el sibolo $ puedo acceder a una columna de valores dentro del data frame

puntosMedios4 = (intervalos4$limites_inferiores4 + intervalos4$limites_superiores4) / 2
# > Cada resultado se guardara en forma de vector en puntosMedios4
print(puntosMedios4)

empleadosDataFrame = data.frame(puntosMedios4 = puntosMedios4, frecuencias4 = frecuencias4)

ggplot(empleadosDataFrame, aes(x = puntosMedios4, y = frecuencias4)) +
  geom_bar(stat = "identity") +
  xlab("Pesos de los empleados") +
  ylab("Empleados") + 
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
  xlab("Notas del examen de fisica") +
  ylab("Estudiantes") + 
  scale_x_continuous(limits = c(0, 52), breaks = seq(3, 52, by = 8)) +
  scale_y_continuous(limits = c(0, 13), breaks = seq(0, 13, by = 1)) 


