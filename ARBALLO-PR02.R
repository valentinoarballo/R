install.packages("ggplot2")
library(ggplot2)

separador = function (ejercicio) {
  cat("\n\n<-------------------------- Ejercicio Nº",ejercicio," -------------------------->\n\n")
}

rango = function (valores) {
  cat("rango: ", max(valores)-min(valores))
}

moda = function(x) {
  ux = unique(x)
  cat("moda: ", ux [which.max(tabulate(match(x, ux)))])
}

media = function (valores) {
  acumulador = 0
  for (valor in valores) {
    acumulador = acumulador + valor
  }
  media = (acumulador/length(valores))
  return (media)
}

mediana = function (valores) {
  valores_ordenados = sort(valores)
  n = length(valores_ordenados)
  if (n %% 2 == 1) {
    mediana = valores_ordenados[(n+1)/2]
  } else {
    mediana = mean(valores_ordenados[c(n/2, n/2+1)])
  }
  cat("valores ordenados:", valores_ordenados, "\n")
  cat("mediana: ", mediana)
}

desviacion_media = function (valores){
  acumulador = 0
  for (valor in valores) {
    acumulador = acumulador + abs(valor - media(valores)) 
  }
  cat("desviacion media:", acumulador/length(valores))
}

varianza = function (valores, poblacion = TRUE){
  acumulador = 0
  for (valor in valores) {
    acumulador = acumulador + (valor - media(valores))^2
  }
  if (poblacion) {
  cat("varianza:", acumulador/length(valores))
  } else {
    cat("varianza:", acumulador/(length(valores)-1))
  }
}

desviacion_tipica = function (valores, poblacion = TRUE) {
  acumulador = 0
  for (valor in valores) {
    acumulador = acumulador + (valor - media(valores))^2
  }
  if (poblacion) {
    cat("desviacion tipica:", sqrt(acumulador/length(valores)))  
  } else {
    cat("desviacion tipica:", sqrt(acumulador/(length(valores)-1)))
  }
}

principales_medidas_dispercion = function (valores, poblacion = TRUE) {
  cat(desviacion_media(valores), "\n")
  cat(rango(valores), "\n")
  if (!poblacion){
    cat(desviacion_tipica(valores, poblacion = FALSE), "\n")
    cat(varianza(valores, poblacion = FALSE), "\n")  
  } else {
    cat(desviacion_tipica(valores), "\n")
    cat(varianza(valores), "\n")
  }
  
}

calcular_todo = function (valores, poblacion = TRUE) {
  cat("media: ",media(valores), "\n")
  cat(moda(valores), "\n")
  cat(mediana(valores), "\n")
  if (!poblacion){
    principales_medidas_dispercion(valores, poblacion = FALSE)
  } else {
    principales_medidas_dispercion(valores)
  }
}

separador(4)
valores4 = c(28, 29, 28, 30, 30, 29, 30, 31, 29, 29, 30, 31, 31, 31, 32, 33, 34, 34, 35, 31, 31, 32, 32, 33, 33, 31, 32, 32, 33, 33, 34)
cat("Las temperaturas máximas en una ciudad durante el mes de enero fueron:\n", valores4, "\n")
calcular_todo(valores4)

separador(5)
valores5 = c(69, 73, 65, 70, 71, 74, 65, 69, 60, 62)
cat("En un estudio que se realizó en un asilo de ancianos, se tomó las edades de los que pueden caminar sin dificultades. Calcular las principales medidas de dispersión\nLos datos: ", valores5)
principales_medidas_dispercion(valores5)

separador(6)
valores6 = c(3, 3, 4, 1, 1, 2, 2, 2, 5, 1, 4, 5, 1, 5, 3, 5, 1, 4, 1, 2, 2, 1, 2, 3, 5)
cat("Se escogió un salón de clases de cuarto grado, con un total de 25 estudiantes, y se les pidió que calificaran del 1 al 5 un programa televisivo.\ Estos fueron los resultados:", valores6)
principales_medidas_dispercion(valores6)

separador(7)
#holy

separador(8)
valores8 = c(6, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 11)
cat("un siquiatra local ha considerado una muestra aleatoria de 20 niños, anotando el tiempo necesario que requiere en cada niño para lograr un plan integral del tratamiento.\n Obteniéndose lo siguiente (en horas):", valores8)
calcular_todo(valores8, poblacion = FALSE)


valores8DataFrame = data.frame(houras = valores8, kids = rep("niñ@s con mala conducta", length(valores8)))
# paso los datos a un data frame para tener una especie de matriz pero que cada fila represente una variable/dato y cada columna una instancia de datos

ggplot(valores8DataFrame, aes(x = kids, y = houras)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(6, 11), breaks = seq(6, 11, 1))
# la funcion "scale_y_continuous" la uso para poner limites en la escala vertical (10 a 22)
# la precision de la escala (1) es para los valores de los intervalos (12, 13, 14, ..., 22)




separador(9)
valores9 = c(10.5, 11.3, 11.9, 12, 12.3, 12.3, 12.5, 12.7, 13.4, 13.7, 13.8, 14.2, 14.8, 15.1, 15.3, 16.7, 16.8, 18.8, 20.8)
cat("Dos profesores están interesados en estudiar los hábitos de sueño de los estudiantes en sus clases.\n Los datos del Profesor B son los siguientes:", valores9)

valores9DataFrame = data.frame(minutos = valores9, alumnos = rep("alumnos que se duermen en clase", length(valores9)))

ggplot(valores9DataFrame, aes(x = alumnos, y = minutos)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(10, 22), breaks = seq(10, 22, 1))




rm(valoresDataFrame)
rm(list = ls())