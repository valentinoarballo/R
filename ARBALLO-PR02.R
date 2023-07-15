source("ARBALLO-PR01.R")

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

principales_medidas_dispercion = function (valores) {
  cat(desviacion_media(valores), "\n")
  cat(rango(valores), "\n")
  cat(desviacion_tipica(valores), "\n")
  cat(varianza(valores), "\n")
}

calcular_todo = function (valores) {
  cat("media: ",media(valores), "\n")
  cat(moda(valores), "\n")
  cat(mediana(valores), "\n")
  principales_medidas_dispercion(valores)
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
rm(list = ls())