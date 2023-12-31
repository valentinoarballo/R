# > Installo la libreria con la que voy a trabajar, se llama ggplot2
install.packages("ggplot2")
# > Elegi esta libreria principalmente por su popularidad, note que es una de las mas utilizadas
# > Tiene una comunidad grande y muchos recursos educativos en internet
library(ggplot2)

separador = function (ejercicio) {
  cat("\n\n=------------------------- Ejercicio Nº",ejercicio," -------------------------->\n\n")
}

rango = function (valores) {
  cat("rango: ", max(valores)-min(valores))
}

moda = function(x) {
  ux = unique(x)
  cat("moda: ", ux [which.max(tabulate(match(x, ux)))])
}

media = function (valores) {
  media = (sum(valores)/length(valores))
  return (media)
}

mediana = function (valores) {
  valores_ordenados = sort(valores)
  n = length(valores_ordenados)
  # > Si n es impar 
  if (n %% 2 == 1) {
    # > Se le asigna a mediana el valor unico central del conjunto de datos
    mediana = valores_ordenados[(n+1)/2]
  } else { # si es par
    # > Agarro los dos valores adyacentes al centro y le asigno a mediana el promedio de estos dos 
    mediana = mean(valores_ordenados[c(n/2, n/2+1)])
  }
  cat("mediana:", mediana)
  cat("\nmediana funcion R median():", median(valores))
}

desviacion_media = function (valores){
  # > Imprimo la sumatoria de diferencias asolutas de cada valor a su media dividido por length de valores
  cat("desviacion media:", sum(abs(valores - media(valores)))/length(valores))
}

varianza = function (valores, poblacion = TRUE){
  # > Acumulador representa la sumatoria de diferencias al cuadrado  
  acumulador = sum((valores - media(valores))^2)
  # > Si es poblacion
  if (poblacion) {
    # > Dividir por la cantidad de datos
    cat("varianza:", acumulador/length(valores), "\n")
  } else { # si es una muestra
    # > Dividir pero antes restarle 1 a length(valores)
    cat("varianza:", acumulador/(length(valores)-1), "\n")
    cat("varianza funcion R var():", var(valores, na.rm = TRUE))
  }
}

desviacion_tipica = function (valores, poblacion = TRUE) {
  acumulador = sum((valores - media(valores))^2)
  if (poblacion) {
    cat("desviacion tipica:", sqrt(acumulador/length(valores)))
  } else {
    cat("desviacion tipica:", sqrt(acumulador/(length(valores)-1)))
    cat("\ndesviacion tipica funcion R sd():", sd(valores, na.rm = TRUE))
  }
}

# > Hago una funcion para llamar funciones, asi es mas comodo si necesito conocer todas las medidas de dispersion
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

# > Esta funcion llama a la media, moda y mediana ademas de llamar a la funcion de arriba
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
cat("Las temperaturas maximas en una ciudad durante el mes de enero fueron:\n", valores4, "\n")
calcular_todo(valores4)

separador(5)
valores5 = c(69, 73, 65, 70, 71, 74, 65, 69, 60, 62)
cat("En un estudio que se realizo en un asilo de ancianos, se tomo las edades de los que pueden caminar sin dificultades. Calcular las principales medidas de dispersion\nLos datos: ", valores5)
principales_medidas_dispercion(valores5)

separador(6)
valores6 = c(3, 3, 4, 1, 1, 2, 2, 2, 5, 1, 4, 5, 1, 5, 3, 5, 1, 4, 1, 2, 2, 1, 2, 3, 5)
cat("Se escogio un salon de clases de cuarto grado, con un total de 25 estudiantes, y se les pidio que calificaran del 1 al 5 un programa televisivo.\ Estos fueron los resultados:", valores6)
principales_medidas_dispercion(valores6)

separador(7)
limites_inferiores = c(170, 175, 180, 185, 190, 195)
limites_superiores = c(175, 180, 185, 190, 195, 200)
frecuencias = c(1, 3, 4, 8, 5, 2)
# paso los datos a un df
datos_intervalos = data.frame(LimiteInferior = limites_inferiores,
                              LimiteSuperior = limites_superiores,
                              Frecuencia = frecuencias
                            )

cat("Las alturas de los jugadores de un equipo de baloncesto vienen dadas por la tabla:")
print(datos_intervalos)

# > Ahora necesitaria sacar el valor medio de cada intervalo para poder trabajarlo
puntosMedios = (datos_intervalos$LimiteInferior + datos_intervalos$LimiteSuperior) / 2
# > Uso el operador $ para acceder a los datos de la columna y asi poder calcular su media
cat("media: ", media(puntosMedios))
cat("media funcion R mean(): ", mean(puntosMedios))
moda(puntosMedios)
mediana(puntosMedios)


separador(8)
valores8 = c(6, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 11)
cat("un siquiatra local ha considerado una muestra aleatoria de 20 niños, anotando el tiempo necesario que requiere en cada niño para lograr un plan integral del tratamiento.\n Obteniendose lo siguiente (en horas):", valores8)
calcular_todo(valores8, poblacion = FALSE)

valores8DataFrame = data.frame(horas = valores8, kids = rep("niñ@s con mala conducta", length(valores8)))
# > Paso los datos a un data frame para que cada fila represente una variable/dato y cada columna una instancia de datos

# > Con la libreria ggplot especifico las variables que voy a usar para representar los ejes del grafico, con la funcion aes()
ggplot(valores8DataFrame, aes(x = kids, y = horas)) + 
# > geom_boxplot() es la funcion que se encarga de dibujar el grafico
  geom_boxplot() +
  scale_y_continuous(limits = c(6, 11), breaks = seq(6, 11, 1))

separador(9)
valores9 = c(10.5, 11.3, 11.9, 12, 12.3, 12.3, 12.5, 12.7, 13.4, 13.7, 13.8, 14.2, 14.8, 15.1, 15.3, 16.7, 16.8, 18.8, 20.8)
cat("Dos profesores estan interesados en estudiar los habitos de sueño de los estudiantes en sus clases.\n Los datos del Profesor B son los siguientes:", valores9)

valores9DataFrame = data.frame(minutos = valores9, alumnos = rep("alumnos que se duermen en clase", length(valores9)))
ggplot(valores9DataFrame, aes(x = alumnos, y = minutos)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(10, 22), breaks = seq(10, 22, 1))

separador(10)
cat("En una empresa se seleccionaron cinco trabajadores, se anotaron sus años de servicio y el tiempo en horas solicitado en el último mes. Los resultados obtenidos fueron:")
servicio10 = c(1, 3, 2, 4, 5, 4)
horas10 = c(1, 1, 3, 4, 6, 5)

correlacion10 = cor(servicio10, horas10)
correlacion10DataFrame = data.frame(servicio10, horas10)

# > Grafico la regresion con ggplot2
ggplot(correlacion10DataFrame, aes(x = servicio10, y = horas10)) +
# > geom_point() dibuja los puntos
  geom_point() +
# > Uso geom_smooth() para agregar una linea encima de los puntos
# > El parametro method = "lm" especifica que voy a usar el metodo de ajuste lineal (regresion lineal) para trazar la linea de ajuste
# > El metodo "lm" usa el algoritmo de minimos cuadrados para encontrar la mejor linea recta que se ajuste a los datos
# > Pongo se = FALSE  para desactivar el sombreado del intervalo de confianza alrededor de la linea de ajudte
  geom_smooth(method = "lm", se = FALSE) +
# > Aca solo pongo titulos, al grafico y a los ejes
  labs(title = "Correlacion entre Servicio y Horas", x = "Servicio", y = "Horas") +
# > geom_text() sirve para poner un texto en el grafico, le digo en que posicion de x e y lo quiero
  geom_text(x = 4, y = 1, label = paste("Correlacion:", round(correlacion10, 2)), color = "black") +
  theme_minimal()



