install.packages("psych")
library("psych")
options(echo=FALSE)

separador = function (ejercicio) {
  cat("\n\n<-------------------------- Ejercicio Nº",ejercicio," -------------------------->\n\n")
}

moda = function(x) {
  ux = unique(x)
  ux [which.max(tabulate(match(x, ux)))]
}

media = function (valores) {
  acumulador = 0
  for (valor in valores) {
    acumulador = acumulador + valor
  }
  media = (acumulador/length(valores))
  cat("media: ", media)
}


media_ponderada = function (valores, pesos) {
  acumulador = 0
  for (x in 1:length(valores)) {
   valor = valores[x]
   peso = pesos[x]
   acumulador = acumulador + (valor*peso)
  }
  media_ponderada = (acumulador/sum(pesos))
  cat("media_ponderada: ", media_ponderada)
}


media_armonica = function (valores) {
  acumulador = 0
  for (valor in valores) {
    acumulador = acumulador + (1/valor)
  }
  media_armonica = (length(valores)/acumulador)
  cat("media_armonica: ", media_armonica)
}


media_cuadratica = function (valores) {
  acumulador = 0
  for (valor in valores) {
    acumulador = acumulador + (valor^2)
  }
  media_cuadratica = (sqrt(acumulador/length(valores)))
  cat("media_cuadratica: ", media_cuadratica)
}


media_geometrica = function (valores) {
  media_geometrica = (prod(valores)^(1/(length(valores)))) #(geometric.mean(valores))
  cat("media_geometrica: ", media_geometrica)
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


separador(3)
cat("Calcula la media simple en cada caso:")
cat("a) 4, 6, 8")
media(c(4, 6, 8))
cat("b) 14, 16, 18")
media(c(14, 16, 18))
cat("c) 100, 120, 180, 200")
media(c(100, 120, 180, 200))

separador(4)
cat("Calcula la media de los siguientes datos. Calcular TODOS los tipos de media\n")
valoresA4 = c(0, 2, 3, 4, 3, 1, 4, 3, 3, 4, 1, 3)
pesos = c(0.05,0.05,0.05,0.05,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
cat("a)", valoresA4)
cat("media ", media(valoresA4))
cat("media ponderada: ", media_ponderada(valoresA4, pesos))
cat("media armonica: ", media_armonica(valoresA4))
cat("media cuadratica: ", media_cuadratica(valoresA4))
cat("media geometrica: ", media_geometrica(valoresA4))
valoresB4 = c(4, 1, 3, 0, 0, 3, 2, 2, 1, 3, 4, 1)
cat("b)", valoresB4)
cat("media ", media(valoresB4))
cat("media ponderada: ", media_ponderada(valoresB4, pesos))
cat("media armonica: ", media_armonica(valoresB4))
cat("media cuadratica: ", media_cuadratica(valoresB4))
cat("media geometrica: ", media_geometrica(valoresB4))

separador(5)
cat("Calcula la media de los siguientes datos")
valoresA5 = c(2.4, 3, 1.1, 4, 3.5, 0.7, 0, 2.8, 3.8, 0.2, 2.8, 1.9)
valoresB5 = c(0.6, 3.8, 3.1, 4, 2.8, 0.2, 0.4, 3.1, 1.5, 1.9, 1.8, 3.1)
cat("a)" , valoresA5)
cat("media ", media(valoresA5))
cat("media ponderada: ", media_ponderada(valoresA5, pesos))
cat("media armonica: ", media_armonica(valoresA5))
cat("media cuadratica: ", media_cuadratica(valoresA5))
cat("media geometrica: ", media_geometrica(valoresA5))
cat("a)" , valoresB5)
cat("media ", media(valoresB5))
cat("media ponderada: ", media_ponderada(valoresB5, pesos))
cat("media armonica: ", media_armonica(valoresB5))
cat("media cuadratica: ", media_cuadratica(valoresB5))
cat("media geometrica: ", media_geometrica(valoresB5))

separador(6)
cat("Buscar la moda para los siguientes datos")
valoresA6 = c(2, 4, 3, 0, 2, 1, 1, 2, 3, 3, 3, 1)
valoresB6 = c(1, 1, 0, 1, 4, 0, 1, 3, 4, 0, 1, 2)
cat("a)", valoresA6)
cat("moda: ", moda(valoresA6))
cat("b)", valoresB6)
cat("moda: ", moda(valoresB6))

separador(7)
cat("Buscar la media, la mediana y la moda de los siguientes números:")

valores7 = c(25, 15, 28, 29, 25, 26, 21, 26)
cat("media ", media(valores7))
mediana(valores7)
cat("moda: ", moda(valores7))

separador(8)
cat("Buscar la media, la mediana y la moda de los siguientes números:")
valores8 = c(15, 16, 19, 15, 14, 16, 20, 15, 17)
cat("media ", media(valores8))
mediana(valores8)
cat("moda: ", moda(valores8))


separador(9)
cat("En un estudio que se realizó en un asilo de ancianos, se tomó las edades de los que pueden caminar
sin dificultades. Buscar la media, la mediana y la moda de las siguientes edades, e indicar si es
muestra o población. No utilice la fórmula.")


cat("a menos que en el asilo vivan 10 personas estos datos solo representan un muestra")
valores9 = c(69, 73, 65, 70, 71, 74, 65, 69, 60, 62)
cat(valores9)
cat("media ", media(valores9))
mediana(valores9)
cat("moda: ", moda(valores9))

separador(10)
cat("Se escogió un salón de clases de cuarto grado, con un total de 25 estudiantes, y se les pidió que
calificaran del 1 al 5 un programa televisivo. Estos fueron los resultados:")
valores10 = c(3, 3, 4, 1, 1, 2, 2, 2, 5, 1, 4, 5, 1 ,5, 3, 5, 1, 4, 1, 2, 2, 1, 2, 3, 5)
cat(valores10)
cat("Buscar la media, la moda y la mediana e indicar si es muestra o población.")
cat("es poblacion.")
cat("media ", media(valores10))
mediana(valores10)
cat("moda: ", moda(valores10))



