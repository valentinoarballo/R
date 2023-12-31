# Función para calcular la moda de un conjunto de datos
moda = function(x) {
  ux = unique(x)
  cat("moda: ", ux [which.max(tabulate(match(x, ux)))])
}

# Función para calcular la media de un conjunto de valores
media = function (valores) {
  acumulador = 0
  # Calcula la suma de los valores
  for (valor in valores) {
    acumulador = acumulador + valor
  }
  media = (acumulador/length(valores)) # Calcula la media
  cat("media:", media)
  cat("media funcion R:", mean(valores))
}

# Función para calcular la media ponderada de un conjunto de valores con pesos
media_ponderada = function (valores, pesos) {
  acumulador = 0
  # Calcula la suma ponderada de los valores
  for (x in 1:length(valores)) {
   valor = valores[x]
   peso = pesos[x]
   # Calcula la suma ponderada de los valores
   acumulador = acumulador + (valor*peso)
  }
  # Calcula la media ponderada
  media_ponderada = (acumulador/sum(pesos))
  cat("media ponderada: ", media_ponderada)
}

# Función para calcular la media armónica de un conjunto de valores
media_armonica = function (valores) {
  acumulador = 0
  # Calcula la suma de los inversos de los valores
  for (valor in valores) {
    acumulador = acumulador + (1/valor)
  }
  # Calcula la media armónica
  media_armonica = (length(valores)/acumulador)
  cat("media armonica: ", media_armonica)
}

# Función para calcular la media cuadrática de un conjunto de valores
media_cuadratica = function (valores) {
  acumulador = 0
  for (valor in valores) {
    acumulador = acumulador + (valor^2)
  }
  # Calcula la media cuadrática
  media_cuadratica = (sqrt(acumulador/length(valores)))
  cat("media cuadratica: ", media_cuadratica)
}

# Función para calcular la media geométrica de un conjunto de valores
media_geometrica = function (valores) {
  media_geometrica = (prod(valores)^(1/(length(valores))))
  cat("media geometrica: ", media_geometrica)
}

# Función para calcular la mediana de un conjunto de valores
mediana = function (valores) {
  valores_ordenados = sort(valores)
  n = length(valores_ordenados)
  # Calcula la mediana dependiendo si la cantidad de valores es par o impar
  if (n %% 2 == 1) {
    mediana = valores_ordenados[(n+1)/2]
  } else {
    mediana = mean(valores_ordenados[c(n/2, n/2+1)])
  }
  cat("mediana: ", mediana)
}

# Función para calcular la media, moda y mediana de un conjunto de valores
media_moda_mediana = function (valores) {
  cat(media(valores), "\n")
  cat(moda(valores), "\n")
  cat(mediana(valores), "\n")
}

# Función para calcular todas las medidas estadísticas de un conjunto de valores
calcular_todo = function (valores, pesos) {
  media_moda_mediana(valores)
  cat(media_ponderada(valores, pesos), "\n")
  cat(media_armonica(valores), "\n")
  cat(media_cuadratica(valores), "\n")
  cat(media_geometrica(valores), "\n")
}

# <------------------------------------->
cat("Calcula la media simple en cada caso:")
cat("a) 4, 6, 8")
media(c(4, 6, 8))
cat("b) 14, 16, 18")
media(c(14, 16, 18))
cat("c) 100, 120, 180, 200")
media(c(100, 120, 180, 200))

# <------------------------------------->
cat("Calcula la media de los siguientes datos. Calcular TODOS los tipos de media\n")
valoresA4 = c(0, 2, 3, 4, 3, 1, 4, 3, 3, 4, 1, 3)
pesos = c(0.05,0.05,0.05,0.05,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
cat("a)", valoresA4)
calcular_todo(valoresA4, pesos)
valoresB4 = c(4, 1, 3, 0, 0, 3, 2, 2, 1, 3, 4, 1)
cat("b)", valoresB4)
calcular_todo(valoresB4, pesos)

# <------------------------------------->
cat("Calcula la media de los siguientes datos")
valoresA5 = c(2.4, 3, 1.1, 4, 3.5, 0.7, 0, 2.8, 3.8, 0.2, 2.8, 1.9)
valoresB5 = c(0.6, 3.8, 3.1, 4, 2.8, 0.2, 0.4, 3.1, 1.5, 1.9, 1.8, 3.1)
cat("a)" , valoresA5)
calcular_todo(valoresA5, pesos)
cat("b)" , valoresB5)
calcular_todo(valoresB5, pesos)

# <------------------------------------->
cat("Buscar la moda para los siguientes datos")
valoresA6 = c(2, 4, 3, 0, 2, 1, 1, 2, 3, 3, 3, 1)
valoresB6 = c(1, 1, 0, 1, 4, 0, 1, 3, 4, 0, 1, 2)
cat("a)", valoresA6)
moda(valoresA6)
cat("b)", valoresB6)
moda(valoresB6)

# <------------------------------------->
valores7 = c(25, 15, 28, 29, 25, 26, 21, 26)
cat("Buscar la media, la mediana y la moda de los siguientes números:\n", valores7)
media_moda_mediana(valores7)

# <------------------------------------->
valores8 = c(15, 16, 19, 15, 14, 16, 20, 15, 17)
cat("Buscar la media, la mediana y la moda de los siguientes números:\n", valores8)
media_moda_mediana(valores8)

# <------------------------------------->
valores9 = c(69, 73, 65, 70, 71, 74, 65, 69, 60, 62)
cat("En un estudio que se realizó en un asilo de ancianos, se tomó las edades de los que pueden caminar sin dificultades. Buscar la media, la mediana y la moda de las siguientes edades, e indicar si es muestra o población. No utilice la fórmula.\nA menos que en el asilo vivan 10 personas estos datos solo representan un muestra\nmuestra: ", valores9)
media_moda_mediana(valores9)

# <------------------------------------->
valores10 = c(3, 3, 4, 1, 1, 2, 2, 2, 5, 1, 4, 5, 1 ,5, 3, 5, 1, 4, 1, 2, 2, 1, 2, 3, 5)
cat("Se escogió un salón de clases de cuarto grado, con un total de 25 estudiantes, y se les pidió que calificaran del 1 al 5 un programa televisivo. Estos fueron los resultados:\n", valores10, "\nBuscar la media, la moda y la mediana e indicar si es muestra o población.\nRta: Es poblacion.")
media_moda_mediana(valores10)

# <------------------------------------->
cat("En una importante empresa láctea hay: 600 empleados que cobran $400, 500 que cobran $600, 100 que cobran $2200, 5 socios que perciben $100000 cada uno.")

valores11 = c(rep(400, 600), rep(600, 500), rep(2200, 100), rep(100000, 5))
media_moda_mediana(valores11)



