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

media_simple = function (valores) {
  acumulador = 0
  for (valor in valores) {
    acumulador = acumulador + valor
  }
  return (acumulador/length(valores))
}

media_ponderada = function (valores, pesos) {
  acumulador = 0
  for (x in 1:length(valores)) {
   valor = valores[x]
   peso = pesos[x]
   acumulador = acumulador + (valor*peso)
  }
  return (acumulador/sum(pesos))
}

media_armonica = function (valores) {
  acumulador = 0
  for (valor in valores) {
    acumulador = acumulador + (1/valor)
  }
  return (length(valores)/acumulador)
}

media_cuadratica = function (valores) {
  acumulador = 0
  for (valor in valores) {
    acumulador = acumulador + (valor^2)
  }
  
  return (sqrt(acumulador/length(valores)))
}

media_geometrica = function (valores) {
  return  (prod(valores)^(1/(length(valores)))) #(geometric.mean(valores))
}

separador(3)
cat("Calcula la media simple en cada caso:")
cat("a) 4, 6, 8")
media_simple(c(4, 6, 8))
cat("b) 14, 16, 18")
media_simple(c(14, 16, 18))
cat("c) 100, 120, 180, 200")
media_simple(c(100, 120, 180, 200))

separador(4)
cat("Calcula la media de los siguientes datos. Calcular TODOS los tipos de media\n")
valoresA4 = c(0, 2, 3, 4, 3, 1, 4, 3, 3, 4, 1, 3)
pesos = c(0.05,0.05,0.05,0.05,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
cat("a)", valoresA4)
media_simple(valoresA4)
media_ponderada(valoresA4, pesos)
media_armonica(valoresA4)
media_cuadratica(valoresA4)
media_geometrica(valoresA4)
valoresB4 = c(4, 1, 3, 0, 0, 3, 2, 2, 1, 3, 4, 1)
cat("b)", valoresB4)
media_simple(valoresB4)
media_ponderada(valoresB4, pesos)
media_armonica(valoresB4)
media_cuadratica(valoresB4)
media_geometrica(valoresB4)

separador(5)
cat("Calcula la media de los siguientes datos")
valoresA5 = c(2.4, 3, 1.1, 4, 3.5, 0.7, 0, 2.8, 3.8, 0.2, 2.8, 1.9)
valoresB5 = c(0.6, 3.8, 3.1, 4, 2.8, 0.2, 0.4, 3.1, 1.5, 1.9, 1.8, 3.1)
cat("a)" , valoresA5)
media_simple(valoresA5)
media_ponderada(valoresA5, pesos)
media_armonica(valoresA5)
media_cuadratica(valoresA5)
media_geometrica(valoresA5)
cat("a)" , valoresB5)
media_simple(valoresB5)
media_ponderada(valoresB5, pesos)
media_armonica(valoresB5)
media_cuadratica(valoresB5)
media_geometrica(valoresB5)

separador(6)
cat("Buscar la moda para los siguientes datos")
valoresA6 = c(2, 4, 3, 0, 2, 1, 1, 2, 3, 3, 3, 1)
valoresB6 = c(1, 1, 0, 1, 4, 0, 1, 3, 4, 0, 1, 2)

cat("a)", valoresA6)

moda(valoresA6)


cat("b)", valoresB6)
moda(valoresB6)




















