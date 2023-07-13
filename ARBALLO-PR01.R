install.packages("psych")
library("psych")
options(echo=FALSE)

separador = function (ejercicio) {
  cat("\n\n<-------------------------- Ejercicio Nº",ejercicio," -------------------------->\n\n")
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

separador(1)
media_simple(c(4, 6, 8))
media_simple(c(14, 16, 18))
media_simple(c(100, 120, 180, 200))

separador(2)

media_simple(c(0, 2, 3, 4, 3, 1, 4, 3, 3, 4, 1, 3))
media_ponderada(c(0, 2, 3, 4, 3, 1, 4, 3, 3, 4, 1, 3), c(0.05,0.05,0.05,0.05,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))
media_armonica(c(0, 2, 3, 4, 3, 1, 4, 3, 3, 4, 1, 3))
media_cuadratica(c(0, 2, 3, 4, 3, 1, 4, 3, 3, 4, 1, 3))
media_geometrica(c(0, 2, 3, 4, 3, 1, 4, 3, 3, 4, 1, 3))

separador(3)

media_simple(c(2.4, 3, 1.1, 4, 3.5, 0.7, 0, 2.8, 3.8, 0.2, 2.8, 1.9))
media_ponderada(c(2.4, 3, 1.1, 4, 3.5, 0.7, 0, 2.8, 3.8, 0.2, 2.8, 1.9), c(0.05,0.05,0.05,0.05,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))
media_armonica(c(2.4, 3, 1.1, 4, 3.5, 0.7, 0, 2.8, 3.8, 0.2, 2.8, 1.9))
media_cuadratica(c(2.4, 3, 1.1, 4, 3.5, 0.7, 0, 2.8, 3.8, 0.2, 2.8, 1.9))
media_geometrica(c(2.4, 3, 1.1, 4, 3.5, 0.7, 0, 2.8, 3.8, 0.2, 2.8, 1.9))
