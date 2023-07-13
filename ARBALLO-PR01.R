
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
  productos = c()
  for (x in 1:length(valores)) {
   valor = valores[x]
   peso = pesos[x]
    productos = c(productos, (valor*peso))
  }
  return (sum(productos)/sum(pesos))
}

separador(1)

media_simple(c(4, 6, 8))

separador(2)

media_ponderada(c(0, 2, 3, 4, 3, 1, 4, 3, 3, 4, 1, 3), c(0.05,0.05,0.05,0.05,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))



