acciones_vendidas = c(200, 300, 500, 300)

precios = matrix(
  c(20, 30, 45, 100),
  nrow = 4
  )

colnames(precios) = c("Precio")
rownames(precios) = c("A", "B", "C", "D")

resultado = acciones_vendidas %*% precios

print(resultado)

rm(list = ls())

#<------------------------------------->

muebles = matrix(
  c(1000, 8000, 8000, 6000, 4000, 6000),
  ncol = 3,
  nrow = 2
  )

colnames(muebles) = c("modelo A", "modelo B", "modelo C")
rownames(muebles) = c("grandes", "pequeños")

recursos = matrix(
  c(16, 6, 12, 4),
  ncol = 2
)

colnames(recursos) = c("grandes", "pequeños")
rownames(recursos) = c("tornillos", "tablas")

resultado = recursos %*% muebles

print(resultado)

rm(list = ls())

#<------------------------------------->

lavadoras = matrix(
  c(400, 300, 200, 100, 50, 30),
  ncol = 2,
  nrow = 3,
  byrow = TRUE
)

colnames(lavadoras) = c("modelo A", "modelo B")
rownames(lavadoras) = c("terminación N", "terminación L", "terminación S")

horas = matrix(
  c(25, 30, 33, 1, 1.2, 1.3),
  ncol = 3,
  byrow = TRUE
)

colnames(horas) = c("terminación N", "terminación L", "terminación S")
rownames(horas) = c("horas de taller", "horas de administración")

resultado = horas %*% lavadoras

print(resultado)

rm(list = ls())

#<------------------------------------->


lingotes = matrix(
  c(20, 30, 40, 30, 40, 50, 40, 50, 90),
  nrow = 3, 
  byrow = TRUE
)
 
nuevo_lingote = c(34, 46, 67)

rm(list = ls())

#<------------------------------------->

minas = matrix(
  c(1, 2, 3, 2, 5, 7, 1, 3, 1)
  
)











