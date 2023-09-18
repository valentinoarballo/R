

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

#<------------------------------------------------------------------------------------------------------------------------->

lingotes = matrix(
  c(20, 30, 40, 30, 40, 50, 40, 50, 90),
  nrow = 3, 
  ncol = 3
)
rownames(lingotes) = c("L1","L2","L3")
colnames(lingotes) = c("Oro","Plata","Cobre")

lingotes

cat("masa total de cada lingote:")

masaTotal = rowSums(lingotes)
masaTotal

nuevo_lingote = matrix(
  c(34, 46, 67),
  nrow = 1,
  ncol = 3
  )
rownames(nuevo_lingote) = c("L4")
colnames(nuevo_lingote) = c("Oro","Plata","Cobre")

nuevo_lingote

masaTotalNuevoLingote = rowSums(nuevo_lingote)
masaTotalNuevoLingote


#resuelvo las incognitas
incognitas = qr.solve(lingotes, t(nuevo_lingote))
colnames(incognitas) = "%"
rownames(incognitas) = c("L1","L2","L3")
incognitas * 100

# masa correspondiente
partes = incognitas * masaTotal
rownames(partes) = c("L1","L2","L3")
colnames(partes) = "gramos en L4"

partes


rm(list = ls())

#<------------------------------------------------------------------------------------------------------------------------->
minas = matrix(
  c(1, 2, 3, 2, 5, 7, 1, 3, 1),
  ncol = 3,
  byrow = TRUE
)

colnames(minas) = c("Niquel", "Cobre", "Hierro")
rownames(minas) = c("Mina A", "Mina B", "Mina C") 

print(minas)

# ¿Cuántas toneladas de cada mina deben utilizarse para obtener 7 toneladas de níquel, 18 de cobre y 16 de hierro?

objetivo = c(7, 18, 16)

resultado = solve(minas, objetivo)
print (resultado)

rm(list = ls())

#<------------------------------------------------------------------------------------------------------------------------> 

hijo_uno = 20
hijo_dos = 5


suma_hijos = (hijo_uno + hijo_dos)
edad_padre = (suma_hijos)*2



diferencia_edades = hijo_uno - hijo_dos

suma_hijos_antes = ( suma_hijos - diferencia_edades )

edad_padre_antes = (suma_hijos_antes)*3


# cuando pasen suma_hijos  años suma total de edades = 150

x150 = (edad_padre + suma_hijos) + (hijo_dos + suma_hijos) + (hijo_uno + suma_hijos)

cat(x150)

# cuantos años tenia el padre cuando nacieron los hijos

rm(list = ls())

#<------------------------------------->
conservas = matrix(
  c(48, 62, 30, 30, 84, 26),
  ncol = 3,
  byrow = TRUE
)

# las ventas totales de la semana son igual a 
# el total de latas vendidas por dia multiplicado por 5 
# mas las ventas del sabado que son la mitad 
ventas_semanales = sum(conservas) * 5 + sum(conservas)/2

colnames(conservas) = c("sardinas", "bonitos", "berberechos")
rownames(conservas) = c("A", "B")

cat("por dia se venden", sum(conservas), "latas")
cat("por semana se venden", ventas_semanales, "latas")

rm(list = ls())

#<------------------------------------->
bandejas = matrix(
  c(40, 160, 80, 120, 120, 120, 150, 80, 80),
  ncol = 3,
  byrow = TRUE
)

colnames(bandejas) = c("fontina", "roquefort", "camembert")
rownames(bandejas) = c("A", "B", "C")

ventas = matrix(
  c(50, 50, 50),
  ncol = 3
)

precios = matrix(
  c(50, 60, 70),
  nrow = 3
)

gramos_vendidos = ventas %*% bandejas

kg_vendidos = gramos_vendidos %/% c(1000, 1000, 1000)

importe_por_queso = precios %*% kg_vendidos
print(precios)
print(kg_vendidos)
print(importe_por_queso)
  
cat("kg de fontina, roquefort y camembert vendidos respectivamente",kg_vendidos)

# <------------------------------------->
familias = matrix(
  c(2,1, 3,1, 1,2),
  byrow = TRUE,
  ncol = 2
)

colnames(familias) = c("dobles", "sencillas")
rownames(familias) = c("A", "B", "C")

hoteles = matrix(
  c(84,45, 86,43, 85,44),
  nrow = 2
)

rownames(hoteles) = c("dobles", "sencillas")
colnames(hoteles) = c("H1", "H2", "H3")

print(familias)
print(hoteles)

gastos_por_dia = familias %*% hoteles
cat("gastos por dia de cada familia")
gastos_por_dia
gastos_10_dias = familias %*% (hoteles*10)
cat("gastos de cada familia si permanecen durante 10 dias")
gastos_10_dias
