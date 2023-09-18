# Simplex
library(boot)


# -----------------------------1

#   1. Max 
#   40*X1 + 60*X2
#   2*X1 + 1*X2 <= 70
#   1*X1 + 1*X2 <= 40
#   1*X1 + 3*X2 <= 90
#   X1 >= 0, X2 >= 0

a<-c(40,60)
A1<-rbind(c(2,1),c(1,1),c(1,3))
b1<-c(70,40,90)

simplex(a,A1,b1,maxi="TRUE")

# -----------------------------2

#   Z = f(x,y) = 3x + 2y
#   2x + y ≤ 18
#   2x + 3y ≤ 42
#   3x + y ≤ 24
#   x ≥ 0, y ≥ 0

a <- c(3,2)
A1 <- rbind(c(2,1), c(2,3), c(3,1))
b1 <- c(18, 42, 24)

simplex(a, A1, b1, maxi="TRUE")


#-----------------3


# Formulación de dieta: Una dieta debe contener al menos 16 unidades de
# carbohidratos y 20 de proteínas. El alimento A contiene 2 unidades de
# carbohidratos y 4 de proteínas; el alimento B contiene 2 unidades de
# carbohidratos y 1 de proteínas. Sí el alimento A cuesta $1,20 por unidad y el B
# $0,80 por unidad. ¿cuántas unidades de cada alimento deben compararse para
# minimizar el costo? ¿Cuál es el costo mínimo?

A1 = c(0, 0)
b1 = c(0)
a <- c(1.2, 0.8)
A2 <- rbind(c(2, 2),  c(4,1))
b2 <- c(16, 20)

simplex(a, A1, b1, A2, b2, maxi="FALSE")


#-----------------4 

a <- c(50, 60)
A1 = c(0, 0)
b1 = c(0)

A2 <- rbind(c(100, 200),  c(200,50))
b2 <- c(3000, 2500)

resultadoAc4 <- simplex(a, A1, b1, A2, b2, maxi="FALSE")

sprintf("se tienen que procesar...")
sprintf("mina 1: %s | mina 2: %s", resultadoAc4$soln[1], resultadoAc4$soln[2])
sprintf("costo minimo: %s", resultadoAc4$value)

#----------------- 5

a <- c(600000, 300000)
A1 = c(0, 0)
b1 = c(0)
A2 <- rbind (c(10, 4), c(20, 30))
b2 <- c(100, 420)

resultadoAc5 <- simplex(a, A1, b1, A2, b2, maxi="FALSE")

sprintf("para minimizar el costo de construcción...")
sprintf("Camara A: %s | Camara B: %s", resultadoAc5$soln[1], resultadoAc5$soln[2])

#----------------- 6

a <- c(0.75, 0.5)
A1 = rbind(c(3,5), c(2,1))
b1 = c(36800, 2400)

resultadoAc6 <- simplex(a, A1, b1, maxi="TRUE")

sprintf("para maximisar los ingresos...")
sprintf("Cajas a transportar de A: %s | Cajas a transportar de B: %s", resultadoAc6$soln[1], resultadoAc6$soln[2])
sprintf("la ganancia maxima: %s", resultadoAc6$value)

#----------------- 7

b1 = c(400, 500, 1450)
A1 = rbind(c(1,1,1),c(1,1,2),c(2,3,5))
a = c(21,24,36)

resultadoAc7 <- simplex(a, A1, b1, maxi="TRUE")

print(resultadoAc7)

#----------------- 8

# horasdisponibles = 10
# horasdeestudio = 4

# horasdejuego =< horasdeestudio
# horasdeestudio + horasdejuego =< horasdisp10onibles

# ???????????????? 

#----------------- 9

a = c(6, 7.5)
A1 = rbind(c(1,0),c(0,1))
b1 = c(200,45)
A2 = c(1,0)
b2 = c(150)

resultadoAc9 <- simplex(a, A1, b1, A2, b2, maxi="TRUE")

sprintf("Nivel optimo...")
sprintf("producto 1: %s | producto 2: %s", resultadoAc9$soln[1], resultadoAc9$soln[2])
sprintf("Horas extra: %s", ((resultadoAc9$soln[1] * 10) + (resultadoAc9$soln[2] * 12)- 2500))

#----------------- 10

a = c(5,7)
A1 = c(1,1)
b1 = (500)
A2 = rbind(c(1,0), c(-2,1))
b2 = c(100,0)

resultadoAc10 <- simplex(a, A1, b1, A2, b2, maxi="TRUE")

sprintf("cantidad  de latas que debe tener la tienda...")
sprintf("latas A: %s | latas B&K: %s", resultadoAc10$soln[1], resultadoAc10$soln[2])

#----------------- 11

a = c(40,24,36,23)
A1= rbind(c(2,1,2.5,5), c(1,3,2.5,0),c(10,5,2,12),c(1,0,0,0),c(0,0,1,0))
b1=c(120,160,1000,20,16)
A2=c(0,0,0,1)
b2=c(10)

resultadoAc11 <- simplex(a, A1, b1, A2, b2, maxi="TRUE")

sprintf("Beneficio maximo: %s", resultadoAc11$value)
