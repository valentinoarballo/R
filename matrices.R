acciones_vendidas = c(200, 300, 500, 300)
precios = matrix(c(20, 30, 45, 100), nrow = 4)

resultado = acciones_vendidas %*% precios 
resultado
# 200 tipo A, 300 tipo B, 500 tipo C, 300 D
#$20, $30, $45 y $100
rm(list = ls())

