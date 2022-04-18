require(pwr)
require(Hmisc)
# 1.Estudios previos habían determinado que la proporción de autoras en la especialidad de medicina interna era de 42%. ¿Respaldan estos datos tal estimación?


# Autoras = 45 > 10
# Autores = 65 > 10
# Por lo tanto se cumple la condición de éxito fracaso y son
# independientes porque son mutuamente excluyentes.


# Ho: La proporción de autoras de la especialidad de medicina
# interna es de 0,42

# Ha: La proporción de autoras de la especialidad de medicina
# es distinta de 0,42

exitos = 45
n = 45 + 55
alfa = 0.05
prueba <- prop.test(x = exitos,
                    n = n,
                    p =0.42,
                    alternative = "two.sided",
                    conf.level = alfa)

pvalor <- prueba$p.value

# p valor = 0.612 > alfa, por lo tanto se falla en rechazar la hipótesis
# nula y los datos respaldan esta afirmación.

#2. Según estos datos, ¿es igual la proporción de autoras en las áreas de obstetricia y radiología?

# Calculando proporción agrupada 



exitos1 <- 71 # éxitos obstetricia
exitos2 <- 17 # éxitos radiología
n1 <- 71 + 66
n2 <- 17 + 35
p1 <- exitos1/n1
p2 <- exitos2/n2

# Calculando proporción agrupada para determinar
# la condición de éxito fracaso, siendo ésta
# una prueba con valor nulo = 0
pagrupada <- (p1*n1 + p2*n2)/(n1+n2)

# Muestra 1
pn1 <- pagrupada*n1
qn1 <- (1-pagrupada)*n1

# Muestra 2
pn2 <- pagrupada*n2
qn2 <- (1-pagrupada)*n2

# p agrupada = 0.465
# p*n1 = 63, q*n1 = 73
# p*n2 = 24, q*n2 = 27

# Todos son mayor a 10, por lo tanto, se cumplen las condiciones 
# de éxito-fracaso en ambas muestras. Además hombres y mujeres son
# mutuamente excluyentes, cumpliendo la condición de normalidad en
# las muestras. Ambas muestras son de especialidades diferentes 
# por lo tanto, son independientes.
# Entonces, se cumplen todas las condiciones para la prueba.

# Ho: La diferencia de la proporción de autoras de obstetricia
# y radiología es 0

# Ha: La diferencia de la proporción de autoras de obstetrucia
# y radiología es distinto de 0 


# Prueba para diferencia de proporciones para dos muestras
# con valor nulo por defecto de 0, y bilateral.
prueba2 <- prop.test(x = c(exitos1,exitos2),
                     n = c(n1,n2),
                     alternative = "two.sided",
                     conf.level = alfa)

pvalue = prueba2$p.value

# p-valor = 0.028 < 0.05 por lo tanto se rechaza Ho en favor de Ha,
# entonces hay suficiente evidencia para afirmar que la proporción
# de autoras en obstetricia y dermatología no es la misma.


# 3.Suponiendo que la diferencia en la proporción de autoras en la especialidad de anestesiología y la de pediatría es de 0,28. ¿A cuántos autores deberíamos monitorear para obtener un intervalo de confianza del 95% y poder estadístico de 80%, si se intenta mantener aproximadamente la misma proporción de gente estudiada en cada caso?



nulo <- 0.28 # nulo
confianza <- 0.95 
alfa <- 1-confianza # significación a partir de la confianza
p1 = 54/(54+52) # proporcion anestesiología
p2 = 21/(21+40) # proporción radiología
poder <- 0.8


# Aplicando bsamsize para sacar las cantidades que hay que
# revisar para obetener los resultados pedidos.
n <- bsamsize(p1,p2,nulo,alfa,poder)

sizes = round(n)

#Por lo tanto, según el par sizes la cantidad de autores a monitoriar para obtener dicho intervalo de confianza y poder,
# es de 96 para anestesiología y 246 para radiología.