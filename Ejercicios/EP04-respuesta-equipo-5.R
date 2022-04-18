require(dplyr)
require(ggpubr)

#1.El Comité Olímpico cree que el mejor tiempo medio de 
#  los atletas blancos después de ingresar al programa de 
#  entrenamiento es inferior a 12,87 segundos. 
#  ¿Soportan los datos esta afirmación?

# Signficiación de 0,01 para todas las pruebas. Ya que
# no es tan grave cometer errores tipo I al plantear conclusiones
# sobre el rendimiento de los atletas.
alfa <- 0.01

#IMPORTANTE: Cambiar ruta a la ruta del archivo
datos <- read.csv2("Direccion/EP04-datos.csv",stringsAsFactors = FALSE)



blancos <- datos %>% filter(Raza == "Blanca")

# Ho: El promedio de los mejores tiempos de blancos es mayor
# o igual a 12,87

# Ha: El promedio de los mejores tiempos de blancos es 
# inferior a 12,87

nulo <- 12.87

promedioBlancos <- mean(blancos[["Posterior"]])

# p-valor de la prueba de shapiro 

p_normal <- c(shapiro.test(blancos[["Posterior"]]), shapiro.test(negros[["Previo"]]),shapiro.test(blancos[["Previo"]]),shapiro.test(orientales[["Previo"]]),shapiro.test(orientales[["Posterior"]]))

for(i in p_normal){
  print(i$p.value)
}

# Viendo los p-valores de el vector anterior,
# todos son mayor a 0,5. Entonces se asume que la población
# en todas las muestras se distribuye normal.

# Graficando Q-Q para cada conjunto de datos que se usará, y
# ver visualmente el supuesto de normalidad.

print(ggqqplot(blancos[["Posterior"]]))
print(ggqqplot(negros[["Previo"]]))
print(ggqqplot(blancos[["Previo"]]))
print(ggqqplot(orientales[["Previo"]]))
print(ggqqplot(orientales[["Posterior"]]))


# Viendo cada gráfico, se puede ver que cada punto está dentro del
# margen, a pesar de que hay unos que están en el borde de salir,
# reconocemos que son datos atípicos por estar alejados del 
# resto del conjunto de datos. Se dejan porque su ausencia podría
# llevar a resultados erróneos.



# Los datos de la muestra se tomaron de forma independiente entre 
# sí y asumimos que es menos del 10% de la población.
# Como la muestra tiene pocos datos se puede hacer t-student
# para todas las pruebas.

# Desviación estándar 
s <- sd(blancos[["Posterior"]])

# Tamaño de la muestra
n <- length(blancos[["Posterior"]])

# Error estándar
errorEstandar <- s/sqrt(n)

# Estadístico de prueba
T <- (promedioBlancos - nulo)/errorEstandar

#p-valor unilateral
p <- pt(T,df=(n-1),lower.tail = TRUE)

# Sacando la conclusión a partir del p-valor
if(p < alfa){
  print("1. Se rechaza Ho a favor de Ha, por lo que hay evidencia para afirmar que el promedio de los mejores tiempos de blancos es inferior a 12,87")
}
if(p >= alfa){
  print("1. Falla en rechazar Ho, por lo que no hay evidencia para afirmar que el promedio de los mejores tiempos de blancos es inferior a 12,87")
}


##################################################################
############################################################

#2. ¿Sugieren los datos que la mejor marca de los atletas 
# orientales se reduce en más de 5,27 segundos tras el entrenamiento?
orientales <- datos %>% filter(Raza == "Oriental")

diferencias <- orientales[["Previo"]] - orientales[["Posterior"]]


# Ho: El promedio de las diferencias es menor o igual a 5,27
# Ha: El promedio de las diferencias es mayor a 5,27

n <- length(diferencias)
# Valor nulo
nulo <- 5.27 

# Como son muestras pareadas, entonces se usa el promedio
# de las diferencias.
promedioDif <- mean(diferencias)

# Desviación estándar de las diferencias.
s <- sd(diferencias)

# Error estándar del estimador.
errorEstandar <- s/sqrt(n)

# Estadístico de prueba.
T <- (promedioDif - nulo)/errorEstandar

# p-valor unilateral
p <- pt(T,df=(n-1),lower.tail = FALSE)


if(p < alfa){
  print("2.Se rechaza Ho a favor de Ha, por lo que hay evidencia para afirmar que el promedio no se reduce en 5,27 segundos")
}
if(p >= alfa){
  print("2.Falla en rechazar Ho,por lo que hay evidencia para afirmar que el promedio se reduce después de entrenar en 5.27 segundos")
}


#########################################################
#########################################################

#3.¿Es posible afirmar que, en promedio, los atletas negros 
# superan a los blancos por 0,76 segundos antes del entrenamiento?

negros <- datos %>% filter(Raza == "Negra")

promedioNegros <- mean(negros[["Previo"]])
promedioBlancos <- mean(blancos[["Previo"]])

nulo = 0.76

# Como son muestras independientes, entonces se usa la 
# diferencia de medias (paired = FALSE en t.test).


# Ho: La diferencia de las medias antes del entrenamiento
# entre los negros y blancos es igual a 0,76 segundos.

# Ha: La diferencia de medias antes del entrenamiento entre
# los negros y blancos es distinto de 0,76 segundos.

prueba<- t.test( x = negros[["Previo"]],
                 y = blancos[["Previo"]],
                 paired = FALSE,
                 alternative = "two.sided", # bilateral
                 mu = nulo,
                 conf.level = 1-alfa
                 
)
p <- prueba[["p.value"]]

if(p < alfa){
  print("3. Se rechaza Ho a favor de Ha,por lo que hay evidencia a favor de que no hay una diferencia de 0,76 segundos entre los tiempos promedios de los blancos y negros antes de entrenar.")
}
if(p >= alfa){
  print("3. Falla en rechazar Ho, por lo que hay evidencia a favor de que hay una diferencia de 0,76 segundos entre los tiempos promedios de los blancos y negros antes de entrenar.")
}
