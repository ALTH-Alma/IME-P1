#install.packages("tidyverse", dependencies = TRUE)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(pwr)


#ACTIVIDAD 05:
#Con los siguientes datos:
#Se sabe que una máquina que envasa detergentes industriales llena bidones con un
#volumen de producto que sigue una distribución normal con desviación estándar de
#1 litro. Usando una muestra aleatoria de 100 botellas, el ingeniero a cargo de la
#planta requiere determinar si la máquina está llenando los bidones con una media 
#de 10 litros.
ds <- 1
n <- 100
u0 <- 10

#RESPONDER:-----------------------------------------------------------------------------
#1.- Si el ingeniero está seguro de que el verdadero volumen medio no puede ser inferior
#a 10 litros y piensa rechazar la hipótesis nula cuando la muestra presente una media 
#mayor a 10,5 litros, ¿cuál es la probabilidad de que cometa un error de tipo I?

#La probabilidad de cometer un error de tipo I corresponde al nivel de significación, 
#el que se puede obtener al calcular área de la región de rechazo de la distribución
#de las medias muestrales según la hipótesis nula. Por lo tanto, basta con obtener el
#área de la región ya mencionada, tomando como valor inicial del área de rechazo 10,5.
valor_rechazo <- 10.5

#Se calcula el error estándar:
error_s <- ds/sqrt(n)

#Se calcula alfa considerando que el área de la región de rechazo solo toma la cola
#derecha de la distribución, pues es una prueba unilateral. 
alfa <- pnorm(valor_rechazo, mean = u0, sd = error_s, lower.tail = FALSE)
cat("La probabilidad de cometer un error de tipo I es ", alfa, "\n")

#-------------------------------------------------------------------------------------
#2.- Si el verdadero volumen medio de los bidones fuera de 10,3 litros, 
#¿cuál sería la probabilidad de que el ingeniero, que obviamente no 
#conoce este dato, cometa un error de tipo II?

#El error de tipo II implica no rechazar la hipótesis nula cuando esta es incorrecta, 
#es decir, no rechazar que "H0: El volumen medio de los bidones es 10", cuando en 
#realidad el volumen medio es 10.3.
m1 <- 10.3

#La probabilidad de cometer un error de tipo 2 (beta), se puede obtener al calcular 
#el área de la distribución de la muestra según la hipótesis nula con la verdadera 
#media, sin considerar su región de rechazo.
beta <- pnorm(valor_rechazo, mean = m1, sd = error_s, lower.tail = TRUE)
cat("La probabilidad de cometer un error tipo II es ", beta, "\n")

#--------------------------------------------------------------------------------------
#3.- Como no se conoce el verdadero volumen medio, genere un gráfico del poder estadístico 
#con las condiciones anteriores, pero suponiendo que el verdadero volumen medio podría 
#variar de 10 a 10,7 litros.

#Se generan vector dentro del rango dado para el efecto de medias.

efecto <- seq(10, 10.7, 0.001) - 10
print(efecto)
#Se calcula el poder para la prueba unilateral, con las condiciones
#antes obtenidas:
poder <- power.t.test(n = 100,
                       delta = efecto, 
                       sd = error_s,
                       sig.level = alfa,   
                       type = "one.sample",
                       alternative = "one.sided")$power 
print(poder)
datos <- data.frame(efecto, poder)
g <- plot(datos, aes(efecto), type = "l")
g <- g + title("Poder estadístico")
print(g)

#--------------------------------------------------------------------------------------
#4.- Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían revisarse
#para conseguir un poder estadístico de 0,8 y un nivel de significación de 0,05?

#Se generan los datos usando la función pwr.t.test y los datos entregados.
#Para ello se calcula el d de Cohen:
d <- (10.3 - 10)/error_s # 10.3 media muestral y 10 nulo
poder1 <- pwr.t.test(n = NULL,
                     d = d,
                     sig.level = 0.05,
                     power = 0.8,
                     type = "one.sample",
                     alternative = "greater")

tamano <- ceiling(poder1[["n"]])
cat("Para conseguir un poder estadístico de 0,8 y un nivel de significación de 0,05
    deberian revisarse ", tamano, " bidones. \n")

#----------------------------------------------------------------------------------------
#5.- ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer 
#un error de tipo I a un 1% solamente?

#Se repite lo anterior con un nivel de significación de 0.01.
poder1 <- pwr.t.test(n = NULL,
                     d = d,
                     sig.level = 0.01,
                     power = 0.8,
                     type = "one.sample",
                     alternative = "greater")

tamano <- ceiling(poder1[["n"]])
cat("Para conseguir un poder estadístico de 0,8 y un nivel de significación de 0,01
    deberian revisarse ", tamano, " bidones. \n")
