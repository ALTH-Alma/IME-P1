#install.packages("tidyverse", dependencies = TRUE)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(pwr)


#ACTIVIDAD 05:
#Con los siguientes datos:
#Se sabe que una m�quina que envasa detergentes industriales llena bidones con un
#volumen de producto que sigue una distribuci�n normal con desviaci�n est�ndar de
#1 litro. Usando una muestra aleatoria de 100 botellas, el ingeniero a cargo de la
#planta requiere determinar si la m�quina est� llenando los bidones con una media 
#de 10 litros.
ds <- 1
n <- 100
u0 <- 10

#RESPONDER:-----------------------------------------------------------------------------
#1.- Si el ingeniero est� seguro de que el verdadero volumen medio no puede ser inferior
#a 10 litros y piensa rechazar la hip�tesis nula cuando la muestra presente una media 
#mayor a 10,5 litros, �cu�l es la probabilidad de que cometa un error de tipo I?

#La probabilidad de cometer un error de tipo I corresponde al nivel de significaci�n, 
#el que se puede obtener al calcular �rea de la regi�n de rechazo de la distribuci�n
#de las medias muestrales seg�n la hip�tesis nula. Por lo tanto, basta con obtener el
#�rea de la regi�n ya mencionada, tomando como valor inicial del �rea de rechazo 10,5.
valor_rechazo <- 10.5

#Se calcula el error est�ndar:
error_s <- ds/sqrt(n)

#Se calcula alfa considerando que el �rea de la regi�n de rechazo solo toma la cola
#derecha de la distribuci�n, pues es una prueba unilateral. 
alfa <- pnorm(valor_rechazo, mean = u0, sd = error_s, lower.tail = FALSE)
cat("La probabilidad de cometer un error de tipo I es ", alfa, "\n")

#-------------------------------------------------------------------------------------
#2.- Si el verdadero volumen medio de los bidones fuera de 10,3 litros, 
#�cu�l ser�a la probabilidad de que el ingeniero, que obviamente no 
#conoce este dato, cometa un error de tipo II?

#El error de tipo II implica no rechazar la hip�tesis nula cuando esta es incorrecta, 
#es decir, no rechazar que "H0: El volumen medio de los bidones es 10", cuando en 
#realidad el volumen medio es 10.3.
m1 <- 10.3

#La probabilidad de cometer un error de tipo 2 (beta), se puede obtener al calcular 
#el �rea de la distribuci�n de la muestra seg�n la hip�tesis nula con la verdadera 
#media, sin considerar su regi�n de rechazo.
beta <- pnorm(valor_rechazo, mean = m1, sd = error_s, lower.tail = TRUE)
cat("La probabilidad de cometer un error tipo II es ", beta, "\n")

#--------------------------------------------------------------------------------------
#3.- Como no se conoce el verdadero volumen medio, genere un gr�fico del poder estad�stico 
#con las condiciones anteriores, pero suponiendo que el verdadero volumen medio podr�a 
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
g <- g + title("Poder estad�stico")
print(g)

#--------------------------------------------------------------------------------------
#4.- Considerando un volumen medio de 10 litros, �cu�ntos bidones deber�an revisarse
#para conseguir un poder estad�stico de 0,8 y un nivel de significaci�n de 0,05?

#Se generan los datos usando la funci�n pwr.t.test y los datos entregados.
#Para ello se calcula el d de Cohen:
d <- (10.3 - 10)/error_s # 10.3 media muestral y 10 nulo
poder1 <- pwr.t.test(n = NULL,
                     d = d,
                     sig.level = 0.05,
                     power = 0.8,
                     type = "one.sample",
                     alternative = "greater")

tamano <- ceiling(poder1[["n"]])
cat("Para conseguir un poder estad�stico de 0,8 y un nivel de significaci�n de 0,05
    deberian revisarse ", tamano, " bidones. \n")

#----------------------------------------------------------------------------------------
#5.- �Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer 
#un error de tipo I a un 1% solamente?

#Se repite lo anterior con un nivel de significaci�n de 0.01.
poder1 <- pwr.t.test(n = NULL,
                     d = d,
                     sig.level = 0.01,
                     power = 0.8,
                     type = "one.sample",
                     alternative = "greater")

tamano <- ceiling(poder1[["n"]])
cat("Para conseguir un poder estad�stico de 0,8 y un nivel de significaci�n de 0,01
    deberian revisarse ", tamano, " bidones. \n")
