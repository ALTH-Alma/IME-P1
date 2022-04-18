#Grupo 5

#Integrantes:
#Alma Vidal
#Mauricio Castro
#Daniel Psijas
#Ramon Parra

#Respuesta de preguntas item 2:
#2.1) Los variables que se cargan son regiones y (número de contagios por) fecha desde el 03/03/2020 hasta el 06/03/2022.
#2.2) El tipo de las variables son: 
# Las regiones: categoricas - nominales.
# Las fechas son numericas - discretas.
#2.3) Las regiones tienen una escala nominal y las fechas una escala de razon.


##Pregunta 1

#Se importan los datos desde un archivo de valores separados por coma en formato espanol
datos <- read.csv2("C:\\Users\\Ramon Parra C\\Desktop\\Universidad\\IME\\EP01 Datos Covid.csv")

#Se importa el paquete dplyr
library(dplyr)

#Se obtiene el vector con los contagios en la región del Maule.
contagiosMaule <- datos %>% filter(Region == "Maule")

#Se otiene indice de las columnas con fecha 01-mar-2021 y el 31-ago-2021 y los contagios
ini = which(colnames(datos) == "X01.03.2021")
fin = which(colnames(datos) == "X31.08.2021")
contagiosIntervalo <- contagiosMaule[c(ini:fin)]
max = max(contagiosIntervalo)
indiceMax = which(contagiosIntervalo == max)
Dia_con_mas_infectados = colnames(contagiosIntervalo[indiceMax])


##Pregunta 2

#Se obttiene el indice de las columnas de cada mes, es decir de marzo, abril, mayo, junio, julio y agosto
iniMarzo = which(colnames(datos) == "X01.03.2021")
finMarzo = which(colnames(datos) == "X31.03.2021")
contagiosIntervaloMarzo <- contagiosMaule[c(iniMarzo:finMarzo)]
#Se suma para obtener el total de contagios de marzo
totalContagiosMarzo = rowSums(contagiosIntervaloMarzo)

#Se obttiene el indice de las columnas de cada mes, es decir de marzo, abril, mayo, junio, julio y agosto
iniAbril = which(colnames(datos) == "X01.04.2021")
finAbril = which(colnames(datos) == "X30.04.2021")
contagiosIntervaloAbril <- contagiosMaule[c(iniAbril:finAbril)]
#Se suma para obtener el total de contagios de marzo
totalContagiosAbril = rowSums(contagiosIntervaloAbril)

#Se obttiene el indice de las columnas de cada mes, es decir de marzo, abril, mayo, junio, julio y agosto
iniMayo = which(colnames(datos) == "X01.05.2021")
finMayo = which(colnames(datos) == "X31.05.2021")
contagiosIntervaloMayo <- contagiosMaule[c(iniMayo:finMayo)]
#Se suma para obtener el total de contagios de marzo
totalContagiosMayo = rowSums(contagiosIntervaloMayo)


#Se obttiene el indice de las columnas de cada mes, es decir de marzo, abril, mayo, junio, julio y agosto
iniJunio = which(colnames(datos) == "X01.06.2021")
finJunio = which(colnames(datos) == "X30.06.2021")
contagiosIntervaloJunio <- contagiosMaule[c(iniJunio:finJunio)]
#Se suma para obtener el total de contagios de marzo
totalContagiosJunio = rowSums(contagiosIntervaloJunio)

#Se obttiene el indice de las columnas de cada mes, es decir de marzo, abril, mayo, junio, julio y agosto
iniJulio = which(colnames(datos) == "X01.07.2021")
finJulio = which(colnames(datos) == "X31.07.2021")
contagiosIntervaloJulio <- contagiosMaule[c(iniJulio:finJulio)]
#Se suma para obtener el total de contagios de marzo
totalContagiosJulio = rowSums(contagiosIntervaloJulio)

#Se obttiene el indice de las columnas de cada mes, es decir de marzo, abril, mayo, junio, julio y agosto
iniAgosto = which(colnames(datos) == "X01.08.2021")
finAgosto = which(colnames(datos) == "X31.08.2021")
contagiosIntervaloAgosto <- contagiosMaule[c(iniAgosto:finAgosto)]
#Se suma para obtener el total de contagios de marzo
totalContagiosAgosto = rowSums(contagiosIntervaloAgosto)
