#PREGUNTA DE GRUPO: 
#�Son similares los ingresos registrados en las diferentes provincias de la RM?

#Para este trabajo las variables relevantes en el archivo csv. es ytot - variable num�rica 
#discrita que representa los ingresos totales (no se especifica unidad); y provincia
#provincia - variable categ�rica nominal.

#La mejor medida para determinar similitudes en los ingresos es el coef. de variaci�n,
#para ello se trabaja con la desviaci�n est�ndar de las medianas de los ingresos en 
#las provincias. Se pudo trabajar con la media pero los datos presentan valores 
#at�picos lo que no es conveniente para el promedio.

#De esta misma forma, la gr�fica que nos ayudar�a a visualizar similitudes entre los ingresos
#de provincias ser�a el de cajas por grupo, pero por los valores at�picos se trabaja con un 
#gr�fico de barras. 

#Cargar librerias
if(!require(dplyr)){
  install.packages("dplyr",dependencies=TRUE)
  require(dplyr)
  
}

if(!require(ggpubr)){
  install.packages("ggpubr",dependencies=TRUE)
  require(ggpubr)
  
}

#Cargar datos de contagios
datos <- read.csv2("D:\\Datos de prueba RStudio\\EP02 Datos Casen 2017.csv",stringsAsFactors = FALSE)
                   
#Se sacan las medianas de ingreso de todas las provincias
datos_agrupados <- datos %>%
  group_by(provincia) %>%
  summarize(mediana_ingreso=median(ytot))

#Luego se obtiene la desviaci�n estandar de las medianas de provincias
desviacion <- sd(datos_agrupados[["mediana_ingreso"]])

#Se obtiene el coef de variaci�n para determinar la similitud en los 
#ingresos de las provincias
promedioTotal <- mean(datos_agrupados[["mediana_ingreso"]])
Cv <- desviacion/promedioTotal

cat("Se puede concluir que los ingresos registrados en las diferentes provincias son 
    similares, pues el coef. de variaci�n de las medianas es: ")
print(Cv)


#Se muestra la gr�fica seleccionada
g <- ggbarplot(datos_agrupados, 
               x="provincia",
               y="mediana_ingreso",
               title = "Mediana de Ingresos por Provincia",
               xlab = "Provincias",
               ylab = "Ingresos")
print(g)
cat("En el gr�fico se puede ver y confirmar la similitud de las medianas
    de ingreso en cada provincia, solo es la RM la que presenta menos
    similitud con las dem�s.")

cat("En s�ntesis, tanto estad�sticos como gr�fica demuestran similud entre los ingresos
    de las provincias.")