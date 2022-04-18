#PREGUNTA DE GRUPO: 
#¿Son similares los ingresos registrados en las diferentes provincias de la RM?

#Para este trabajo las variables relevantes en el archivo csv. es ytot - variable numérica 
#discrita que representa los ingresos totales (no se especifica unidad); y provincia
#provincia - variable categórica nominal.

#La mejor medida para determinar similitudes en los ingresos es el coef. de variación,
#para ello se trabaja con la desviación estándar de las medianas de los ingresos en 
#las provincias. Se pudo trabajar con la media pero los datos presentan valores 
#atípicos lo que no es conveniente para el promedio.

#De esta misma forma, la gráfica que nos ayudaría a visualizar similitudes entre los ingresos
#de provincias sería el de cajas por grupo, pero por los valores atípicos se trabaja con un 
#gráfico de barras. 

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

#Luego se obtiene la desviación estandar de las medianas de provincias
desviacion <- sd(datos_agrupados[["mediana_ingreso"]])

#Se obtiene el coef de variación para determinar la similitud en los 
#ingresos de las provincias
promedioTotal <- mean(datos_agrupados[["mediana_ingreso"]])
Cv <- desviacion/promedioTotal

cat("Se puede concluir que los ingresos registrados en las diferentes provincias son 
    similares, pues el coef. de variación de las medianas es: ")
print(Cv)


#Se muestra la gráfica seleccionada
g <- ggbarplot(datos_agrupados, 
               x="provincia",
               y="mediana_ingreso",
               title = "Mediana de Ingresos por Provincia",
               xlab = "Provincias",
               ylab = "Ingresos")
print(g)
cat("En el gráfico se puede ver y confirmar la similitud de las medianas
    de ingreso en cada provincia, solo es la RM la que presenta menos
    similitud con las demás.")

cat("En síntesis, tanto estadísticos como gráfica demuestran similud entre los ingresos
    de las provincias.")