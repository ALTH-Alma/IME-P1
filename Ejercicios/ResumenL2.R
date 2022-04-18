#EXPLORACION DE DATOS:

#Estadisticas variables datos numericos:
#media (mean) = sum(x_i)/n --> medir tendencia central junto con la "mediana (median)"(valor centrico)
#desciavion muestral(sd)= s = raiz(sum(x_i - media)¨2/n-1) -->medir desviacion de las observaciones
#varianza muestral (var)= s¨2 -->medir variabilidad o dispersion 
#Rango = max(), min(), range()
#sapply(): aplica una misma funcion a multiples columnas.

# Cargar conjunto de datos .
 datos <- rea.csv2 ("C:/ Inferencia / Mtcars .csv", stringsAsFactors = TRUE ,
                         row.names = 1)

 # Calcular la media para la variable Rendimiento .
 media <- mean ( datos [[" Rendimiento "]])
 cat (" Rendimiento medio :", media , "\n\n")

# Calcular la media para la tercera y quinta columnas
 # ( variables Desplazamiento y Eje).
 cat (" Medias \n")
 print ( sapply ( datos [c(3 , 5) ] , mean ) )
cat ("\n")

 # Calcular la media para las columnas 3 a 6
 # ( variables Desplazamiento , Potencia , Eje y Peso ).
 cat (" Medias \n")
 print ( sapply ( datos [3:6] , mean ))
 cat ("\n")

 # Calcular la media para la variable Rendimiento omitiendo valores faltantes .
 print ( mean ( datos [[" Rendimiento "]] , na.rm = TRUE ) )
 
 
#cuartiles:
#Percentiles: dividen el conjunto de datos en 100 subconjuntos de igual tamaño.
#Deciles: dividen el conjunto de datos en 10 subconjuntos de igual tamaño.
#Quintiles: dividen el conjunto de datos en 5 subconjuntos de igual tamaño.
#Cuartiles: dividen el conjunto de datos en 4 subconjuntos de igual tamaño.
 
 # Cá lculo de percentiles para la variable Rendimiento .
cat (" Cuartiles :\n")
print ( quantile ( datos [[" Rendimiento " ]]) )
 cat ("\n")

cat (" Quintiles :\n")
 print ( quantile ( datos [[" Rendimiento "]] , seq (0 , 1 , 0.2) ) )
 cat ("\n")

 cat (" Deciles :\n")
print ( quantile ( datos [[" Rendimiento "]] , seq (0 , 1 , 0.1) ) )
cat ("\n")

 cat (" Percentiles :\n")
 print ( quantile ( datos [[" Rendimiento "]] , seq (0 , 1 , 0.01) ) )
 
 
#IQR(): rango intercuartil = Q_3-Q_1 --> a mayor valor --> mas disperso el conjunto de datos.
 
 
#Calculo de diferentes medidas al mismo tiempo:

 library ( dplyr )
#Cargar datos
 # Cá lculo de varias medidas para la variable Potencia .
 medidas_potencia <- datos %> % summarise ( Media = mean ( Potencia ) ,
                                                Mediana = median ( Potencia ) ,
                                                 Varianza = var( Potencia ) ,
                                                IQR = IQR( Potencia ) )

 print ( medidas_potencia )
 cat ("\n")

 # Cá lculo de la media y la desviaci ón está ndar para las variables Peso y
 # Cuarto _ milla .
  medidas_varias <- datos %> % summarise ( Media_P = mean ( Peso ) ,
                                              Media_C = median ( Cuarto_milla ) ,
                                               SD_P = sd( Peso ) ,
                                                SD_C = sd( Cuarto_milla ) )
 print ( medidas_varias )
 cat ("\n")
 
 
 #Datos categoricos para 1 variable: Tablas de contigencias, matriz de confusion o tabla de frecuencias 
 
 # Cargar datos .
 # Crear tabla de contingencia para la variable gear .
  contingencia <- table ( datos [[" Cambios "]])
  cat (" Tabla de contingencia generada con table () :\n")
  print ( contingencia )
  cat ("\n")

 # Otra forma de crear la misma tabla .
 contingencia <- xtabs (~ Cambios , data = datos )
 cat (" Tabla de contingencia generada con xtabs () :\n")
 print ( contingencia )
 cat ("\n")

 # Calcular totales por fila y mostrarlos por separado .
 totales <- marginSums ( contingencia )
 cat (" Totales por fila :\n")
 print ( totales )
 cat ("\n")

# Calcular totales por fila y agregarlos a la tabla .
 con_totales <- addmargins ( contingencia , 1)
 cat (" Tabla de contingencia con totales por fila :\n")
 print ( con_totales )
 cat ("\n")

 # Convertir a tabla de proporciones
 proporciones <- prop.table ( contingencia )
 proporciones <- addmargins ( proporciones , 1)
 cat (" Tabla de contingencia con proporciones :\n")
 print ( proporciones )
 cat ("\n")

 # Convertir a tabla de porcentajes con 2 decimales .
 porcentajes <- round ( prop.table ( contingencia ) , 4) * 100
 porcentajes <- addmargins ( porcentajes )
 cat (" Tabla de contingencia con porcentajes :\n")
  print ( porcentajes )
 cat ("\n")
 
 
 #Datos categoricos para 2 variables: 
 
 # Crear tabla de contingencia para las variables Transmision y gear .
  contingencia <- table ( datos [[" Transmision "]] , datos [[" Cambios "]])
  cat (" Tabla de contingencia generada con table () :\n")
  print ( contingencia )
  cat ("\n")

 # Otra forma de crear la misma tabla .
 contingencia <- xtabs (~ Transmision + Cambios , data = datos )
 cat (" Tabla de contingencia generada con xtabs () :\n")
 print ( contingencia )
 cat ("\n")
 # Proporciones con totales por fila .
 proporciones_fila <- prop.table ( contingencia , margin =1)
 proporciones_fila <- addmargins ( proporciones_fila , margin =2)
 cat (" Tabla de contingencia con proporciones totales por fila :\n")
  print ( proporciones_fila )
 cat ("\n")

 # Proporciones con totales por columna .
 proporciones_columna <- prop.table ( contingencia , margin =2)
 proporciones_columna <- addmargins ( proporciones_columna , margin =1)
 cat (" Tabla de contingencia con proporciones totales por columna :\n")
 print ( proporciones_columna )
 cat ("\n")

 # Proporciones con totales .
 proporciones <- prop.table ( contingencia )
 proporciones <- addmargins ( proporciones )
 cat (" Tabla de contingencia con proporciones totales :\n")
 print ( proporciones )
  cat ("\n")
 
 
 #Datos categoricos para 3 variables: 
  
  # Convertir la variable Cambios en categ ó rica .
 datos [[" Cambios "]] <- factor ( datos [[" Cambios "]])
 
   # Crear tabla de contingencia para las variables Transmision ,
   # Cambios y Motor .
   contingencia <- ftable ( datos [[" Transmision "]] , datos [[" Cambios "]] ,
                               datos [[" Motor " ]])
  
   cat (" Tabla de contingencia generada con ftable () :\n")
   print ( contingencia )
   cat ("\n")
  
   # Otra forma de crear la misma tabla .
   xtabs (~ Cambios + Transmision + Motor , data = datos )
   cat (" Tabla de contingencia generada con xtabs () :\n")
   print ( contingencia )
   cat ("\n")
   
  #Para datos agrupados:
   
   library ( dplyr )

# Cargar datos .


resumen <- group_by( datos , Cambios ) %> %
  summarise ( count = n () , mean ( Rendimiento ) , median ( Rendimiento ) ,
                   sd( Rendimiento ) , IQR ( Rendimiento ) , mean ( Potencia ) )

 print ( resumen )
   
  
 