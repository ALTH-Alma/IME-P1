#1 BASICOS
#Variable:
#- Numericas: continuas, discretas.
#- Categoricas: nominales, ordinales(tiene orden).
#Escalas: 
#-Categoricas: Nominal, ordinal o rango
#- Numericas: intervalo(T), razon(cero verdadero).

#Variables dependientes y independientes:
#- asociacion negativa o positiva.

#Parametro (poblacion): numero que describe una poblacion.
#Estadistico (muestra): estimador puntual de un parametro.


#Lectura de archivos:

# Cargar un conjunto de datos disponible en R.
datos1 <- mtcars

# Importar desde un archivo de texto plano delimitado por tabuladores .
datos2 <- read.delim ( file.choose () )

# Importar desde un archivo de valores separados por coma
 # en formato ingl és ( figura 1.2 b).
 datos3 <- read.csv("C:\\ Inferencia \\ ejemplo1 -csv -eng.csv ")

 # Configurar carpeta de trabajo
 setwd ("C:\\ Inferencia ")

# Importar desde un archivo de valores separados por coma
# en formato espa ñol ( figura 1.2 c).
datos4 <- read.csv2 (" ejemplo1 -csv -esp. csv")

# Mostrar las primeras 6 filas del conjunto de datos
# almacenado en datos1 .
head( datos1 )

# Mostrar las ú ltimas 6 filas del conjunto de datos
# almacenado en datos1 .
tail( datos1 )


# Importar un paquete , instal á ndolo de ser necesario .
if (! require ( dplyr ) ) {
  install.packages (" dplyr ", dependencies = TRUE )
  require ( dplyr )
 }


#Cosntruccion de dataframe.

 # Crear un vector de strings y guardarlo en la variable nombre .
 nombre <- c(" Alan Brito Delgado ",
               " Zacar ías Labarca del Río",
               " Elsa Payo Maduro ")

 # Crear un vector de fechas y guardarlo en la variable
 # fecha _ nacimiento .
 fecha_nacimiento <- as.Date (c(" 2008 -1 -25", "2006 -10 -4", "2008 -3 -27 ") )

 # Crear tres vectores de reales entre 1.0 y 7.0 y guardarlos
 # en prueba _i, respectivamente .
 prueba_1 <- c(5.5 , 3.4 , 4.5)
 prueba_2 <- c(3.2 , 4.7 , 4.1)
 prueba_3 <- c(4.8 , 4.3 , 5.1)

 # Construir un data frame a partir de los vectores anteriores y
 # guardarlo en la variable dataframe .
 dataframe <- data.frame( nombre ,
                                fecha_nacimiento ,
                                prueba_1 ,
                                prueba_2 ,
                                prueba_3 ,
                                stringsAsFactors = FALSE )

 # Guardar un dataframe en un archivo csv ( formato espa ñol).
 write.csv2 ( dataframe , "C:/ Inferencia / Ejemplo .csv ", row.names = FALSE )
 
 
 
 #Modificar dataframe:
 
 # Leer un dataframe desde archivo csv .
datos <- read.csv2 ("C:/ Inferencia / Ejemplo .csv ", stringsAsFactors = FALSE )

# Eliminar del data frame la columna fecha _ nacimiento .
dataframe $ fecha_nacimiento <- NULL
 
# Agregar al data frame la columna edad .
dataframe $ edad <- c(23 , 25 , 23)


# Crear una nueva observaci ón.
 nueva <- data.frame ( nombre =" Elba Calao del Río",
                             prueba_1 = 6.4 ,
                             prueba_2 = 2.3 ,
                             prueba_3 = 4.6 ,
                             edad = 24)

  # Agregar la nueva observaci ón al data frame .
 dataframe <- rbind ( dataframe , nueva )
 
 # Eliminar las primeras 3 observaciones del data frame .
 dataframe <- dataframe [ -c (1:3) ,]

 # Guardar el dataframe en un archivo csv .
 write.csv2 ( dataframe , "C:/ Inferencia / Ejemplo _mod .csv", row.names = FALSE )
 
 #Obtener datos de dataframe:
 #filter(): selecciona instancias (filas) de acuerdo a su valor.
 #arrange(): modifica el orden de las filas.
 #select(): permite seleccionar variables (características) por sus nombres, a la vez que las reordena.
 #mutate(): permite agregar nuevas variables que se obtienen como funciones de otras ya existentes.
 
 library ( dplyr )
 
# Cargar dataframe iris incluido en R.
datos <- iris
 
#Seleccionar observaciones correspondientes a la especie versicolor .
versicolor <- datos %> % filter ( Species == " versicolor ")
 
# Seleccionar observaciones de la especie versicolor cuyos sé palos tengan una
# longitud igual o superior a 6 cm.
largas <- datos %> % filter ( Species == " versicolor " & Sepal.Length >= 6)

# Seleccionar la especie y variables relativas a los pé talos .
petalos <- datos %> % select ( Species , starts_with (" Petal ") )

# Seleccionar variables de ancho y la especie .
anchos <- datos %> % select ( ends_with (" Width ") , Species )

# Agregar al conjunto de datos de los pé talos una nueva variable con la razón
# entre el largo y el ancho de é stos .
petalos <- petalos %> % mutate ( Species , Petal.Width ,
                                      Petal.Ratio = Petal.Length / Petal.Width )

# Ordenar el conjunto de datos de pé talos en forma descendente seg ún la razón
 # de los pé talos .
 petalos <- petalos %> % arrange ( desc ( Petal.Ratio ) )

# Ordenar el conjunto de datos de pé talos en forma ascendente según el largo de

# los pé talos .
petalos <- petalos %> % arrange ( Petal.Length )
 
# Ordenar el conjunto de datos de pé talos en forma ascendente según el largo de
# los pé talos .
petalos <- petalos %> % arrange ( Petal.Length )


#Cambiar estructura dataframe:
#pivot_longer(cols, names_to, values_to) del paquete tidyr, donde:
#cols: nombres de las columnas a pivotar.
#names_to: especifica el nombre de una nueva columna cuyos valores corresponden a los nombres de las
#columnas a pivotar.
#values_to: especifica el nombre de una nueva columna donde se almacenan los valores de las columnas
#a pivotar.

#pivot_wider(names_from, values_from), también del paquete tidyr, donde:
#names_from: especifica el nombre de una variable desde la que se obtienen los nombres de las nuevas
#columnas.
#values_from:  especifica el nombre de una variable desde donde se obtienen los valores de las nuevas columnas.


library ( dplyr )
 library ( tidyr )

 # Crear el data frame .
 Instancia <- 1:6
 Quicksort <- c(23.2 , 22.6 , 23.4 , 23.3 , 21.8 , 23.9)
 Bubblesort <- c(31.6 , 29.3 , 30.7 , 30.8 , 29.8 , 30.3)
 Radixsort <- c(30.1 , 28.4 , 28.7 , 28.3 , 29.9 , 29.1)
 Mergesort <- c(25.0 , 25.7 , 25.7 , 23.7 , 25.5 , 24.7)
 datos <- data.frame ( Instancia , Quicksort , Bubblesort , Radixsort , Mergesort )
 # Mostrar las primeras filas de la matriz de datos .
 cat (" Datos originales \n")
 print ( head ( datos ) )
cat ("\n")


 # Convertir la matriz de datos a formato largo .
 datos_largos <- datos %> % pivot_longer (c(" Quicksort ", " Bubblesort ",
                                                   " Radixsort ", " Mergesort ") ,
                                                 names_to = " Algoritmo ",
                                                 values_to = " Tiempo ")

 # Mostrar las primeras filas de la matriz de datos largos .
 cat (" Datos largos \n")
 print ( head ( datos_largos ) )
 cat ("\n")

# Convertir la matriz de datos largos a formato ancho .
 datos_anchos <- datos_largos %> % pivot_wider ( names_from = " Algoritmo ",
                                                          values_from = " Tiempo ")

 # Mostrar las primeras filas de la matriz de datos largos .
 cat (" Datos anchos \n")
 print ( head ( datos_anchos ) )
 cat ("\n")

 
 #Modificacin de datos:
 
 library ( dplyr )

# Cargar conjunto de datos .
datos <- mtcars
# Renombrar columnas .
 datos <- datos %> % rename ( Rendimiento = mpg , Cilindrada = cyl ,
                                 Desplazamiento = disp , Potencia = hp ,
                                Eje=drat , Peso = wt , Cuarto_milla = qsec ,
                                 Motor = vs , Transmision = am , Cambios = gear ,
                                 Carburadores = carb )

# Dar formato categ ó rico a las variables Motor y Transmision , renombrando
# sus niveles .
datos [[" Motor "]] <- factor ( datos [[" Motor "]] , levels = c(0 , 1) ,
                                  labels = c("V", " Recto ") )

 datos [[" Transmision "]] <- factor ( datos [[" Transmision "]] , levels = c(0 , 1) ,
                                           labels = c(" Autom á tico ", " Manual ") )

 # Dar formato ordinal a las variables Cilindrada y Cambios , renombrando
 # sus niveles .
 datos [[" Cilindrada "]] <- factor ( datos [[" Cilindrada "]] , levels = c(4 , 6 , 8) ,
                                         labels = c("4 cilindros ", "6 cilindros ",
                                                        "8 cilindros ") ,
                                          ordered = TRUE )
 
  datos [[" Cambios "]] <- factor ( datos [[" Cambios "]] , levels = c(3 , 4 , 5) ,
                                       labels = c("3 cambios ", "4 cambios ", "5 cambios ") ,
                                       ordered = TRUE )
 
 write.csv2 ( datos , "C:/ Inferencia / Mtcars .csv ")
 
