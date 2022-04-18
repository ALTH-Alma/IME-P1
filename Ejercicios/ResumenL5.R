#iNFERENCIA CON MEDIAS MUESTRALES:

#Prueba z:
#Verificaciones:
#La muestra debe tener al menos 30 observaciones. Si la muestra tiene menos de 30 observaciones, se
#debe conocer la varianza de la poblaci�n.
#Las observaciones deben ser independientes, es decir que la elecci�n de una observaci�n para la muestra
#no influye en la selecci�n de las otras.
#La poblaci�n de donde se obtuvo la muestra sigue aproximadamente una distribuci�n normal.

#Sirve para:
#Esta prueba resulta adecuada si queremos asegurar o descartar que la media de la poblaci�n tiene un
#cierto valor hipot�tico.

#Estadistico de prueba z = mu(poblacional) - valor nulo / ds(poblacional)

#Funcion R:
#z.test(x,mu, stdev, alternative, conf.level) - paquete TeachingDemos
#x: vector con las observaciones de la muestra.
#mu: valor nulo.
#stdev: desviaci�n est�ndar de la poblaci�n.
#alternative: tipo de hip�tesis alternativa. Puede tomar los valores "two.sided" (hip�tesis bilateral),
#"less" (hip�tesis unilateral que la media de la poblaci�n es menor que el valor nulo) o "greater"
#(hip�tesis unilateral que la media de la poblaci�n es mayor que el valor nulo).
#conf.level: nivel de confianza.

#Ejemplo cod:
install.packages("TeachingDemos", dependencies = TRUE)
library ( TeachingDemos )
 library ( ggpubr )

 # Ingresar los datos .
 muestra <- c (19.33 , 29.37 , 29.14 , 32.10 , 25.04 , 22.22 , 31.26 , 26.92 ,
                 31.40 , 17.66 , 22.55 , 20.69 , 24.68 , 28.74 , 26.85 , 29.68 ,
                 29.27 , 26.72 , 27.08 , 20.62)

 # Establecer los datos conocidos .
desv_est <- 2.32
 n <- length ( muestra )
 valo_nulo <- 20

 # Crear gr� fico Q-Q para verificar la distribuci �n de la muestra ,
datos <- data.frame ( muestra )

 g <- ggqqplot ( datos , x = " muestra ", color = " SteelBlue ")
 print ( g )

 # Verificar distribuci �n muestral usando la prueba de normalidad
 # de Shapiro - Wilk .
 normalidad <- shapiro.test ( muestra )
 print ( normalidad )

 # Fijar un nivel de significaci �n.
 alfa <- 0.01

 # Calcular la media de la muestra .

 cat ("\ tPrueba Z para una muestra \n\n")
 media <- mean ( muestra )
 cat (" Media =", media , "M$\n")

 # Calcular el estad � stico de prueba .
 Z <- ( media - valor_nulo ) / desv_est
 cat ("Z =", Z , "\n")

 # Calcular el valor p.
 p <- 2 * pnorm (Z , lower.tail = FALSE )
 cat ("p =", p , "\n")
 # Hacer la prueba Z con R.
prueba <- z.test ( media , mu = valor_nulo , alternative = "two. sided ",
                         stdev = desv_est , conf.level = 1 - alfa )

 print ( prueba )


#Prueba T-student:
#Verificaciones:
# 1. Las observaciones son independientes entre s�.
# 2. Las observaciones provienen de una distribuci�n cercana a la normal.

#Sirve para:
 #Esta prueba resulta adecuada si queremos asegurar o descartar que la media de la poblaci�n tiene un
 #cierto valor hipot�tico.

#Estadistico:
 #T = (mu_muestral ??? valor nulo)/(???s_muestral /n)
#Funcion R:
 #t.test(x,alternative, mu, conf.level)
  #x: vector no vac�o de valores num�ricos (la muestra).
 #alternative: tipo de prueba de hip�tesis. Los posibles valores son "two.sided" (prueba bilateral),
 #"greater" (hip�tesis unilateral que la media de la poblaci�n es mayor que el valor nulo) o "less"
 #(hip�tesis unilateral que la media de la poblaci�n es menor que el valor nulo).
 #mu: valor nulo.
 #conf.level: nivel de confianza.
 

#Ejemplo cod:
 
 library ( ggpubr )

 # Cargar los datos .
 tiempo <- c (411.5538 , 393.2753 , 445.8905 , 411.4022 , 498.8969 ,
                 388.6731 , 430.0382 , 469.4734 , 409.5844 , 442.0800 ,
                 418.1169 , 408.4110 , 463.3733 ,407.0908 , 516.5222)

 # Establecer los datos conocidos .
n <- length ( tiempo )
 grados_libertad <- n - 1
 valor_nulo <- 500
 

 # Verificar si la distribuci �n se acerca a la normal .
 g <- ggqqplot ( data = data.frame ( tiempo ) ,
                     x = " tiempo ",
                     color = " steelblue ",
                     xlab = "Te� rico ",
                     ylab = " Muestra ",
                     title = "Gr� fico Q-Q muestra v/s distr . normal ")

  print ( g )
 
 # Fijar un nivel de significaci �n.
  alfa <- 0.025
 
  # Calcular el estad � stico de prueba .
  cat ("\ tPrueba t para una muestra \n\n")
  media <- mean ( tiempo )
 cat (" Media =", media , "M$\n")
  desv_est <- sd( tiempo )
  error <- desv_est / sqrt ( n )
  t <- ( media - valor_nulo ) / error
  cat ("t =", t, "\n")
 
  # Calcular el valor p.
  p <- pt(t, df = grados_libertad , lower.tail = TRUE )
  cat ("p =", p , "\n")
  
  # Construir el intervalo de confianza .
  t_critico <- qt( alfa , df = grados_libertad , lower.tail = FALSE )
  superior <- media + t_critico * error
  cat (" Intervalo de confianza = ( -Inf , ", superior , "]\n", sep = "")
 
  # Aplicar la prueba t de Student con la funcu �n de R.
  prueba <- t.test ( tiempo ,
                         alternative = " less ",
                         mu = valor_nulo ,
                         conf.level = 1 - alfa )
 
  print ( prueba )
  
  #Prueba T muestras pareadas:
  
  #Sirve para:
  #El "algo" de dos "algos" es equivalente.
  #cada observaci�n de un conjunto tiene una correspondencia o conexi�n especial con exactamente una observaci�n del otro.
  #Por ejemplo, muestras iguales de tiempos de ejecucion y la diferencia entre esos tiempos es igual.
  
  #Estadistico:
  #T = (mu_muestral_dif ??? valor nulo)/(raiz(s_muestral_dif) /n)
  
  #Funcion R:
  #t.test(x, y, paired, alternative, mu, conf.level)
  #x: vector de valores num�ricos para la primera muestra).
  #y: vector de valores num�ricos para la segunda muestra).
  #paired: booleano (por defecto falso) que, cuando es verdadero, indica que ambas muestras est�n pareadas.
  #alternative: tipo de prueba de hip�tesis.
  #mu: valor nulo.
  #conf.level: nivel de confianza.
  

  #Ejemplo cod:
  
  # Cargar los datos .
 instancia <- seq (1 , 35 , 1)

 t_A <- c (436.5736 , 470.7937 , 445.8354 , 470.9810 , 485.9394 ,
                464.6145 , 466.2139 , 468.9065 , 473.8778 , 413.0639 ,
                496.8705 , 450.6578 , 502.9759 , 465.6358 , 437.6397 ,
                458.8806 , 503.1435 , 430.0524 , 438.5959 , 439.7409 ,
                464.5916 , 467.9926 , 415.3252 , 495.4094 , 493.7082 ,
                433.1082 , 445.7433 , 515.2049 , 441.9420 , 472.1396 ,
                451.2234 , 476.5149 , 440.7918 , 460.1070 , 450.1008)

  t_B <- c (408.5142 , 450.1075 , 490.2311 , 513.6910 , 467.6467 ,
                 484.1897 , 465.9334 , 502.6670 , 444.9693 , 456.3341 ,
                 501.1443 , 471.7833 , 441.1206 , 544.1575 , 447.8844 ,
                 432.4108 , 477.1712 , 482.4828 , 458.2536 , 474.9863 ,
                 496.0153 , 485.8112 , 457.4253 , 483.3700 , 510.7131 ,
                 467.5739 , 482.5621 , 453.5986 , 385.9391 , 548.7884 ,
                 467.2533 , 494.7049 , 451.9716 , 522.3699 , 444.1270)
  
   diferencia <- t_A - t_B
  
   # Verificar si la distribuci �n se acerca a la normal .
   normalidad <- shapiro.test ( diferencia )
   print ( normalidad )
  
   # Fijar un nivel de significaci �n.
   alfa <- 0.05
  
   # Aplicar la prueba t de Student a la diferencia de medias .
   prueba_1 <- t.test ( diferencia ,
                             alternative = " two. sided ",
                             mu = valor_nulo ,
                             conf.level = 1 - alfa )
  
   print ( prueba_1)
  
   # Otra alternativa puede ser aplicar la prueba t de Student
   # para dos muestras pareadas .
   prueba_2 <- t.test ( x = t_A ,
                             y = t_B , paired = TRUE ,
                             alternative = " two. sided ",
                             mu = valor_nulo ,
                             conf.level = 1 - alfa )
  
   print ( prueba_2)
   
   
   
   #Prueba T - 2 muestras independientes:
   
   #Sirve para:
   #Se usa para comparar las medias de dos poblaciones en que las observaciones con
   #que se cuenta no tienen relaci�n con ninguna de las otras observaciones, ni influyen en su selecci�n, ni en la
   #misma ni en la otra muestra. En este caso la inferencia se hace sobre la diferencia de las medias: �1 ???�2 = d0,
   #donde d0 es un valor hipot�tico fijo para la diferencia.
   
   #Requisitos:
   #1. Cada muestra cumple las condiciones para usar la distribuci�n t.
   #2. Las muestras son independientes entre s�.
   
   #Estadistico:
   #T = ((mu_muestral1 - mu_muestral2) ??? d0)/ SE_(mu_muestral1 - mu_muestral2)
   
   #Funcion R:
   #t.test(x, y, paired, alternative, mu, conf.level)
   #x: vector de valores num�ricos para la primera muestra).
   #y: vector de valores num�ricos para la segunda muestra).
   #paired: booleano (por defecto falso) que, cuando es verdadero, indica que ambas muestras est�n pareadas.
   #alternative: tipo de prueba de hip�tesis.
   #mu: valor nulo.
   #conf.level: nivel de confianza.
   
   
   #Ejemplo cod:
   
   library ( ggpubr )
   
    # Cargar los datos .
    vacuna_A <- c(6.04 , 19.84 , 8.62 , 13.02 , 12.20 , 14.78 , 4.53 , 26.67 ,
                      3.14 , 19.14 , 10.86 , 13.13 , 6.34 , 11.16 , 7.62)
   
    vacuna_B <- c(5.32 , 3.31 , 5.68 , 5.73 , 4.86 , 5.68 , 2.93 , 5.48 , 6.10 ,
                      2.56 , 7.52 , 7.41 , 4.02)
   
    # Verificar si las muestras se distribuyen de manera cercana
    # a la normal .
    normalidad_A <- shapiro.test ( vacuna_A )
   print ( normalidad_A )
    normalidad_B <- shapiro.test ( vacuna_B )
    print ( normalidad_B )
   
    # Fijar un nivel de significaci �n.
    alfa <- 0.01
   
    # Aplicar la prueba t para dos muestras independientes .
    prueba <- t.test ( x = vacuna_A ,
                           y = vacuna_B ,
                           paired = FALSE ,
                           alternative = " greater ",
                           mu = 0 ,
                           conf.level = 1 - alfa )

    print ( prueba )
   
    # Calcular la diferencia entre las medias .
    media_A <- mean ( vacuna_A )
    media_B <- mean ( vacuna_B )
    diferencia <- media_A - media_B
    cat (" Diferencia de las medias =", diferencia , "[mg/ml ]\n")

    #SE_(mu1-mu2) = raiz((SE_mu1)¨2) + (SE_mu2)¨2 )
   
 