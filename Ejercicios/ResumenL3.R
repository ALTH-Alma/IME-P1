#DISTRIBUCIONES:
#Variable aleatoria continua: infinita
#Variable aleatoria discreta: finita
#Valor esperado E(x): resultado promedio = mu
#Varianza general(var): que tan lejos esta un valor obtenido del valor esperado.

#discreteRV libreria: trabaja con VAD
library ( discreteRV )
 # Crear una variable discreta para representar el dado
# adulterado de la tabla 3.1.
resultados <- 1:6
 probabilidades = c(0.25 , 0.125 , 0.125 , 0.125 , 0.125 , 0.25)
 X <- RV( outcomes = resultados , probs = probabilidades )

 # Calcular el valor esperado .
 esperado <- E( X )
 cat (" Valor esperado :", esperado , "\n")

 # Calcular la varianza .
 varianza <- V(X)
 cat(" Varianza :", varianza , "\n")

 # Calcular la desviaci ón está ndar .
 desviacion <- SD( X )
 cat (" Desviaci ón está ndar :", desviacion , "\n")
 
 #Graficas de VAD:
 
 library ( discreteRV )
  library ( ggpubr )
 
  # Crear una variable discreta para representar el dado
  # adulterado de la tabla 4.1.
  resultados <- 1:6
 
  probabilidades = c(0.25 , 0.125 , 0.125 , 0.125 , 0.125 , 0.25)
  X <- RV ( outcomes = resultados , probs = probabilidades )
 
  # Crear vector con los resultados de 5 lanzamientos del dado .
  lanzar_5 <- SofIID (X , n =5)
 
  # Crear vector con los resultados de 10 lanzamientos del dado .
  lanzar_10 <- SofIID (X , n =10)
 
  # Crear vector con los resultados de 20 lanzamientos del dado .
  lanzar_20 <- SofIID (X , n =20)
 
  # Graficar los resultados .
  par( mfrow =c(1 , 3) )
 
  plot( lanzar_5 ,
            main =" Lanzamiento de 5 dados ",
            xlab =" Suma de resultados ",
            ylab =" Probabilidad ")
 
  plot ( lanzar_10 ,
           main =" Lanzamiento de 10 dados ",
           xlab =" Suma de resultados ",
           ylab =" Probabilidad ")
 
  plot ( lanzar_20 ,
            main =" Lanzamiento de 20 dados ",
            xlab =" Suma de resultados ",
            ylab =" Probabilidad ")
  
  
 # Crear una variable discreta para representar el dado adulterado de la tabla
 # 3.1 , y calcular su valor esperado , varianza y desviaci ón está ndar .
 resultados <- 1:6
 probabilidades = c(0.25 , 0.125 , 0.125 , 0.125 , 0.125 , 0.25)
 X <- RV ( outcomes = resultados , probs = probabilidades )
 esperado_x <- E ( X )
 varianza_x <- V ( X )
 desviacion_x <- SD ( X )

 # Crear una variable aleatoria para un dado balanceado , y calcular su valor
 # esperado , varianza y desviaci ón está ndar .
 Y <- RV ( outcomes = resultados , probs = 1/6)
#Si las variables no son independientes, se requieren métodos más complejos fuera del alcance de este libro.

   esperado_y <- E ( Y )
   varianza_y <- V ( Y )
   desviacion_y <- SD ( Y )

 # Crear una combinaci ón lineal de variables aleatorias , y calcular su valor
   # esperado , varianza y desviaci ón est á ndar .
   Z <- 0.5 * X + 0.5 * Y
   esperado_z <- E ( Z )
   varianza_z <- V ( Z )
   desviacion_z <- SD ( Z )

#Variables continuas:
  
#Distribucion gaussiana o normal: unimodal, simetrica (N(media,desv))
   library ( ggpubr )
   
    # Generar valores para una distribuci ón normal con media 10 y
    # desviaci ón está ndar 6.
   media <- 10
    desv_est <- 6
    x <- seq ( -15 , 35 , 0.01)
    y <- dnorm (x , mean = media , sd = desv_est )
    normal_2 <- data.frame (x , y )
   
   # Graficar ambas distribuciones .
    g <- ggplot ( normal_1 , aes (x , y ) ) + geom_line ( color = " blue ")
    g <- g + geom_line ( data = normal_2 , color = "red")
    g <- g + theme_pubr ()
   
    print ( g )
  
    #dnorm(x, mean,sd):calcula la densidad de una distribución normal. 
    #pnorm(q, mean, sd, lower.tail): permite encontrar percentiles, los cuales corresponden a la función de distribución acumulada
    #(es decir, la probabilidad de que la variable tome valores menores o iguales que un valor dado), a partir de las probabilidades.
    #qnorm(p, mean, sd, lower.tail): encuentra el percentil para las probabilidades dadas en p, por lo que es la función inversa de pnorm().
    #rnorm(n, mean, sd): genera aleatoriamente n observaciones de la distribución normal especificada.
    #x, q: vector de cuantiles (percentiles).
    #p: vector de probabilidades.
    #mean: media de la distribución normal.
    #sd: desviación estándar de la distribución normal.
    #lower.tail: valor lógico que señala cuál de los dos extremos o colas de la distribución emplear.
    #n: tamaño del vector resultante.
    
 #---------------------------------------------------
    
#Distribucion Z: N(0,1) / Z = (x-mu)/des
    
#Distribucion chi-cuadrado: mu = V = grados de libertad; des= 2*V
    #dchisq(x, df).
    #pchisq(q, df, lower.tail).
    #qchisq(p, df, lower.tail).
    #rchisq(n, df).
    #x, q son vectores de cuantiles (enteros no negativos).
    #p es un vector de probabilidades.
    #n es la cantidad de observaciones.
    #df son los grados de libertad.
    #lower.tail es análogo al de la función pnorm.
#------------------------------------------------------
    
#Distribucion T-Student: muestras pequeñas. V>1 --> mu=0 / V>2 --> ds= raiz(V/V-2) 
    
    #dt(x, df).
    #pt(q, df, lower.tail).
    #qt(p, df, lower.tail).
    #rt(n, df).
    
#Distribucion F:
    #df(x, df1, df2).
    #pf(q, df1, df2, lower.tail).
    #qf(p, df1, df2, lower.tail).
    #rf(n, df1, df2).
    
    
#______________________
    #Distribuciones discretas:
    
    #1.- Bernoulli: mu = p / ds = raiz(p*q)
    #Probabilidad de exito = p.
    # // de fracaso = q = 1-p
    #extraDistr de R ofrece 4 funciones: 
    #dbern(x, prob).
    #pbern(q, prob, lower.tail).
    #qbern(p, pro, lower.tail).
    #rbern(n, prob).
#--------------------
    #2.- Binomial: mu=n*p / ds= raiz(n*p(1-p))
    #Para usar la distribución binomial, tenemos que verificar cuatro condiciones:
    #1. Los intentos son independientes.
    #2. La cantidad de intentos (n) es fija.
    #3. El resultado de cada intento puede ser clasificado como éxito o fracaso.
    #4. La probabilidad de éxito (p) es la misma para cada intento.
    #Las funciones que ofrece R para trabajar con esta distribución son:
    #dbinom(x, size, prob).
    #pbinom(x, size, prob).
    #qbinom(p, size, prob).
    #rbinom(n, size, prob).
    #x es un vector numérico.
    #p es un vector de probabilidades.
    #n es la cantidad de observaciones.
    #size corresponde al número de intentos.
    #prob es la probabilidad de éxito de cada intento.
    
    
    
 