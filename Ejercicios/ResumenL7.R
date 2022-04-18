#INFERENCIA CON PROPORCIONES MUESTRALES:

#Metodo de waltd:
#Verificar:
#se distribuye de manera cercana a la normal cuando se cumplen las siguientes condiciones:
#1. Las observaciones de la muestra son independientes.
#2. Se cumple la condición de éxito-fracaso, que establece que se espera observar al menos 10 observaciones correspondientes a éxito y al menos 10, correspondientes a fracasos. Matemáticamente, np ??? 10
#y n(1 ??? p) ??? 10.

# mu = p; ds = raiz(p*q); p = 1-p; SE = ds = raiz(p*q/n)

#Para una proporcion:______________________
#Ejemplo: de una muestra de n instancias, XX% de dichas instancias cumplen con exito.
#SE = ds = raiz(p*(1-p)/n)

#Code:
# Fijar valores conocidos
 n <- 150
 p_exito <- 0.64
 alfa <- 0.05
 valor_nulo <- 0.7

 # Construcci ón del intervalo de confianza .
 error_est <- sqrt (( p_exito * (1 - p_exito ) ) / n )
 Z_critico <- qnorm ( alfa / 2 , lower.tail = FALSE )
 inferior <- p_exito - Z_critico * error_est
 superior <- p_exito + Z_critico * error_est
 cat (" Intervalo de confianza = [", inferior , ", ", superior , "]\n", sep = "")

 # Prueba de hipó tesis .
 error_est_hip <- sqrt (( valor_nulo * (1 - valor_nulo ) ) / n)
 Z <- ( p_dxito - valor_nulo ) / error_est_hip
 p <- pnorm (Z , lower.tail = FALSE )
 cat (" Hipó tesis alternativa unilateral \n")
 cat ("Z =", Z , "\n")
 cat ("p =", p )

 #Para dos proporciones: estudia la diferencia entre las proporciones de dos
 #poblaciones, considerando para ello como estimador puntual la diferencia p^1 ??? p^2.
 #Verificar:
#1. Cada proporción, por separado, sigue el modelo normal.
 #2. Las dos muestras son independientes una de la otra.
 
 #SE_p1-p2 = raiz(SE_p1¨2 + SE_p2¨2):
 
 
#Code para dif = 0:
# Fijar valores conocidos
  n_hombres <- 48
 n_mujeres <- 42
  exitos_hombres <- 26
  exitos_mujeres <- 20
  alfa <- 0.05
  valor_nulo <- 0
 
  # Calcular probabilidades de é xito .
  p_hombres <- exitos_hombres / n_hombres
  p_mujeres <- exitos_mujeres / n_mujeres
 
  # Estimar la diferencia .
  diferencia <- p_hombres - p_mujeres
 
  # Construcci ón del intervalo de confianza .
  error_hombres <- ( p_hombres * (1 - p_hombres ) ) / n_hombres
  error_mujeres <- ( p_mujeres * (1 - p_mujeres ) ) / n_mujeres
  error_est <- sqrt ( error_hombres + error_mujeres )
  Z_critico <- qnorm ( alfa / 2 , lower.tail = FALSE )
  inferior <- diferencia - Z_critico * error_est
  superior <- diferencia + Z_critico * error_est
  cat (" Intervalo de confianza = [", inferior , ", ", superior , "]\n", sep = "")
 
  # Prueba de hipó tesis .
 
  p_agrupada <- ( exitos_hombres + exitos_mujeres ) / ( n_hombres + n_mujeres )
  error_hombres <- ( p_agrupada * (1 - p_agrupada ) ) / n_hombres
  error_mujeres <- ( p_agrupada * (1 - p_agrupada ) ) / n_mujeres
  error_est_hip <- sqrt ( error_hombres + error_mujeres )
  Z <- ( diferencia - valor_nulo ) / error_est_hip
  p <- 2 * pnorm (Z , lower.tail = FALSE )
  cat (" Hipó tesis alternativa bilateral \n")
  cat ("Z =", Z , "\n")
  cat ("p =", p )
 
 #code dis =/= 0. --> estimador: p1-p2 / / SE= raiz(ES_p1+ES_p1)
  
  # Fijar valores conocidos
   n_hombres <- 89
   n_mujeres <- 61
   exitos_hombres <- 45
   exitos_mujeres <- 21
   alfa <- 0.05
   valor_nulo <- 0.1
  
   # Calcular probabilidades de é xito .
   p_hombres <- exitos_hombres / n_hombres
   p_mujeres <- exitos_mujeres / n_mujeres
  
   # Estimar la diferencia .
   diferencia <- p_hombres - p_mujeres
  
   # Prueba de hipó tesis .
   p_agrupada <- ( exitos_hombres + exitos_mujeres ) / ( n_hombres + n_mujeres )
   error_hombres <- ( p_hombres * (1 - p_hombres ) ) / n_hombres
   error_mujeres <- ( p_mujeres * (1 - p_mujeres ) ) / n_mujeres
   error_est <- sqrt ( error_hombres + error_mujeres )
   Z <- ( diferencia - valor_nulo ) / error_est
   p <- pnorm (Z , lower.tail = FALSE )
   cat (" Hipó tesis alternativa bilateral \n")
   cat ("Z =", Z , "\n")
   cat ("p =", p )
  
   
   ##Solo se usa metodo de wilson: No permite trabajar con diferencia de proporciones =/= 0
   #prop.test(x, n, p, alternative, conf.level,...)
    #x: cantidad de éxitos en la muestra.
  #n: tamaño de la muestra.
   #p: valor nulo (por defecto, p=0).
   #alternative: tipo de hipótesis alternativa, por defecto bilateral (alternative="two.sided"), y valores
   #"less" y "greater" para hipótesis unilaterales.
   #conf.level: nivel de confianza (conf.level=0.95 por defecto).
   
   #Ejemplo para una proporcion:
   # Fijar valores conocidos
    n <- 150
    p_exito <- 0.64
    alfa <- 0.05
    valor_nulo <- 0.7
   
    # Calcular cantidad de é xitos .
    exitos <- p_exito * n
   
    # Prueba de Wilson en R.
    prueba <- prop.test ( exitos , n = n , p = valor_nulo ,
                               alternative = " greater ", conf.level = 1 - alfa )
   
    print ( prueba )
   
    
    #Ejemplo para diferencia ede proporciones:
    # Fijar valores conocidos ( hombres , mujeres )
    n <- c(c(48 , 42) )
    exitos <- c(26 , 20)
    alfa <- 0.05
    valor_nulo <- 0.0
   
    # Prueba de Wilson en R.
    prueba <- prop.test ( exitos , n = n , alternative = "two. sided ",
                              conf.level = 1 - alfa )
   
    print ( prueba )
  
    
    
  #____________Pruebas de poder__________
    #power.prop.test(n, p1, p2, sig.level, power, alternative)
    #n: número de observaciones por cada grupo.
    #p1: probabilidad de éxito en un grupo.
    #p2: probabilidad de éxito en otro grupo.
    #sig.level: nivel de significación.
    #power: poder de la prueba.
    #alternative: tipo de hipótesis alternativa ("one.sided" si es unilateral, "two.sided" si es bilateral).
 
    
    #otras:
    #pwr.p.test(h, n, sig.level, power, alternative): para pruebas con una única proporción.
    #pwr.2p.test(h, n, sig.level, power, alternative): para pruebas con dos proporciones donde
    #ambas muestras son de igual tamaño.
    #pwr.2p2n.test(h, n1, n2, sig.level, power, alternative): para pruebas con dos proporciones
    #y muestras de diferente tamaño.
    #Donde:
    #h: tamaño de efecto.
    #n, n1, n2: tamaño(s) de la(s) muestra(s).
    #sig.level: nivel de significación.
    #power: poder.
    #alternative: tipo de hipótesis alternativa ("two.sided", "less" o "greater").
    
 
