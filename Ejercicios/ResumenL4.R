#FUNDAMENTOS DE INFERENCIA:
#Ley de los grandes numeros: el estimador tiende a mejorar a medida que crece la muestra.

#Error estandar (SE): desviasion estandar de la distribucion de un estimador muestral.
#SE_mu = s/raiz(n); s:ds de la muestra.
#Intervalo de confianza: rango de valores plausibles para un estimador. XX% confianza de haber campturado el valor real.
#95% (mu +- 2*SE; z* = 2 --> margen de error) de confianza, 99% de confianza, etc. ----> confianza = 1-alfa.
#IC --> mu (+/-) z* * SE_mu)--> con confianza= 1-alfa --> z* = qnorm((1-alfa)/2,mean,sd,lower.tail=FALSE)
# Se tiene xx% confianza de que el parametro de la poblacion se encuentre en IC.
#Ejemplo calculo z* con 95% de confianza:
#P. bilateral:
qnorm(0.025, mean = 0, sd = 1, lower.tail = FALSE)

#P. unilateral, menor que valor nulo --> se descarta cola superior: IC [-inf,mu-zSE]
qnorm(0.05, mean = 0, sd = 1, lower.tail = FALSE)

#P. unilateral, mayor que valor nulo --> se descarta cola inferior: IC [mu+zSE,+inf]
qnorm(0.05, mean = 0, sd = 1, lower.tail = TRUE)

#HIPOTESIS:
#Pruebas bilaterales: 
#   H0: tal estimador es igual a X. Por ejemplo: mu = X. Tal que X: valor nulo
#   Ha: tal estimador es diferente a X. Por ejemplo: mu =/= X.

#Pruebas unilaterales: 
#   H0: tal estimador es igual a X. Por ejemplo: mu = X. Tal que X: valor nulo
#   Ha: tal estimador es > 0 < a X. Por ejemplo: mu > X o mu < X.

#Las posibles respuestas son: 
#  - Se rechaza hipotesis nula (H0) a favor de la hipotesis alternativa (Ha)
#  - Se falla en rechaza hipotesis nula (H0) a favor de la hipotesis alternativa (Ha)

#Tipos de errores:
#El error tipo I corresponde a rechazar H0 cuando en realidad es verdadera = alfa
#El error tipo II corresponde a no rechazarla cuando en realidad HA es verdadera = beta.


#Prueba p-valor:
# Si  p-value > ?? --> se falla en rechazar H0.


