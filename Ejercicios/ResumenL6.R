#PODER:

#Tamño del efecto:
#d de Cohen para prueba t simple:
#d = (mu_muestral - valor nulo)/ ds_muestral (n-1 = v)

#d de Cohen para prueba t independiente con n >50:
#d = (mu_muestral1 - media_muestral2)/ ds_p

#d de Cohen para prueba t independiente con n <50:
#d = ((mu_muestral1 - media_muestral2)/ ds_p) * ((n1+n2-3)/(n1+n2-2.25))

#ds_p = raiz((sum((x-mu1)¨2))+sum((x-mu2)¨2))/ n1+n2-2)

#d de Cohen para prueba t pareada n>50:
#d = mu_muestrad-dif / ds_muestral_dif


#Funciones poder: library(pwr)

#1.- power.t.test(n, delta, sd, sig.level, power, type, alternative)
#n: tamaño de la muestra (por cada grupo, si corresponde).
#delta: diferencia observada entre las medias, o entre la media muestral y el valor nulo, no estandarizada.
#sd: desviación estándar observada.
#sig.level: nivel de significación.
#power: poder de la prueba.
#type: tipo de prueba t de Student ("two.sample" para diferencia de medias, "one.sample" para una
                                    #sola muestra o "paired" para dos muestras pareadas).
#alternative: tipo de hipótesis alternativa ("one.sided" si es unilateral, "two.sided" si es bilateral).


#Para prueba t sinple, pareada o t independiente con muestras de igual tamaño:
# 2.- pwr.t.test(n, d, sig.level, power, type, alternative):
#n: tamaño de la muestra (por cada grupo, si corresponde).
#d: tamaño del efecto (d de Cohen).
#sig.level: nivel de significación.
#power: poder de la prueba.
#type: tipo de prueba t de Student ("two.sample" para diferencia de medias, "one.sample" para una
                                    #sola muestra o "paired" para dos muestras pareadas).
#alternative: tipo de hipótesis alternativa ("greater" o "less" si es unilateral, "two.sided" si es
                                           #  bilateral).

#Para prueba t independiente con muestras de diferente tamaño:
#pwr.t2n.test(n1, n2, d, sig.level, power, alternative).

