library(dplyr)
#1. En su desquiciada investigación para acabar con los vampiros, Van Helsing ha descubierto que sus
#enemigos tienen predilección por la sangre humana tipo AII+. El cazador sospecha que estos monstruos
#tienen preferencia por la sangre de los adultos, pero aún no está convencido. Por esta razón, mediante
#artimañas, ha encerrado a 14 niños y 20 adultos con este tipo de sangre en un reducto de vampiros. Tras
#3 noches, 3 de los niños y 13 de los adultos fueron atacados. ¿Existe relación entre el ataque de vampiros
#y la edad de la víctima?

# Las muestras son menos del 10% de la población y las muestras son independientes ya
# que son mutuamente excluyentes.

# Ho: La edad y la preferencia de los vampiros son variables independientes.

# Ha: La edad y la preferencia de los vampiros son variables dependientes.

kid <- c(3,11)
adulto <- c(13,7)
alfa <- 0.05

datos <- data.frame(kid,adulto)

prueba <- fisher.test(datos,1-alfa)


p <- prueba$p.value

# p = 0.017 < alfa, por lo tanto se rechaza Ho, por lo tanto hay
# evidencia para concluir que la edad influye en los ataques de los 
# vampiros.

#2.Una Universidad ha detectado que muchos de sus nuevos estudiantes ingresan con elevados niveles de
#ansiedad. Para ello, han decidido evaluar un nuevo programa de bienvenida que busca facilitar la
#adaptación a la vida universitaria. Para ello, han reclutado a un grupo de 15 voluntarios a quienes se les
#midió el nivel de ansiedad (alto o bajo) antes y después de participar en el programa de bienvenida:
#   4 estudiantes no presentaron ansiedad ni antes ni después.
# 5 estudiantes inicialmente ansiosos dejaron de estarlo.
# 4 estudiantes mantuvieron un elevado nivel de ansiedad.
# Los 2 estudiantes restantes desarrollaron síntomas de ansiedad tras participar en el programa.
#¿Qué se puede concluir acerca del nuevo programa de bienvenida?

#H0: El programa de bienvenida no produce cambios significativos en la ansiedad de los alumnos.
#Ha: #H0: El programa de bienvenida si produce cambios significativos en la ansiedad de los alumnos.

modelo1 <- c(rep("ansiedad",4),rep("noAnsiedad",2),rep("ansiedad",5),rep("noAnsiedad",4))
modelo2 <- c(rep("ansiedad",6), rep("noAnsiedad",9))
tabla1 <- table(modelo2,modelo1)
print(tabla1)

prueba2 <- mcnemar.test(tabla1)
print(prueba2)
#p-value = 0,4497 > 0.05 por lo tanto se falla en rechazar Ho. Por lo tanto,
#el programa no produce cambios significativos en la ansiedad de los alunmos.

#3.- En noviembre de 2019, se realizó un estudio acerca de la aprobación al presidente Sebastián Piñera entre
#440 profesores y estudiantes de una prestigiosa universidad, obteniéndose los resultados que se muestran
#en la tabla. ¿Son similares las opiniones de ambos segmentos de la comunidad universitaria?

#Prueba de homogeneidad:
#H0: Entre profesores y alumnos existen preferencias similares.
#Ha: Entre profesores y alumnos no existen preferencias similares.

estudiantes <- c(35,208,17)
profesores <- c(20,151,9)
tabla3 <- as.table(rbind(estudiantes,profesores))
print(tabla3)

prueba3 <- chisq.test(tabla3)
print(prueba3)

#p-value = 0,5789 > 0.05. Por lo tanto, se falla en rechazar la H0 con un 95% de confianza.
#Las preferencias entre alumnos y profesores son similares.


#4.- La Facultad de Ingeniería desea saber si existe diferencia significativa en el desempeño de los estudiantes
#en asignaturas críticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3
#asignaturas, indica si una muestra de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir la Facultad?
#Indicación: obtenga la muestra a partir del archivo “EP07 Datos.csv” que se encuentra en el directorio
#compartido, usando la semilla 685. Considere un nivel de significación α=0,05.

datos <- read.csv2("C:/Users/dpriv/OneDrive/Escritorio/IngInfo/IME/R/EP07 Datos.csv",stringsAsFactors = FALSE)

#Prueba de Q Cochran:
#H0: La proporción de aprobaciones es igual para las 3 asignaturas.
#Ha:La proporción de aprobaciones es diferentes para las 3 asignaturas.

set.seed(685)



if(!require(RVAideMemoire)){
  install.packages("RVAideMemoire")
}
library(dplyr)
library(tidyr)


#Prueba de Q Cochran:
#H0: La proporción de aprobaciones es igual para las 3 asignaturas.
#Ha: La proporción de aprobaciones es diferentes para las 3 asignaturas.

set.seed(685)
#Muestra de 50 alumnos aleatorios (Se saca id y luego se toman sus valores):
id <- sample(datos[["Id"]],50)
muestra <- filter(datos, Id %in% id)
print(muestra)

muestra <- muestra %>% pivot_longer(c("Calculo","Algebra","Fisica"),
                                    names_to = "asignatura",
                                    values_to = "resultado")

prueba4 <- cochran.qtest(resultado ~ asignatura | Id,
                         data = muestra, alpha = 0.05)

print(prueba4)

#p-value = 0,042 < 0,05. Por lo tanto, se rechaza Ho con 95% de confianza.
#La proporcion de aprobaciones es diferente para las 3 asignaturas.