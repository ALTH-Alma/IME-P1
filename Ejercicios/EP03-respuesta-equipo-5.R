
require(dplyr)
require(discreteRV)
require(ggpubr)

if(!require(lattice)){
  install.packages("lattice",dependencies = TRUE)
  require(lattice)
}
#IMPORTANTE: Cambiar ruta del archivo.
poblaci�n <- read.csv2("Direccion",stringsAsFactors = FALSE)
tama�o <- nrow(poblaci�n)

# C�digo de uvirtual
ingreso <- as.numeric(poblaci�n[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tama�o.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tama�o.podado )
set.seed(233)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
##

# Calculando la distribuci�n normal est�ndar restandole la media
# y dividiendola por la desviaci�n est�ndar.
Z <- (ingreso.normal - media.ingreso)/sd.ingreso

# Creando la distribuci�n chi-cuadrado con 6 grados
# de libertad.
X <- c()
for(i in 1:5000){
  chi <- sample(Z,6)
  chi <- sum(chi^2)
  X <- append(X,chi)
  
}

# Calculando la distribuci�n chi-cuadrado con
# 9 grados de libertad.
X2 <- c()
for(i in 1:5000){
  chi <- sample(Z,9)
  chi <- sum(chi^2)
  X2 <- append(X2,chi)
  
}

# Calculando la distribuci�n F a partir de las 
# dos chi-cuadrado anteriores.
F <- (X/6)/(X2/9)


# Graficando los diagramas de densidad.
dz <- densityplot(Z)
dx <- densityplot(X)
df <- densityplot(F)
print(dz)
print(dx)
print(df)


# C�digo de uvirtual
set.seed(233)
n.repeticiones <- 40

ensayo <- function(x)
  ifelse(sample(poblaci�n[["sexo"]], 1) == "Mujer", 1, 0)
## 


# Calculando la distribuci�n binomial sacando 5000
# muestras y sumando los �xitos, para graficarlos.
B <- c()
for(i in 1:5000){
  repeticiones <- sapply(1:repeticiones, ensayo)
  exitos <- sum(repeticiones)
  B <- append(B,exitos)
}

hb <- histogram(B)


# Sacando la distribuci�n geom�trica, calculando
# cu�ntos intentos se requieren hasta tener un �xito
# y graficarlos en 5000 muestras.
intentos <- 0
G <- c()

for(i in 1:5000){
  while(!ensayo()){
    intentos <- intentos + 1
  }
  intentos <- intentos + 1
  G <- append(G,intentos)
  intentos <- 0
  
}
intentos <- 0
exitos <- 0
hg <- histogram(G)
print(hg)


# Sacando la binomial negativa con k=10 
# calculando cu�ntos intentos se requieren 
# para tener 10 �xitos y graficarlos. 
intentos <- 0
exitos <- 0
k <- 10
BN <- c()
for(i in 1:5000){
  while(exitos <= k){
    if(ensayo()){
      exitos <- exitos + 1
    }
    intentos <- intentos + 1
  }
  
  exitos <- 0
  BN <- append(BN,intentos)
  intentos <- 0
  
}

hbn <- histogram(BN)
print(hbn)
