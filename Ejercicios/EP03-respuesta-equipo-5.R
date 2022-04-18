
require(dplyr)
require(discreteRV)
require(ggpubr)

if(!require(lattice)){
  install.packages("lattice",dependencies = TRUE)
  require(lattice)
}
#IMPORTANTE: Cambiar ruta del archivo.
población <- read.csv2("Direccion",stringsAsFactors = FALSE)
tamaño <- nrow(población)

# Código de uvirtual
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )
set.seed(233)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
##

# Calculando la distribución normal estándar restandole la media
# y dividiendola por la desviación estándar.
Z <- (ingreso.normal - media.ingreso)/sd.ingreso

# Creando la distribución chi-cuadrado con 6 grados
# de libertad.
X <- c()
for(i in 1:5000){
  chi <- sample(Z,6)
  chi <- sum(chi^2)
  X <- append(X,chi)
  
}

# Calculando la distribución chi-cuadrado con
# 9 grados de libertad.
X2 <- c()
for(i in 1:5000){
  chi <- sample(Z,9)
  chi <- sum(chi^2)
  X2 <- append(X2,chi)
  
}

# Calculando la distribución F a partir de las 
# dos chi-cuadrado anteriores.
F <- (X/6)/(X2/9)


# Graficando los diagramas de densidad.
dz <- densityplot(Z)
dx <- densityplot(X)
df <- densityplot(F)
print(dz)
print(dx)
print(df)


# Código de uvirtual
set.seed(233)
n.repeticiones <- 40

ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)
## 


# Calculando la distribución binomial sacando 5000
# muestras y sumando los éxitos, para graficarlos.
B <- c()
for(i in 1:5000){
  repeticiones <- sapply(1:repeticiones, ensayo)
  exitos <- sum(repeticiones)
  B <- append(B,exitos)
}

hb <- histogram(B)


# Sacando la distribución geométrica, calculando
# cuántos intentos se requieren hasta tener un éxito
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
# calculando cuántos intentos se requieren 
# para tener 10 éxitos y graficarlos. 
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
