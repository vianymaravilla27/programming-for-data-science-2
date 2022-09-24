#Ejercicio 2 Practica 9
# Análisis Factorial
# Dataset de hospitalizaciones por SARS-COV2
# Vianey Maravilla Pérez 3AM1

#Definimos las librerias a utilizar durante todo el proceso
library(openxlsx) #Librería que interactúa con MSExcel
library(corrplot) #Librería para el gráfico de correlaciones
library(corrr) #Otra opción de librería para el cálculo y gráfico de correlaciones
library(psych)
library (ggcorrplot)
library(stats) #Librería del sistema base
library(polycor)
library(GPArotation)

# Lectura de la BDD de acuerdo a su ubicación
hospi <- read.csv('C:/Users/viane/Desktop/ESCOM/3.-TERCER SEMESTRE/PROGRAMACION PARA LAS CIENCIAS DE DATOS/Ejercicio2P9.csv') 

# Visualización de la tabla
hospi

# Obtener matriz de correlación
matriz_correlaciones <- cor(hospi, use = "pairwise.complete.obs")
matriz_correlaciones

# Obtenermos la gráfica de las correlaciones 
corrplot(cor(hospi), order = "hclust", tl.col='black', tl.cex=1)

# Calculo de un objeto de correlaciones
hospi_correlaciones <- correlate(hospi)

# Obtener la gráfica de las correlaciones
rplot(hospi_correlaciones, legend = TRUE, colours = c("firebrick1", "black", "darkcyan"), print_cor = TRUE)

# Determinante de la matriz de correlaciones de las variables ingresadas
det(matriz_correlaciones)

# Obtener el cálculo de los estimadores del Test de Barlett y KMO
bartlett.test(hospi)

KMO(hospi)

factanal(hospi, factors = 2, rotation = "none")

factanal(hospi, factors = 2, rotation = "none", scores = "regression")$scores

puntuaciones <- factanal(hospi, factors = 2, rotation = "none", scores = "regression")$scores
hospi <- cbind(hospi, puntuaciones)
hospi$Factor1 <- round(((hospi$Factor1 - min(hospi$Factor1))/(max(hospi$Factor1) - min(hospi$Factor1))), 2)
hospi

hist(hospi$Factor1, freq = TRUE, main = "Gráfico de la Distribución del Factor 1", 
     xlab = "Factor 1", ylab = "Frecuencia", col = "#009ACD")

#Calcular la matriz de correlación policorica 
mat_cor <- hetcor(hospi)$correlations #matriz de correlación policorica
ggcorrplot(mat_cor,type="lower",hc.order = T)

# Obtener el cálculo de los estimadores del Test de Barlett y KMO con nuestra matriz de correlación polocorica
cortest.bartlett(mat_cor)->p_esf
p_esf$p

KMO(mat_cor)

# Pendiente 
# Escoger un método para extraer los factores
# minres: minimo residuo /
# mle: maxima verosimilitud /
# paf: método de ejes principales /
# alpah: alfa /
# minchi: minimos cuadrados /
# minrak : rango minimo /

modelo1<-fa(mat_cor,
            nfactors = 3,
            rotate = "none",
            fm="mle") # Modelo máxima verosimilitud

modelo2<-fa(mat_cor,
            nfactors = 3,
            rotate = "none",
            fm="minres") # Modelo minimo residuo


# Se comparan las comunalidades
sort(modelo1$communality,decreasing = T)->c1
sort(modelo2$communality,decreasing = T)->c2
head(cbind(c1,c2))

#Se comparan las unicidades
sort(modelo1$uniquenesses,decreasing = T)->u1
sort(modelo2$uniquenesses,decreasing = T)->u2
head(cbind(u1,u2))

# Termina pendiente 

# Determinar el número de factores
scree(mat_cor)
fa.parallel(mat_cor,n.obs=200,fa="fa",fm="minres")


# Rotar la matriz
rot<-c("none", "varimax", "quartimax","Promax")
bi_mod<-function(tipo){
  biplot.psych(fa(hospi,nfactors = 2,fm="minres",rotate = tipo),main = paste("Biplot con rotación ",tipo),
               col=c(2,3,4),pch = c(21,18),group = bfi[,"gender"])  
}
sapply(rot,bi_mod)


# Interpretación
modelo_varimax<-fa(mat_cor,nfactors = 5,rotate = "varimax",
                   fa="minres")
fa.diagram(modelo_varimax)

