#Ejercicio 1 Practica 9
# An?lisis Factorial
# Vianey Maravilla P?rez 3AM1

#Definimos las librerias a utilizar durante todo el proceso
library(openxlsx) #Librer?a que interact?a con MSExcel
library(corrplot) #Librer?a para el gr?fico de correlaciones
library(corrr) #Otra opci?n de librer?a para el c?lculo y gr?fico de correlaciones
library(psych)
library (ggcorrplot)
library(stats) #Librer?a del sistema base
library(polycor)
library(GPArotation)

# Lectura de la BDD de acuerdo a su ubicaci?n
archivo <- read.csv('C:/Users/viane/Desktop/ESCOM/3.-TERCER SEMESTRE/PROGRAMACION PARA LAS CIENCIAS DE DATOS/Ejercicio1.csv') 

# Visualizaci?n de la tabla
archivo 

# Obtener matriz de correlaci?n
matriz_correlaciones <- cor(archivo, use = "pairwise.complete.obs")
matriz_correlaciones

# Obtenermos la gr?fica de las correlaciones 
corrplot(cor(archivo), order = "hclust", tl.col='black', tl.cex=1)

# Calculo de un objeto de correlaciones
archivo_correlaciones <- correlate(archivo)

# Obtener la gr?fica de las correlaciones
rplot(archivo_correlaciones, legend = TRUE, colours = c("firebrick1", "black", "darkcyan"), print_cor = TRUE)

# Determinante de la matriz de correlaciones de las variables ingresadas
det(matriz_correlaciones)

# Obtener el c?lculo de los estimadores del Test de Barlett y KMO
bartlett.test(archivo)

KMO(archivo)

factanal(archivo, factors = 2, rotation = "none")

factanal(archivo, factors = 2, rotation = "none", scores = "regression")$scores

puntuaciones <- factanal(archivo, factors = 2, rotation = "none", scores = "regression")$scores
archivo <- cbind(archivo, puntuaciones)
archivo$Factor1 <- round(((archivo$Factor1 - min(archivo$Factor1))/(max(archivo$Factor1) - min(archivo$Factor1))), 2)
archivo

hist(archivo$Factor1, freq = TRUE, main = "Gr?fico de la Distribuci?n del Factor 1", 
     xlab = "Factor 1", ylab = "Frecuencia", col = "#009ACD")

#Calcular la matriz de correlaci?n policorica 
mat_cor <- hetcor(archivo)$correlations #matriz de correlaci?n policorica
ggcorrplot(mat_cor,type="lower",hc.order = T)

# Obtener el c?lculo de los estimadores del Test de Barlett y KMO con nuestra matriz de correlaci?n polocorica
cortest.bartlett(mat_cor)->p_esf
p_esf$p

KMO(mat_cor)


# Escoger un m?todo para extraer los factores
# minres: minimo residuo /
# mle: maxima verosimilitud /
# paf: m?todo de ejes principales /
# alpah: alfa /
# minchi: minimos cuadrados /
# minrak : rango minimo /


modelo1<-fa(mat_cor,
            nfactors = 3,
            rotate = "none",
            fm="mle") # Modelo m?xima verosimilitud

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




# Determinar el n?mero de factores
scree(mat_cor)
fa.parallel(mat_cor,n.obs=200,fa="fa",fm="minres")


# Rotar la matriz
rot<-c("none", "varimax", "quartimax","Promax")
bi_mod<-function(tipo){
  biplot.psych(fa(archivo,nfactors = 2,fm="minres",rotate = tipo),main = paste("Biplot con rotaci?n ",tipo),
               col=c(2,3,4),pch = c(21,18),group = bfi[,"gender"])  
}
sapply(rot,bi_mod)


# Interpretaci?n
modelo_varimax<-fa(mat_cor,nfactors = 5,rotate = "varimax",
                   fa="minres")
fa.diagram(modelo_varimax)











