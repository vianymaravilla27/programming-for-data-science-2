# Primero importamos las librerias que vayamos a ocupar, para esto, se hizo una instalación para posteriormente hacer 

library(readr)
library(tidyverse)# Manipulacion de datos 
library(factoextra)# Librería de clusterización y visualización 
library(openxlsx) # Librería que interactúa con MSExcel
library(corrplot) # Librería para el gráfico de correlaciones
library(corrr) # Otra opción de librería para el cálculo y gráfico de correlaciones
library(psych)
library (ggcorrplot)
library(stats) # Librería del sistema base
library(polycor)
library(GPArotation)
library (cluster)# Algoritmos de clusterización
library(gridExtra)
library (ggplot2)

######################### Analisis de componentes principales ########################

#Mandamos a leer nuestro dataset, esto primero lo debemos de descargar para poderlo mandar a llamar con la siguiente instrucción

dataset <- read_csv("C:\Users\cobym\OneDrive - Instituto Politecnico Nacional\ESCOM\Tercer Semestre\Programacion Avanzada  a Ciencia de Datos\Proyecto 3/Proyecto(1).csv")
dataset

# O lo siguiente
# read.xlsx(xlsxFile='C:/Users/viane/Desktop/ESCOM/3.-TERCER SEMESTRE/PROGRAMACION PARA LAS CIENCIAS DE DATOS/af.xlsx',
          #sheet = 'Hoja1')
#Nomralizacion de los datos

datos_Centrados<- dataset
datos_Centrados$fixed_acidity            <- dataset$fixed_acidity                   - mean(dataset$fixed_acidity  )
datos_Centrados$volatile_acidity         <- dataset$volatile_acidity                - mean(dataset$volatile_acidity )
datos_Centrados$citric_acid              <- dataset$citric_acid                     - mean(dataset$citric_acid)
datos_Centrados$residual_sugar           <- dataset$residual_sugar                  - mean(dataset$residual_sugar)
datos_Centrados$alcohol                  <- dataset$alcohol                         - mean(dataset$alcohol)
datos_Centrados$free_sulfur_dioxide      <- dataset$free_sulfur_dioxide             - mean(dataset$free_sulfur_dioxide)
datos_Centrados

#Una vez con los datos ya calculados, calculamos la covarianza de nuestros respectivos datos

matriz_Cov <- cov(datos_Centrados)
matriz_Cov

#PCA
#Calculamos los autovalores y autovectores  (valores propios de la matriz de covarianza)

eigen_a<- eigen(matriz_Cov)
eigen_a$values

#Obtencion de los vectores propios, componentes principales

eigen_a$vectors

#Se calcula la transpuesta de nuestros datos ya tratados

t_eigenvectors <- t(eigen_a$vectors)
t_eigenvectors

#Se calacula la transpuesta de nuestros datos originales con su promedio

t_datos_Centrados <- t(datos_Centrados)
t_datos_Centrados

# Se multiplican los componentes principales por los datos nomalizados (Transpuestos)

pc_scores<- t_eigenvectors %*% t_datos_Centrados
pc_scores
rownames(pc_scores) <- c("PC 1", "PC 2", "PC 3", "PC 4", "PC 5", "PC 6")
pc_scores


#Los transformamos nuevamente para que los datos esten en modo tabla

t(pc_scores)
datos_recuperados <- t(eigen_a$vectors %*% pc_scores)
datos_recuperados

#Con apply aplicamos la funcion en todos los elementos de nuestra matriz

Media <- apply(X = dataset, MARGIN = 2, FUN = mean)  #Promedio
Varianza <- apply(X = dataset, MARGIN = 2, FUN = var)   #Varianza
Desviacion_e <- apply(X = dataset, MARGIN = 2, FUN = sd) #Desviación Estandar
Media
Varianza
Desviacion_e


#Calculamos nuestro Analisis de componentes principales 

ACP <- prcomp(dataset, scale= TRUE)
names(ACP)

#Lo aplicamos para las siguientes funciones y se pueda visualizar la informacion de cada "renglon"

ACP$center
ACP$scale
ACP$rotation

#Definimos nuestras graficas para poder tener una mejor visualizacion de los datos y llegar a una mejor conclusion de nuestros datos 

head(ACP$x)  # Visualizamos los primeros 6 datos de nuestra matriz 
fviz_eig(ACP) #Hacemos el grafico de nuestro respectivos datos de nuestra PCA

#Representación bidimensional de las primeras dos componentes (por defecto)

fviz_pca_biplot(ACP, repel = TRUE,
                col.var = "#008B8B", # Definimos el color de las variables
                col.ind = "#8A2BE2"  # Definimos el color de las variables independientes 
)

#Representacion gbidimensional de las primeras dos componentes (por defecto)

fviz_pca_ind(ACP,
             col.ind = "cos2", #Definimos el color de la representación
             gradient.cols = c("#76EEC6", "#66CDAA", "#458B74"),
             repel = TRUE     #Evitamos la superposición del texto
)

# Representación en tercera dimensión de nuestras 3 componentes, tanto con variables como individuos

fviz_pca_var(ACP,
             col.var = "contrib", # Definimos el color 
             gradient.cols = c("#76EEC6", "#66CDAA", "#458B74"),
             repel = TRUE     #Evitamos la superposición del texto
)

######################### Analisis factorial ########################

# Obtener matriz de correlación

matriz_correlaciones <- cor(dataset, use = "pairwise.complete.obs")
matriz_correlaciones

# Obtenermos la gráfica de las correlaciones 

corrplot(cor(dataset), order = "hclust", tl.col='black', tl.cex=1)

# Calculo de un objeto de correlaciones

dataset_correlaciones <- correlate(dataset)

# Obtener la gráfica de las correlaciones

rplot(dataset_correlaciones, legend = TRUE, colours = c("firebrick1", "black", "darkcyan"), print_cor = TRUE)

# Determinante de la matriz de correlaciones de las variables ingresadas

det(matriz_correlaciones)

# Obtener el cálculo de los estimadores del Test de Barlett y KMO

bartlett.test(dataset)

KMO(dataset)

factanal(dataset, factors = 2, rotation = "none")

factanal(dataset, factors = 2, rotation = "none", scores = "regression")$scores

puntuaciones <- factanal(dataset, factors = 2, rotation = "none", scores = "regression")$scores
dataset <- cbind(dataset, puntuaciones)
dataset$Factor1 <- round(((dataset$Factor1 - min(dataset$Factor1))/(max(dataset$Factor1) - min(dataset$Factor1))), 2)
dataset

hist(dataset$Factor1, freq = TRUE, main = "Gráfico de la Distribución del Factor 1", 
     xlab = "Factor 1", ylab = "Frecuencia", col = "#009ACD")

#Calcular la matriz de correlación policorica 

mat_cor <- hetcor(dataset)$correlations #matriz de correlación policorica
ggcorrplot(mat_cor,type="lower",hc.order = T)

# Obtener el cálculo de los estimadores del Test de Barlett y KMO con nuestra matriz de correlación polocorica

cortest.bartlett(mat_cor)->p_esf
p_esf$p

KMO(mat_cor)


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




# Determinar el número de factores

scree(mat_cor)
fa.parallel(mat_cor,n.obs=200,fa="fa",fm="minres")


# Rotar la matriz

rot<-c("none", "varimax", "quartimax","Promax")
bi_mod<-function(tipo){
  biplot.psych(fa(dataset,nfactors = 2,fm="minres",rotate = tipo),main = paste("Biplot con rotación ",tipo),
               col=c(2,3,4),pch = c(21,18),group = bfi[,"gender"])  
}
sapply(rot,bi_mod)


# Interpretación

modelo_varimax<-fa(mat_cor,nfactors = 5,rotate = "varimax",
                   fa="minres")
fa.diagram(modelo_varimax)

######################### Análisis de clúster ########################

principal<-function()
{
dataset <- dataset[,-1]

# Desplegar la estructura de los datos

str(dataset)

# Obtencioón de medidas estadisticas 

summary(dataset)

# Valores NA

any(is.na(dataset))

#Convertir los datos a un DataFrame

DF<- as.data.frame(scale(dataset))

# Obtención de medidas estadisticas del data frame

summary(DF)

# Aplicar la medida de distancia para obtener la matriz distancia

d_mat <- dist(DF, method= 'euclidean')

# Obtención de los grupos

grupos <- hclust(d_mat, method= "ward.D")

# Obtención del gráfico 

plot(grupos)

# Trazar la línea de corte 

l_cort <- cutree(grupos, k = 6)

# Obtención de gráfico

plot(grupos)
rect.hclust(grupos, k = 6, border = 2:6)
abline(h = 3, col = 'red')

# Extra: aplicamos Herarchical Clustering 
eucli <- hclust(d = dist(x = DF, method = "euclidean"),method = "complete")
fviz_dend(eucli, k = 6, cex = 0.6) + geom_hline(yintercept = 5.5, linetype = "dashed") +
labs(title = "Herarchical clustering", subtitle = "Distancia euclídea, Lincage complete, K=6")

######################## Algorimo de K-Means########################

# Se aplica algoritmo K-Means

grupoK2 <- kmeans(d_mat, centers = 2, nstart = 25)

# Obtención de la estructura de los datos K2

str(grupoK2)

# Impresión de los grupos 

print(grupoK2)

# Obtención de gráfico de los grupos

fviz_cluster(grupoK2, data = d_mat)

# Resultados de algoritmo K-means
set.seed(1234)
fviz_cluster(grupoK2, data = d_mat, show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, 
repel = TRUE) + labs(title = "Resultados clustering K-means") + theme_bw() + theme(legend.position = "none")

######################## Método del codo #######################
set.seed(1234)
wcss <- vector()
for ( i in 1:20)
{
  wcss[i] <- sum(kmeans(DF,i)$withinss) 
}

ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'red') + geom_line(aes(x = 1:20, y = wcss), color = 'red') +
ggtitle ("Método Del Codo ") + xlab('Cantidad de Centroides K') + ylab('WCSS')

}








