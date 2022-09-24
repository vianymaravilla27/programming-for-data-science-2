#Examen 3
#Vianey Maravilla Pérez 

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

############################ Analisis Multivariante ########################

######################### Analisis de componentes principales ########################

#Mandamos a leer nuestro dataset, esto primero lo debemos de descargar para poderlo mandar a llamar con la siguiente instrucción

dataset <- read_csv("C:/Users/viane/Desktop/ESCOM/3.-TERCER SEMESTRE/PROGRAMACION PARA LAS CIENCIAS DE DATOS/Examen.csv")
dataset

#Normalizacion de los datos

datos_Centrados<- dataset
datos_Centrados$instr         <- dataset$instr            - mean(dataset$instr)
datos_Centrados$class         <- dataset$vclass           - mean(dataset$class)
datos_Centrados$nb.repeat     <- dataset$nb.repeat        - mean(dataset$nb.repeat)
datos_Centrados$attendance    <- dataset$attendance       - mean(dataset$attendance)
datos_Centrados$difficulty    <- dataset$difficulty       - mean(dataset$difficulty)
datos_Centrados$Q1            <- dataset$Q1               - mean(dataset$Q1)
datos_Centrados$Q2            <- dataset$Q2               - mean(dataset$Q2)
datos_Centrados$Q3            <- dataset$Q3               - mean(dataset$Q3)
datos_Centrados$Q4            <- dataset$Q4               - mean(dataset$Q4)
datos_Centrados$Q5            <- dataset$Q5               - mean(dataset$Q5)
datos_Centrados$Q6            <- dataset$Q6               - mean(dataset$Q6)
datos_Centrados$Q7            <- dataset$Q7               - mean(dataset$Q7)
datos_Centrados$Q8            <- dataset$Q8               - mean(dataset$Q8)
datos_Centrados$Q9            <- dataset$Q9               - mean(dataset$Q9)
datos_Centrados$Q10           <- dataset$Q10              - mean(dataset$Q10)
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
rownames(pc_scores) <- (c ("PC ","PC 1", "PC 2", "PC 3", "PC 4", "PC 5", "PC 6", "PC 7", "PC 8","PC 9 ","PC 10"))
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


######################### Análisis de clúster-Jerárquico ########################

principal<-function(){
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
  
  ######################### Intervalo de confianza ########################
  str(dataset$difficulty)
  
  mean(dataset$difficulty)
  media <- mean(dataset$cdifficulty) #Calculamos la media
  
  z <- 1.96 #Confinza al 95%
  
  length(dataset$difficulty)
  
  n <- length(dataset$cdifficulty) #Identificar el numero de casos
  
  sd(dataset$difficulty)
  desviacion <- sd(dataset$difficulty) #Desviación estandar
  
  errorst <- desviacion/sqrt(n) #Error estandar
  margen_error <- z*errorst #Nivel de confianza al 95%
  
  lim_inf <- media - margen_error #Limite inferior
  
  lim_sup <- media + margen_error #Limite superior
  
  inter_m <- data.frame(n, media, desviacion, z, errorst, lim_inf, lim_sup) #Visualizando los datos
  inter_m
  
cat("Intervalo de confianza de la dificultad\n", t.test(x=dataset$difficulty, conf.level=0.95)$conf.int)
  
  
  
  
  