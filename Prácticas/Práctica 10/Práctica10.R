# Vianey Maravilla Pérez 3AM1
# Práctica 10
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

principal <- function()
{
  
# Cargar los datos del archivo
#read_csv("C:/Users/viane/Desktop/ESCOM/3.-TERCER SEMESTRE/PROGRAMACION PARA LAS CIENCIAS DE DATOS/Absenteeism_at_work.csv")
Absenteeism <- read.xlsx (xlsxFile='C:/Users/viane/Desktop/ESCOM/3.-TERCER SEMESTRE/PROGRAMACION PARA LAS CIENCIAS DE DATOS/Absenteeism_at_work.xlsx',
            sheet = 'Absenteeism_at_work')
View(Absenteeism)
Absenteeism <- Absenteeism[, -1]
  
# Explorar los datos
# Desplegar la estructura de los datos
str (Absenteeism)

  
# Obtención de medidas estadísticas
summary(Absenteeism)
  
# Valores NA
any(is.na(Absenteeism))
  
# Convertir los datos a un DataFrame
Absen<- as.data.frame(scale(Absenteeism))

# Obtención de medidas estadísticas del DF
summary(Absen)
  
# Se aplica la medida de distancia para obtener la matriz distancia
dist_mat <- dist(Absen, method = 'euclidean')

# Obtención de los grupos
grupos <- hclust(dist_mat, method = "ward.D")
plot(grupos)
  
# Trazar la línea de corte y mostrar rectangulo en los grupos 
lineaCorte <- cutree(grupos, k=3)
  
plot(grupos)
rect.hclust(grupos, k = 3, border = 2:6)
abline(h = 3, col = 'red')

# Extra: aplicamos Herarchical Clustering 
eucli <- hclust(d = dist(x = Absen, method = "euclidean"),method = "complete")
fviz_dend(eucli, k = 3, cex = 0.6) + geom_hline(yintercept = 5.5, linetype = "dashed") +
labs(title = "Herarchical clustering", subtitle = "Distancia euclídea, Lincage complete, K=3")

######################## Se aplica algoritmo K-Means #######################
grupoK2 <- kmeans(dist_mat, centers = 2, nstart = 25)
  
# Obtención de la estructura de los datos K2
str(grupoK2)
  
# Impresión de los grupos 
print(grupoK2)
  
# Obtención de gráfico de los grupos
fviz_cluster(grupoK2, data = dist_mat)

# Resultados de algoritmo K-means
set.seed(1234)
fviz_cluster(grupoK2, data = dist_mat, show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, 
             repel = TRUE) + labs(title = "Resultados clustering K-means") + theme_bw() + theme(legend.position = "none")

####################### Método del codo #######################
set.seed(1234)
wcss <- vector()
for ( i in 1:20)
{
wcss[i] <- sum(kmeans(Absen,i)$withinss) 
}


ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'red') + geom_line(aes(x = 1:20, y = wcss), color = 'red') +
ggtitle ("Método Del Codo ") + xlab('Cantidad de Centroides K') + ylab('WCSS')

 
}
