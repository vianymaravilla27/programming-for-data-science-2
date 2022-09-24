" Ejercicio 2
Se tiene un dataset de camas de hospitalización por covid19 en CDMX y EDOMEX
Se aplicará el análisis de componentes principales a los siguientes datos, así mismo las mismas intrucciones y pasos"

# Primero importamos las librerias que vayamos a ocupar, para esto, se hizo una instalación para posteriormente hacer 
library(readr)
library(factoextra)

#Mandamos a leer nuestro dataset, esto primero lo debemos de descargar para poderlo mandar a llamar con la siguiente instrucción
dataset <- read_csv("C:/Users/viane/Desktop/ESCOM/3.-TERCER SEMESTRE/PROGRAMACION PARA LAS CIENCIAS DE DATOS/Hospitalización.csv")


#Nomralizacion de los datos
datos_Centrados<- dataset
datos_Centrados$hospitalizados_totales <- dataset$hospitalizados_totales      - mean(dataset$hospitalizados_totales)
datos_Centrados$intubados_totales      <- dataset$intubados_totales           - mean(dataset$intubados_totales)
datos_Centrados$hospitalizados_edomex  <- dataset$hospitalizados_edomex       - mean(dataset$hospitalizados_edomex)
datos_Centrados$hospitalizados_cdmx    <- dataset$hospitalizados_cdmx         - mean(dataset$hospitalizados_cdmx)
datos_Centrados$intubados_cdmx         <- dataset$intubados_cdmx              - mean(dataset$intubados_cdmx)
datos_Centrados$intubados_edomex       <- dataset$intubados_edomex            - mean(dataset$intubados_edomex)
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
rownames(pc_scores) <- c("PC 1", "PC 2", "PC 3", "PC 4", "PC 5", "PC 6", "PC 7", "PC 8")
pc_scores


#Los transformamos nuevamente para que los datos esten en modo tabla
t(pc_scores)
datos_recuperados <- t(eigen_a$vectors %*% pc_scores)
datos_recuperados[, 1] <- datos_recuperados[, 1] + mean(dataset$X1)
datos_recuperados[, 2] <- datos_recuperados[, 2] + mean(dataset$X2)
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


