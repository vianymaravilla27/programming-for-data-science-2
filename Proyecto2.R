"Proyecto 2
Castillo Reyes Eduardo Armando
Maravilla P�rez Vianey
V�zquez Portuguez Jos� Antonio"

# Librerias
library(readr)
library(nortest)
library(ggplot2)
library(DataCombine)
library(tidyverse)
library(janitor)
library(dplyr)
library(readr)
library(nortest)
library(kableExtra)
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(modeest)
library(plotrix)

#LECTURA DE DATOS

df <- read_csv("C:/Users/viane/Desktop/ESCOM/3.-TERCER SEMESTRE/PROGRAMACION PARA LAS CIENCIAS DE DATOS/Dummy Data HSS.csv") # Leemos el archivo csv


#LIMPIEZA DE DATOS

clean<-na.omit(df) # Limpiamos los datos que tengan NA

tv <- clean$TV
radio <- clean$Radio
sales <- clean$Sales

# MEDIA DE DATOS

media.tv <- mean(clean$TV)
media.radio <- mean(clean$Radio)
media.socials <- mean(clean$"Social Media")
media.ventas <- mean(clean$Sales)



# MEDIANA DE DATOS

mediana.tv <- median(clean$TV)
mediana.radio <- median(clean$Radio)
mediana.socials <- median(clean$"Social Media")
mediana.ventas <- median(clean$Sales)

# MODA DE DATOS

moda.tv <- mlv(tv, method= "mfv")
moda.radio <- mlv(radio, method = "mfv")
moda.sales <- mlv(sales, method = "mfv")
moda.inf <- mlv(clean$"Social Media", method = "mfv")



#CUARTILES DE DATOS

cuart.tv <- quantile(tv)
cuart.rad <- quantile(radio)
cuart.ventas <- quantile(sales)
cuart.influ <- quantile(clean$"Social Media")

# DECILES DE DATOS

decil.tv <- quantile(tv, prob=seq(0,1, length=11))
decil.rad <- quantile(radio, prob=seq(0,1, length=11))
decil.ventas <- quantile(sales, prob=seq(0,1, length=11)) 
decil.influ <- quantile(clean$"Social Media", prob=seq(0,1, length=11))

# PERCENTILES DE DATOS

percentil.tv <- quantile(tv, prob=seq(0,1, length=101))
percentil.rad <- quantile(radio, prob=seq(0,1, length=101))

percentil.ventas <- quantile(sales, prob=seq(0,1, length=101))
percentil.influ <- quantile(clean$"Social Media", prob=seq(0,1, length=101))


# Desviacion de los datos

std.tv <- sd(tv)
std.radio <- sd(clean$Radio)
std.socials <- sd(clean$`Social Media`)
std.sales <- sd(clean$Sales)

# Varianza de los datos

var.TV <- var(tv)
var.Radio <- var(clean$Radio)
var.socials <- var(clean$`Social Media`)
var.sales <- var(clean$Sales)

# Rango de los datos

rang.TV <- range(clean$TV, na.rm = FALSE)
rang.Radio <- range(clean$Radio, na.rm = FALSE)
rang.socials <- range(clean$`Social Media`, na.rm = FALSE)
rang.sales <- range(clean$Sales, na.rm = FALSE)

#Rango de variación 

rangi.tv <- rang.TV[2] - rang.TV[1]
rangi.Radio   <- rang.Radio[2] - rang.Radio[1]
rangi.socials <- rang.socials[2] - rang.socials[1]
rangi.sales <- rang.sales[2] - rang.socials[1]


# Coeficiente de variacion || relación entre el tamaño de la media y la variabilidad de la variable

coef_var <- function(x, na.rm = FALSE) {
  sd(x, na.rm=na.rm) / mean(x, na.rm=na.rm)
}

coef.tv <- coef_var(tv, na.rm = T)
coef.radio <- coef_var(clean$Radio, na.rm = T)
coef.socials <- coef_var(clean$`Social Media`, na.rm = T)
coef.sales <- coef_var(clean$Sales, na.rm = T)



# Covarianza de presupuesto respecto a ventas
# Utilizaremos esta funcion para conocer la relacion entre el dinero asignado a cada tipo de campaña y las ventas que genera

cov(clean$TV, clean$Sales)
cov(clean$Radio, clean$Sales)
cov(clean$`Social Media`, clean$Sales)



# Matriz de covarianza

mat.cov <- rcorr(as.matrix(clean))
correlacion <- round(cor(clean), 1)
corrplot(correlacion, method="number", type="upper")

#Coeficiente de correlación lineal (medir la fuerza y la dirección de la relación lineal entre dos variables.)

corrTV <- cor(clean$TV, clean$Sales)
corrRad <- cor(clean$Radio, clean$Sales)
corrSoc <- cor(clean$`Social Media`, clean$Sales)

plot(clean$TV, clean$Sales, pch = 19, col = "blue")
abline(lm(clean$TV~clean$Sales), col = "red", lwd = 3)
text(paste("Correlación:", round(cor(clean$TV, clean$Sales), 2)), x = 40, y = 250)

plot(clean$Radio, clean$Sales, pch = 19, col = "blue")
abline(lm(clean$Radio~clean$Sales), col = "red", lwd = 3)
text(paste("Correlación:", round(cor(clean$Radio, clean$Sales), 2)), x = 40, y = 150)

plot(clean$`Social Media`, clean$Sales, pch = 1, col = "blue")
abline(lm(clean$`Social Media`~clean$Sales), col = "red", lwd = 3)
text(paste("Correlación:", cor(clean$`Social Media`, clean$Sales), 2), x = 14, y = 100)

# Coeficiente de pearson

#Radio
dat1 <- data.frame(clean$TV, clean$Sales)
chart.Correlation(dat1)

#TV
dat2 <- data.frame(clean$Radio, clean$Sales)
chart.Correlation(dat2)

#Sociales
dat3 <- data.frame(clean$`Social Media`, clean$Sales)
chart.Correlation(dat3)

# Normal CONTINUA
 
# TV
normTV<- rnorm(clean$TV)
densTV <- dnorm(normTV)

library(ggplot2)
ggplot(data.frame(x = normTV, y = densTV)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Random Normal Variable", y = "Densidad")

ggplot(data.frame(x = normTV), aes(x = x)) +
  geom_histogram(binwidth = 0.1) +
  labs(x = "Random Normal Variable", y = "Frecuencia")

distNTV <- pnorm(c(-2,2), media.tv, std.tv, lower.tail = TRUE) 

# RADIO

normRad<- rnorm(clean$Radio)
densRad <- dnorm(normRad)

library(ggplot2)
ggplot(data.frame(x = normRad, y = densRad)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Random Normal Variable", y = "Densidad")

ggplot(data.frame(x = normRad), aes(x = x)) +
  geom_histogram(binwidth = 0.1) +
  labs(x = "Random Normal Variable", y = "Frecuencia")

DistNRad <- pnorm(c(-2,2), media.radio, std.radio, lower.tail = TRUE) 

# SOCIALES

betaSoc<- rbeta(clean$`Social Media`,1,5)
densSoc <- dbeta(betaSoc,2,5)

library(ggplot2)
ggplot(data.frame(x = betaSoc, y = densSoc)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Random Normal Variable", y = "Densidad")

ggplot(data.frame(x = betaSoc), aes(x = x)) +
  geom_histogram(binwidth = 0.1) +
  labs(x = "Random Normal Variable", y = "Frecuencia")

DistBRad <- pbeta(c(-2,2), media.socials, std.socials, lower.tail = TRUE)

# Error estandar de estimacion || calculamos el error std de estimacion

ErrStdTV <- std.error(clean$TV)
ErrStdRad <- std.error(clean$Radio)
ErrStdSoc <- std.error(clean$`Social Media`)
ErrStdVent <- std.error(clean$Sales)

# Minimos cuadrados Y Regresion lineal simple

corrData <- cor(clean) # Buscamos la correlacion y la guardamos acá 

aux <- data.frame(clean$TV, clean$Sales)
regressionTV  <- lm(clean.TV ~ clean.Sales , data = aux)
resumenTV <- summary(regressionTV)

plot(clean$TV, clean$Sales, xlab='TV', ylab='SALES')
abline(regressionTV)

regressionRadio  <- lm(radio ~ sales, data = clean)
resumenRad <- summary(regressionRadio)

plot(clean$Radio, clean$Sales, xlab='RADIO', ylab='SALES')
abline(regressionRadio)

regressionSoc  <- lm(`Social Media` ~ sales, data = clean)
resumenSoc <- summary(regressionSoc)

plot(clean$`Social Media`, clean$Sales, xlab='SOCIAL MEDIA', ylab='SALES')
abline(regressionSoc)


# IMPRESION DE DATOS

  paste("La media de presupuesto de TV (En millones) es: ", media.tv)
  paste("La media de presupuesto de Radio (En millones) es: ",media.radio)
  paste("La media de presupuesto de Redes Sociales (En millones) es: ",media.socials)
  paste("La media de ventas (En millones) es: ",media.ventas)


  paste("La mediana de presupuesto de TV (En millones) es: ", mediana.tv)
  paste("La mediana de presupuesto de Radio (En millones) es: ",mediana.radio)
  paste("La mediana de presupuesto de Redes Sociales (En millones) es: ",mediana.socials)
  paste("La mediana de ventas (En millones) es: ",mediana.ventas)

  paste("La moda de presupuesto de TV (En millones) es: ", moda.tv)
  paste("La moda de presupuesto de Radio (En millones) es: ",moda.radio)
  paste("La moda de presupuesto de Redes Sociales (En millones) es: ",moda.inf)
  paste("La moda de ventas (En millones) es: ",moda.sales)

  print("LOS CUARTILES POR CADA CATEGORIA")
  paste(cuart.tv)
  paste(cuart.rad)
  paste(cuart.influ)
  paste(cuart.ventas)

  print("LOS DECILES POR CADA CATEGORIA")
  paste(decil.tv)
  paste(decil.rad)
  paste(decil.influ)
  paste(decil.ventas)

  print("LOS PERCENTILES POR CADA CATEGORIA")
  paste(percentil.tv)
  paste(percentil.rad)
  paste(percentil.influ)
  paste(percentil.ventas)

  print("LOS Rangos POR CADA CATEGORIA")
  paste(rang.TV)
  paste(rang.Radio)
  paste(rang.socials)
  paste(rang.sales)



