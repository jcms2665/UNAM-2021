#--------------------------------------------------------------------------------
# Tema:       Analisis Factorial Exploratorio
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      10-08-2021
# Datos:      pacientes.csv
# Github:     https://github.com/jcms2665/UNAM-2021-Multivariado


#               CONTENIDO

#     0. Entorno de trabajo
#     1. Cargar base
#     2. Ajuste del modelo
#         2.1 Normalizacion 
#         2.2 Prueba Kaiser-Meyer-Olkin
#     3. Factorial
#     4. Interpretacion
#--------------------------------------------------------------------------


#0.  Entorno de trabajo

rm(list=ls())     
graphics.off()    

library(foreign)
library(factoextra)
library(ggplot2)
library(psych)
library(dplyr)

#1. Cargar base

setwd("...")
pacientes<-read.csv("pacientes.csv", sep=",",header = TRUE)
View(pacientes)

#2. Ajuste del modelo

#2.1 Normalización
norm01 <- function(x){(x-min(x))/(max(x)-min(x))}
pacientes_norm<-data.frame(apply(pacientes,2,norm01))

apply(pacientes_norm, 2, max)%>%round(2)


#2.2 Prueba Kaiser-Meyer-Olkin
#   La prueba de 0 a 1 e indica si las correlaciones entre variables son
#   suficientemente pequenas. Valores menores a 0.5 no deben ser incluidos

KMO(pacientes_norm)


#2.3 Numero de factores (Grafica de sedimentacion)
corr<-round(cor(pacientes_norm),2)
aucor=eigen(corr)
plot(1:5,aucor$values,type="l",xlab="Factores",ylab="Autovalores")


#3. Factorial
fit <- factanal(pacientes, 2, rotation="varimax")


#4. Interpretacion 
load <- fit$loadings[,1:2]
load
