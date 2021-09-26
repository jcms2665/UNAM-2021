#--------------------------------------------------------------------------------
# Tema:       Analisis Factorial para variables categoricas
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      10-08-2021
# Datos:      Latinobarometro_2018_Esp_R_v20190303.Rds
# Github:     https://github.com/jcms2665/UNAM-2021-Multivariado


#               CONTENIDO

#     0. Entorno de trabajo
#     1. Cargar base
#     2. Preparacion de la base de datos
#     3. Correlacion policorica
#     4. Ajuste del modelo
#--------------------------------------------------------------------------


#0.  Entorno de trabajo

rm(list=ls())     
graphics.off()    

library(foreign)
library(ggplot2)
library(psych)
library(dplyr)
library(psych)
library(tidyr)


#1. Cargar base

setwd("...")
latino <- readRDS("Latinobarometro_2018_Esp_R_v20190303.Rds")



# Variables:

# P15STGBSC.A --- Confianza en el Ejercito
# P15STGBSC.B --- Confianza en la Policia
# P15STGBSC.C --- Confianza en la Iglesia
# P15STGBSC.D --- Confianza en el Congreso
# P15STGBSC.E --- Confianza en el Gobierno

# Respuestas:
# 1.- Mucha confianza
# 2.- Algo de confianza
# 3.- Poca confianza
# 4.- Ninguna confianza
# -1-.- No sabe
# -2-.- No responde
# -4-.- No preguntada


#2. Arreglo de la base de datos

# Filtrar por pais: Mexico y Brasil
dat<-latino%>%filter(as.numeric(IDENPA)==862 | as.numeric(IDENPA)==76)


# Filtrar variables
var<-c("P15STGBSC.A","P15STGBSC.B","P15STGBSC.C","P15STGBSC.D", "P15STGBSC.E")
dat<-dat[,var]
names(dat)<-c("Ejercito","Policia","Iglesia","Congreso","Gobierno")


# Quitar respuestas invalidas
dat[dat <=0] <- NA
dat<-dat%>%drop_na()

# Tabulados

knitr::kable(table(dat$Ejercito))
knitr::kable(table(dat$Iglesia))


#3. Correlacion policorica 
poly_cor = polychoric(dat)
rho = poly_cor$rho
cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Correlacion tetracorica", show.legend = FALSE)


#4. Ajuste del modelo
poly_model = fa(dat, nfactor=2, cor="poly", fm="mle", rotate = "none")
poly_model$loadings
fa.diagram(poly_model)


























