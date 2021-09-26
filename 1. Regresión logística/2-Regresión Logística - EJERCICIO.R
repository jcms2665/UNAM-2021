#--------------------------------------------------------------------------------
# Tema:       Regresion logistica
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      10-08-2021
# Datos:      fumadores.csv
# Github:     https://github.com/jcms2665/UNAM-2021-Multivariado


#               CONTENIDO

#       0.  Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Cargar base de datos
#       3. Seleccionar covariables
#       4. Variables
#           4.1 Variable dependiente
#           4.2 Covariables
#       5. Ajuste del modelo
#       6. Interpretacion
#--------------------------------------------------------------------------------

# PREGUNTA DE INVESTIGACION: Que factores influyen para que una persona fume?
# 


#0.  Preparar entorno de trabajo

rm(list=ls())                  
options(warn=-1)              


#1. Cargar librerias

library(foreign)                 
library(dplyr) 
library(stats) 

#2. Cargar base de datos

base <-read.csv(".../fumadores.csv")
View(base)

# variables
# fuma = fumadores
#           1 = Si 
#           0 = No 
# edad = Edad de las personas
#           1 = Niñno 
#           2 = Joven  
#           3 = Adulto 
# civil = Estado civil
#           1 = soltero  
#           2 = casado 
# actividad = Tipo de actividad
#           1 = trabaja  
#           2 = estudia 


#3. Etiquetar variables

base$fuma<-factor(base$fuma, levels = c(0,1), labels = c("No","Si"))
base$edad<-factor(base$edad, levels = c(1,2,3), labels = c("Nino","Joven","Adulto"))
base$civil<-factor(base$civil, levels = c(1,2), labels = c("soltero","casado"))
base$actividad<-factor(base$actividad, levels = c(1,2), labels = c("trabaja","estudia"))


#4. Seleccionar variables (2 variables)

fumadores<-base[c("fuma","___","___")]  

rm("base")



#4.1. Tipo de datos

sapply(fumadores, class) 


#4.1 Variable dependiente

table(fumadores$fuma)

#4.2 Covariables

table(fumadores$___)
table(fumadores$___)


#5. Ajuste del modelo


regresion <- glm(fuma ~___+___, 
                 data = fumadores, family = "binomial")
summary(regresion)


#6. Interpretación

momios<-exp(coefficients(regresion))%>%round(digits = 4)%>%data.frame()
momios
