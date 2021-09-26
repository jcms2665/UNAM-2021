#--------------------------------------------------------------------------------
# Tema:       Analisis Discriminante
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      10-08-2021
# Datos:      Alumnos.csv
# Github:     https://github.com/jcms2665/UNAM-2021-Multivariado


#               CONTENIDO

#     0. Entorno de trabajo
#     1. Cargar base
#     2. Aplicar el modelo
#     3. Nueva observacion
#     4. Prediccion
#--------------------------------------------------------------------------



#0.  Entorno de trabajo
rm(list=ls())     
graphics.off()    

library(foreign)
library(ggplot2)
library(MASS)

setwd("...")
Alumnos<-read.csv("Alumnos.csv")
View(Alumnos)

#2. Aplicar el modelo

Alumnos$aprobado<-factor(Alumnos$aprobado, levels = c(0,1), labels = c("Reprueba","Aprueba"))

dis=lda(aprobado~hrs_estudio+hrs_fiesta+hrs_camino_escuela+calificacion_mate, data=Alumnos,prior=c(0.5,0.5))
dis

#3. Nueva observacion
#   Supongamos que entra un alumno nuevo. Y que:
#     hrs_estudio =4.3
#     hrs_fiesta  =1.5
#     hrs_camino  =1.3
#     calif_mate  =8.0

nuevo.alumno=rbind(c(4.3,1.5,1.3,8.0))

colnames(nuevo.alumno)=colnames(Alumnos[,2:5])
nuevo.alumno=data.frame(nuevo.alumno)

#4. Prediccion
predict(dis,newdata =nuevo.alumno)





