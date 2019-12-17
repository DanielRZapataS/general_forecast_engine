########################################################
##Código para extraer la periodicidad de las oficinas
########################################################

###Lectura de librerías

#install.packages("plyr")
library(plyr)   ###Tablas de frecuencias
#install.packages("tm")
library("tm")
#install.packages("SnowballC")
library("SnowballC")
##library("wordcloud")
#install.packages("RColorBrewer")
library("RColorBrewer")
#install.packages("ggplot2")
library(ggplot2) 
#install.packages("wordcloud2")
library("wordcloud2")
#install.packages("stringr")
library(stringr)
#install.packages("FactoClass")
library(FactoClass)
#install.packages("ggpubr")
library("ggpubr")
#install.packages("dplyr")
library(dplyr)   ####Para poder hacer select 
#install.packages("data.table")
library(data.table)
#install.packages("lubridate")
library(lubridate)
#install.packages("pROC")
library(pROC)
#install.packages("SDMTools")
library("SDMTools")
library(foreign)
#install.packages("VARSEDIG")
library(VARSEDIG)
#install.packages("kulife")
library(kulife)
library(MASS)
#install.packages("ROCR")
library(ROCR)
library(caTools)
#install.packages("clusterSim")
#library(clusterSim)
#install.packages("fastDummies")
library(fastDummies)
#install.packages("neuralnet")
library(neuralnet)
#install.packages("xgboost")
library("xgboost")
#install.packages("pdp")
##install.packages('vip')  ###No se puede instalar por la versi?n
#install.packages("pdp")
#library(pdp)      # for partial dependence plots (PDPs)
#install.packages("vip")
#library(vip)      # for variable importance plots (VIPs)
library(tidyverse)
library(FactoClass)
library(MASS)
library("factoextra")
library("FactoMineR")
library("factoextra")
library("tm")
library("SnowballC")
#install.packages("wordcloud")
library("wordcloud")
library("RColorBrewer")
library(stringr)


###############################
##Comportamiento de horario
################################

##################################
###Lectura de datos
##################################

#data <- fread(file = "Y:/V2.0/data/staging/staging_201905.csv")

comportamiento_horario <- function(path, periodo){

setwd("Y:/V2.0/data/comportamiento_horarios")   ###Directorio destino 
path <- "Y:/V2.0/data/staging"    ##Directorio origen
periodo<-"201904"

staging <- fread(file = paste0(path,"/staging_", periodo, ".csv" )) 
#Convertir cnl_fecha a formato fecha
staging[, cnl_fecha:=as.Date(cnl_fecha) ]
##Agrupación por la variable llave y fecha
tabla_conteo_semana<- staging[, .N, by =.(llave, cnl_fecha)]     
##Construcción de la variable dia de la semana Domingo=1 , Lunes=2, Martes=3, Miercoles=4 , Jueves=5, Viernes=6, Sábado=7
tabla_conteo_semana[, week_day:=paste0("d_",as.factor(lubridate::wday(cnl_fecha))) ]    
##Construcción de tabla que permite identificar los días que ha trabajado una oficina en el mes
tabla_conteo<-tabla_conteo_semana[, .N, by = .(llave, week_day)] %>% dcast(llave ~ week_day)    
###Definir funcion reemplazar
reemplazar <- function(x) ifelse(is.na(x)==T,0, ifelse(x>=2,1,0))
##Aplicar funcion reemplazar a los datos de tabla_conteo
tabla_conteo[,2:ncol(tabla_conteo)] <- lapply(tabla_conteo[,2:ncol(tabla_conteo)], reemplazar)
###Obtener los días que trabaja la oficina en el mes
aux <- copy(tabla_conteo)
aux[, llave := NULL]
resultado <- c()
for (i in 1:nrow(aux)){
  fila <- as.vector(aux[i])
  resultado[i] <- paste(names(aux)[as.logical(fila)], collapse = ",")
}
tabla_conteo[, resultado := resultado]
rm(aux, resultado, fila)
##Para definir la variable periodicidad
tabla_conteo[, periodicidad:= rowSums(tabla_conteo[,2:(ncol(tabla_conteo)-1)   ])]












#ok
tabla_conteo<-tabla_conteo[periodicidad!=0] 



###Función para reemplazar 2 por el nombre de la variable 

tabla_conteo[,2:(ncol(tabla_conteo)-1)] <- lapply(tabla_conteo[,2:(ncol(tabla_conteo)-1)], reemplazar2)

head(tabla_conteo)

for (x in 2:(ncol(tabla_conteo)-1)){
  tabla_conteo[,x:=ifelse(x==1,x,"")]
}






tabla_conteo[,3:=d_3]


head(tabla_conteo)





###Definir funcion reemplazar

reemplazar_por_dia <- function(x) ifelse(x==1, names(x), "na")


for(i in 2: ))

reemplazar_por_dia <-function(x){ 
ifelse(tabla_conteo[1,1]==1, names(tabla_conteo[1,1]), "")
}




##Aplicar funcion reemplazar a los datos de tabla_conteo
tabla_conteo[,2:ncol(tabla_conteo)] <- lapply(tabla_conteo[,2:ncol(tabla_conteo)], reemplazar_por_dia)



DF$results <- names(tabla_conteo[-1])






colnames(tabla_conteo[1, ])[apply(tabla_conteo, 2, function(u) any(u==1))]



col = colnames(tabla_conteo)[apply(tabla_conteo, 2, function(u) any(u==1))]
> col



tabla_conteo[, dias:=  ]





reemplazar <- function(x) ifelse((x)==1,1, ifelse(x>=2,1,0))
tabla_conteo[,2:ncol(tabla_conteo)] <- lapply(tabla_conteo[,2:ncol(tabla_conteo)], reemplazar)


##Definir función reemplazar 2

reemplazar <- function(x) ifelse((x)==1,1, ifelse(x>=2,1,0))

tabla_conteo[,dia1:= ifelse(d_1==1,1,99)]
tabla_conteo[,dia2:= ifelse(d_2==1,2,99)]
tabla_conteo[,dia3:= ifelse(d_3==1,3,99)]
tabla_conteo[,dia4:= ifelse(d_4==1,4,99)]
tabla_conteo[,dia5:= ifelse(d_5==1,5,99)]
tabla_conteo[,dia6:= ifelse(d_6==1,6,99)]
tabla_conteo[,dia7:= ifelse(d_7==1,7,99)]
###primer dia a la semana que trabaja la oficina 
tabla_conteo[,primer_dia:= pmin(dia1, dia2, dia3, dia4, dia5, dia6, dia7)]

##Variables auxiliares para construir el último día de la semana que trabaja una oficina
tabla_conteo[,dia1:= ifelse(d_1==1,1,0)]
tabla_conteo[,dia2:= ifelse(d_2==1,2,0)]
tabla_conteo[,dia3:= ifelse(d_3==1,3,0)]
tabla_conteo[,dia4:= ifelse(d_4==1,4,0)]
tabla_conteo[,dia5:= ifelse(d_5==1,5,0)]
tabla_conteo[,dia6:= ifelse(d_6==1,6,0)]
tabla_conteo[,dia7:= ifelse(d_7==1,7,0)]

###último dia a la semana que trabaja la oficina
tabla_conteo[,ultimo_dia:= pmax(dia1, dia2, dia3, dia4, dia5, dia6, dia7)]

##Seleccionar unicamente las variables necesarias 
tabla_conteo<- tabla_conteo[, c("llave","periodicidad","primer_dia","ultimo_dia"), with=FALSE]
tabla_conteo[, fecha:=as.Date(paste0(as.character(periodo), '01'), format='%Y%m%d')]
write.csv(tabla_conteo, paste0("comportamiento_horario_", periodo, ".csv"))
}

#########################################
##Aplicar función a todos los datos
#########################################


path <- "Y:/V2.0/data/staging"

comportamiento_horario(path, 201904)






                             









































 