
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


serie_temporal <- fread(file= "Y:/V2.0/data/serie_temporal/serie_temporal_201905.csv")  

##copiar oficinas 
horarios_aux <- copy(serie_temporal)
#Convertir fecha de la transacción  a formato fecha
horarios_aux[, fecha:=as.Date(fecha) ]
#Extraer el día de la semana 
horarios_aux[, weekday := wday(fecha)]
#construir la variable dia mes
horarios_aux[, year_month:=format(fecha, "%Y-%m")]

#Agrupar por la variable llave, año_mes y día
horarios_aux <- horarios_aux[, .N, by = .(llave,year_month, weekday)]

############################################
##Incluir aquí el diccionario de festivos
############################################


#festivos <- fread("Y:/V2.0/data/diccionarios/festivos.csv")
#festivos[, fecha := as.Date(festivos, format = "%d/%m/%Y")]
##festivos[, weekday := wday(fecha, label = T)]
#festivos[, year_month := substr(fecha, 1,7)]
#festivos[, year_month_wday := paste(year_month, weekday, sep = "-")]

#count_holidays <- festivos[, .(num_festivos = .N), by = year_month_wday]

#count_holidays <- count_holidays[num_festivos > 1]



horarios_aux[, dummy := ifelse(N >= 2, 1, 0)]

horarios_aux <- dcast(horarios_aux, llave + year_month ~ weekday, value.var = "dummy" )

#rteemplazar NA's con 0
reemplazar <- function(x) ifelse(is.na(x)==T,0, x)
##Aplicar funcion reemplazar a los datos de tabla_conteo
horarios_aux[,3:ncol(horarios_aux)] <- lapply(horarios_aux[,3:ncol(horarios_aux)], reemplazar)

###Obtener los días que trabaja la oficina en el mes
aux <- copy(horarios_aux)
aux[, llave := NULL]
aux[, year_month := NULL]
resultado <- c()
for (i in 1:nrow(aux)){
  fila <- as.vector(aux[i])
  resultado[i] <- paste(names(aux)[as.logical(fila)], collapse = ",")
}
horarios_aux[, dias := resultado]
rm(aux, resultado, fila)

##Definir periodicidad , no se necesita por que esta periodicidad es del horario original
#horarios_aux[, periodicidad:= rowSums(horarios_aux[,3:(ncol(horarios_aux)-1)   ])]


##Oficinas actuales
oficinas_actuales <- horarios_aux[year_month == "2019-05", llave]
##Seleccionar únicamente las oficinas cuyas oficinas están activas en 2019-05
horarios <- horarios_aux[llave %in% oficinas_actuales]

##Para obtener el número de horarios distintos.
#group by llave y conteo periodicidad
#analisis_horarios <-horarios[, .(horarios_distintos = uniqueN(dias)), by = llave]
##merge con la data de horarios
#horarios <- merge(horarios, analisis_horarios, by = c("llave"), all.x = T)

##Ordnar los datos
horarios <- horarios[order(llave,year_month)]
##Para ver algunos horarios 
#unique(horarios[horarios_distintos==2]$llave)


#########################
###Segundo análisis
#########################

##Horarios del último mes 
horario_ultimo_mes <- horarios[year_month == "2019-05", .(llave, dias)]

#horario_ultimo_mes[, horario_ult_mes := 1]
horario_ultimo_mes[,dias2:=dias]
horario_ultimo_mes[,  dias:=NULL]

horarios <- merge(horarios, horario_ultimo_mes, by = c("llave"), all.x = T)

##se construye una variable indicadora que significa lo siguiente:
##si el horario actual está contenido en el horario de la fecha correspondiente, entonces
##asignar el horario actual, sino, asignar el horario
horarios[,horario_definitivo:=ifelse(str_detect(dias,dias2)==T,dias2, dias)]


###Generar variable indicadora que dice: si horario_definitivo es igual a dias entonces 1 sino 0.
horarios[,indicadora:=ifelse(dias==horario_definitivo,1,0)]

##Calculo de la variable periodicidad 2

horarios[,periodicidad2:=str_count(horario_definitivo, '\\d+')]




















##Para revisar este caso en específico
#unique(horarios[horarios_distintos==2]$llave)
##horarios[llave== 2100112]





##Borrar el horario del últimpo mes
rm(horario_ultimo_mes)  
horarios[is.na(horario_ult_mes), horario_ult_mes := 0]
##Comparar dias2 con dias


















horarios <- horarios[order(llave, year_month)]

horarios[llave %in% horarios[horario_ult_mes == 0, llave][27]]

horarios[, horario_ult_mes_lead := shift(horario_ult_mes,-1, fill = 0, type = "lead"), by = llave]

horarios[, cambio_horario :=  horario_ult_mes - horario_ult_mes_lead]

cambios_horario <- horarios[, .(cambios_horario = sum(cambio_horario)), by = llave]

cambios_horario[cambios_horario > 1]

horarios[llave == analisis_horarios[horarios_distintos == 3, llave][3]]


fwrite(horarios, "data/insumos_pronostico/horarios/resumen_horarios.csv")

