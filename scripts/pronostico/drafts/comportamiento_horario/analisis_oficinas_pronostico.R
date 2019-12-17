


serie_temporal <- list.files(ts_path, full.names = T) %>% fread()
serie_temporal[, fecha := as.Date(fecha)]
# encontrar los huecos de las oficinas 

serie_temporal_aux <- serie_temporal[order(llave, fecha)]
serie_temporal_aux[, fecha_lag := shift(fecha, 1, "lag"), by = llave]
serie_temporal_aux <- na.omit(serie_temporal_aux)
serie_temporal_aux[, diferencia := fecha - fecha_lag]

umbral <-  10
oficinas <- serie_temporal_aux[diferencia > umbral, unique(llave)]


oficinas_huecos <- serie_temporal_aux[diferencia > umbral]
fwrite(oficinas_huecos, "resultados/graficas/series_huecos/oficinas_huecos.csv")

plot_list <- list()
for(i in 1:length(oficinas)){
  cortes <- serie_temporal_aux[llave == oficinas[i] & diferencia > umbral, .(fecha, fecha_lag)]
  cortes <- c(cortes$fecha, cortes$fecha_lag)
  
  p <-  serie_temporal_aux[llave == oficinas[i]] %>% ggplot(aes(x = fecha, y = txs )) + geom_line() +
    geom_vline(xintercept = cortes, linetype="dashed", 
               color = "red") +
  labs(title = oficinas[i])
  plot_list[[i]] <- p
  # ggsave(plot = p, filename = paste0("resultados/graficas/series_huecos/oficina_", oficinas[i], ".png"))
  
}

# festivos 
festivos <- fread("data/diccionarios/festivos.csv")
festivos[, fecha := as.Date(festivos, format = "%d/%m/%Y")]
festivos[, weekday := wday(fecha, label = T)]
festivos[, year_month := substr(fecha, 1,7)]
festivos[, year_month_wday := paste(year_month, weekday, sep = "-")]

count_holidays <- festivos[, .(num_festivos = .N), by = year_month_wday]

count_holidays <- count_holidays[num_festivos > 1]

# encontrar los horarios de oficinas 
horarios_aux <- copy(serie_temporal)
horarios_aux[, weekday := wday(fecha, label = T)]
horarios_aux[, year_month := substr(fecha, 1,7)]
horarios_aux <- horarios_aux[,  .N, by = .(llave,year_month, weekday)]
horarios_aux[, year_month_wday := paste(year_month, weekday, sep = "-")]

horarios_aux[, dummy := ifelse(N >= 2, 1, 0)]

horarios_aux[year_month_wday  %in% count_holidays$year_month_wday, dummy := ifelse(N >= 1, 1, 0)]

# ver que dias trabajo una oficina
horarios_aux[year_month == "2019-02" & llave == 2116115]


horarios_1 <- dcast(horarios_aux, llave + year_month ~ weekday, value.var = "dummy" )
horarios_2 <- dcast(horarios_aux, llave + year_month ~ weekday, value.var = "N" )

var_days <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

horarios_1[, (var_days) := lapply(.SD, function(x){
  x <- ifelse(is.na(x) == T, 0, x)
  return(x)}), .SDcols = var_days]
horarios_2[, (var_days) := lapply(.SD, function(x){
  x <- ifelse(is.na(x) == T, 0, x)
  return(x)}), .SDcols = var_days]
###
trx <- paste0(var_days, "_trx")

names(horarios_2)[3:9] <- paste0(var_days, "_trx")

horarios <- merge(horarios_1, horarios_2, by = c("llave", "year_month"))

horarios[, periodicidad := rowSums(horarios[, .SD, .SDcols = var_days])]

###
oficinas_actuales <- horarios[year_month == "2019-05", llave]
horarios <- horarios[llave %in% oficinas_actuales]

# horario
aux <- copy(horarios[, .SD, .SDcols = var_days])
resultado <- c()
for (i in 1:nrow(aux)){
  fila <- as.vector(aux[i])
  resultado[i] <- paste(names(aux)[as.logical(fila)], collapse = ",")
}
horarios[, horario := resultado]
rm(aux, resultado, fila)

## oficinas con periodicidad igual a cero 
horarios[year_month == "2019-05" & periodicidad == 0, unique(llave)]

# que horaris hay ? 
horarios[year_month == "2019-05", .N, by = horario][order(-N)]
horarios[year_month != "2019-05", .N, by = horario][order(-N)]

# horarios[, .(desv = sd(periodicidad)), by = llave]

# que ofinas cambian su horario mes a me s
analisis_horarios <-
  horarios[, .(horarios_distintos = uniqueN(horario)), by = llave]
analisis_horarios[, .(.N) , by = .( horarios_distintos)][order(horarios_distintos)]


horarios[llave == analisis_horarios[horarios_distintos == 3, llave][1]]

#### horario del ultimo mes observado

horario_ultimo_mes <- horarios[year_month == "2019-05", .(llave, horario)]
horario_ultimo_mes[, horario_ult_mes := 1]
horarios <- merge(horarios, horario_ultimo_mes, by = c("llave", "horario"), all.x = T)


horarios[is.na(horario_ult_mes), horario_ult_mes := 0]

horarios <- horarios[order(llave, year_month)]

horarios[llave %in% horarios[horario_ult_mes == 0, llave][27]]
horarios[llave == analisis_horarios[horarios_distintos == 3, llave][3]]

horarios <- mes_modelamiento(horarios)

horario_ultimo_mes[, horario_ult_mes := NULL]
setnames(horario_ultimo_mes, "horario", "horario_ult_mes2")

horarios <- merge(horarios, horario_ultimo_mes, by = c("llave"), all.x = T)

horarios[,  horario_ult_mes3 := ifelse(str_detect(horario, horario_ult_mes2), 1,0)]

horarios[is.na(horario_ult_mes3), horario_ult_mes3 := 0]
horarios[ , horario_definitivo := ifelse(horario_ult_mes3 == 1, horario_ult_mes2, horario)]

horarios[, diferencia := horario_ult_mes - horario_ult_mes3]
horarios[llave %in% horarios[diferencia != 0, llave][3]]

rm(horario_ultimo_mes)

fwrite(horarios, "data/insumos_pronostico/horarios/resumen_horarios.csv")
horarios <- fread( "data/insumos_pronostico/horarios/resumen_horarios.csv")

analisis_horarios <-
  horarios[, .(horarios_distintos = uniqueN(horario_definitivo)), by = llave]

analisis_horarios[, .(.N) , by = .( horarios_distintos)][order(horarios_distintos)]

# oficina que no trabajo un jueves de la nada
horarios[llave %in% analisis_horarios[horarios_distintos == 3, llave][1]]

# agunata y se arregla con la dummy 
horarios[llave %in% analisis_horarios[horarios_distintos == 3, llave][2]]

# aguanta y se solucina con la dummy nueva 
horarios[llave %in% analisis_horarios[horarios_distintos == 3, llave][3]]


# ofinas re pailas 
horarios[llave %in% analisis_horarios[horarios_distintos == 4, llave][1]]

horarios[llave %in% analisis_horarios[horarios_distintos == 8, llave][1]]

horarios[llave %in% analisis_horarios[horarios_distintos == 2, llave][4]]
