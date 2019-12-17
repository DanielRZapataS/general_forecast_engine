
#' Generacion de la tabla de metadata a partir del insumo de series temporales
#'
#' @param mes el mes de corte
#'
#' @return exporta en la ruta de data/metadata la tabla en formato csv
#'
#' @examples
#' generacion_meta(mes = "201812")
#' 
generacion_meta <- function(mes) {
  
  serie_temporal <- fread(paste0(ruta_serie, "serie_temporal_", mes, ".csv"))
    
  #eliminar llaves de oficinas cerradas
  dicc_gob <- fread(paste0(ruta_dicc, nombre_dic_gob, ".csv"), encoding = "Latin-1", integer64 = "character")
  dicc_gob[, region := stri_trans_general(dicc_gob$region,"Latin-ASCII")]
  dicc_gob[, zona := stri_trans_general(dicc_gob$zona,"Latin-ASCII")]
  dicc_gob[, nombre := stri_trans_general(dicc_gob$nombre,"Latin-ASCII")]
  dicc_gob[, oficina := as.numeric(str_extract(codigo_oficina, "\\-*\\d+\\.*\\d*"))]
  dicc_gob <- dicc_gob[, .(oficina, estado_operacion)]
  serie_temporal[, oficina := as.numeric(substr(llave, 3, 6))]
  serie_temporal <- merge(serie_temporal, dicc_gob, by = "oficina", all.x = T, all.y = F)
  rm(dicc_gob)
  serie_temporal <- serie_temporal[estado_operacion == ""]
  serie_temporal[, oficina := NULL]
  serie_temporal[, estado_operacion := NULL]
  
  serie_temporal[, weekday := wday(fecha)]
  serie_temporal[, year_month := format(as.Date(fecha), "%Y-%m")]
  
  #copiar oficinas 
  horarios_aux <- copy(serie_temporal)
  horarios_aux[, fecha := as.Date(fecha) ]
  horarios_aux[, weekday := wday(fecha)]
  horarios_aux[, year_month := format(fecha, "%Y-%m")]
    
  #Agrupar por la variable llave, ano_mes y dia
  horarios_aux <- horarios_aux[, .N, by = .(llave,year_month, weekday)]
  horarios_aux[, year_month_wday := paste0(year_month, "-", weekday)]
    
  #Incluir el diccionario de festivos
  festivos <- fread(paste0(ruta_dicc, nomb_festivos, ".csv"))
  festivos[, fecha := as.Date(fecha, format = "%d/%m/%Y")]
  festivos[, weekday := wday(fecha)]
  festivos[, year_month := substr(fecha, 1, 7)]
  festivos[, year_month_wday := paste(year_month, weekday, sep = "-")]
    
  count_holidays <- festivos[, .(num_festivos = .N), by = year_month_wday]
  count_holidays <- count_holidays[num_festivos > 1]
    
  horarios_aux <- merge(horarios_aux, count_holidays, by = "year_month_wday", all.x = T, all.y = F)
  horarios_aux[, year_month_wday := NULL]
  horarios_aux[is.na(num_festivos), num_festivos := 0]
    
  horarios_aux[, dummy := ifelse(N >= 2, 1, 
                                 ifelse(num_festivos > 1 & N >= 1, 1, 0))]
    
  horarios_aux <- dcast(horarios_aux, llave + year_month ~ weekday, value.var = "dummy" )
  
  #reemplazar NA's con 0
  reemplazar <- function(x) {ifelse(is.na(x), 0, x)}
  #aplicar funcion reemplazar a los datos de tabla_conteo
  horarios_aux[,3:ncol(horarios_aux)] <- lapply(horarios_aux[,3:ncol(horarios_aux)], reemplazar)
    
  #Obtener los dias que trabaja la oficina en el mes
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
    
  ##Oficinas actuales
  oficinas_actuales <- horarios_aux[year_month == paste0(substr(mes, 1, 4), "-", substr(mes, 5, 6)), llave]
  #Seleccionar unicamente las oficinas cuyas oficinas estan activas en el mes actual
  horarios <- horarios_aux[llave %in% oficinas_actuales]
  #Ordenar los datos
  horarios <- horarios[order(llave, year_month)]
  
  #########################
    
  #Horarios del ultimo mes 
  horario_ultimo_mes <- horarios[year_month == paste0(substr(mes, 1, 4), "-", substr(mes, 5, 6)), .(llave, dias)]
  names(horario_ultimo_mes) <- c("llave", "dias2")
    
  horarios <- merge(horarios, horario_ultimo_mes, by = "llave", all.x = T, all.y = F)
    
  ##se construye una variable indicadora que significa lo siguiente:
  ##si el horario actual esta contenido en el horario de la fecha correspondiente, entonces
  ##asignar el horario actual, sino, asignar el horario
  suppressWarnings(horarios[, horario_definitivo := ifelse(str_detect(dias, dias2), dias2, dias)])
  
  #Generar variable indicadora que dice: si horario_definitivo es igual a dias entonces 1 sino 0.
  horarios[, indicadora := ifelse(dias2 == horario_definitivo, 1, 0)]
  
  #dummy de considerar las fechas
  horarios <- dummy_considerar_fechas(horarios)
  
  #Calculo de la variable periodicidad
  horarios[, periodicidad := str_count(horario_definitivo, ",") + 1]
  horarios[horario_definitivo == "", periodicidad := 0]
  
  #########################
  #Tabla de horarios para exportar
  
  horarios_exportar <- copy(horarios)
  horarios_exportar <- horarios_exportar[, .(llave, year_month, dias)]
  
  horarios_exportar[, dias := gsub(1, "Sun", dias)]
  horarios_exportar[, dias := gsub(2, "Mon", dias)]
  horarios_exportar[, dias := gsub(3, "Tue", dias)]
  horarios_exportar[, dias := gsub(4, "Wed", dias)]
  horarios_exportar[, dias := gsub(5, "Thu", dias)]
  horarios_exportar[, dias := gsub(6, "Fri", dias)]
  horarios_exportar[, dias := gsub(7, "Sat", dias)]
  
  fwrite(horarios_exportar, paste0(ruta_ins_pron, carp_horarios, "resumen_horarios.csv"))
  pprint(paste0("Se ha exportado la tabla de resumen de horarios relacionada al mes ", mes, "."))
  
  #########################
  #filtro de cuales considerar
  
  horarios <- horarios[considerar == 1]
  
  meta <- horarios[, .(mes_inicio = min(year_month),
                       meses = .N), by = .(llave, horario_definitivo, periodicidad)]
  
  #cuales meses considerar
  meses_considerar <- horarios[considerar == 1, .(meses_considerar = paste(year_month, collapse = "|")), by = llave]
  meta <- merge(meta, meses_considerar, by = "llave", all.x = T, all.y = F)
  
  fwrite(meta, paste0(ruta_ins_pron, carp_meta, "meta_pronostico.csv"))
  pprint(paste0("Se ha exportado la tabla de metadata relacionada al mes ", mes, "."))
  
}


#' Genera las tablas de metadata para los meses que no estan, pero si hay informacion en series temporales
#'
#' @return exporta las tablas mencionadas en la carpeta data/insumos_pronostico/metadata
#'
#' @examples
#' metadata()
#' 
metadata <- function() {
  
  files <- list.files(ruta_serie)
  position <-
    as.vector(sapply(files, extraer_numeros))
  mes <- as.character(max(position))
  
  generacion_meta(mes)
  
}

