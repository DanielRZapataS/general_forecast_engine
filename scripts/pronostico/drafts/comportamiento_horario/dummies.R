mes_cambio <- function(tabla){
  
  source("scripts/historico/staging_maker/auxiliar.R")
  
  min_unos  <- tabla[horario_ult_mes == 1, .(min_unos = min(year_month)), by = llave]
  max_ceros <- tabla[horario_ult_mes == 0, .(max_ceros = max(year_month)), by = llave]
  
  dt <- merge(min_unos, max_ceros, by = "llave", all = T)
  
  dt[is.na(max_ceros), fecha_inicial := min_unos]
  dt[min_unos > max_ceros, fecha_inicial := min_unos]
  dt[min_unos < max_ceros, fecha_inicial := sapply(max_ceros, mes_adelante_n, n = 1, sep = "-")]
  
  dt <- dt[, .(llave, fecha_inicial)]
  
  tabla <- merge(tabla, dt, by = "llave", all.x = T, all.y = F)
  tabla[, mes_cambio := as.numeric(year_month >= fecha_inicial)]
  tabla[, fecha_inicial := NULL]
  
  return(tabla)
  
}

mes_cambio <- function(tabla){
  
  source("scripts/historico/staging_maker/auxiliar.R")
  
  min_unos  <- tabla[horario_ult_mes == 1, .(min_unos = min(year_month)), by = llave]
  max_ceros <- tabla[horario_ult_mes == 0, .(max_ceros = max(year_month)), by = llave]
  
  dt <- merge(min_unos, max_ceros, by = "llave", all = T)
  
  dt[is.na(max_ceros), fecha_inicial := min_unos]
  dt[min_unos > max_ceros, fecha_inicial := min_unos]
  dt[min_unos < max_ceros, fecha_inicial := sapply(max_ceros, mes_adelante_n, n = 1, sep = "-")]
  
  dt <- dt[, .(llave, fecha_inicial)]
  
  tabla <- merge(tabla, dt, by = "llave", all.x = T, all.y = F)
  tabla[, mes_cambio := as.numeric(year_month >= fecha_inicial)]
  tabla[, fecha_inicial := NULL]
  
  return(tabla)
  
}


