

#' Generacion de la tabla de tasas de ocupacion mensual por llave, a partir del insumo de staging
#' IMPORTANTE: requiere que este completo el diccionario de cajeros.
#'
#' @param mes el mes de corte
#'
#' @return genera el objeto con las tasas de ocupacion para cada llave que aparezca en staging
#'
#' @examples
#' generacion_staging(mes = "201812")
#' 
t_ocup_mensual <- function(mes) {
  
  #cargue de staging como insumo principal para el calculo de la t_ocup
  staging <- fread(file = paste0(ruta_staging, "staging_", mes, ".csv"), integer64 = "character", showProgress = F)
  
  #numero de tx en general, y dependiendo del valor, por llave
  conteo_tx <- staging[, .(conteo     = .N,
                           mayores_1m = sum(valor_tx == "mayor a 1m"),
                           menores_1m = sum(valor_tx == "menor a 1m"),
                           otros      = sum(valor_tx == "otro")),
                      by = .(llave, oficina_segun_terminal)][order(llave)]
  
  #pegar informacion de cajeros 
  dicc_cajeros <- fread(file = paste0(ruta_dicc, carp_dicc_caj, "dicc_cajeros_", mes, ".csv"))
  dicc_cajeros <- dicc_cajeros[, .(num_cajeros = mode(num_cajeros)), by = llave]
  
  conteo_tx <- merge(conteo_tx, dicc_cajeros,
                     by = "llave", all.x = T, all.y = F)
  conteo_tx$num_cajeros <- as.numeric(conteo_tx$num_cajeros)

  #if (sum(is.na(conteo_tx$num_cajeros)) > 0) {stop(paste(c("Las siguientes llaves no aparecen en el diccionario de cajeros:", unique(staging[!(llave %in% dicc_cajeros$llave)]$llave)), collapse = " "))}
  conteo_tx <- conteo_tx[!(is.na(num_cajeros))]
  
  rm(dicc_cajeros)
  
  #creacion de la tasa de servicio
  param_tasa_servicio <- fread(paste0(ruta_paramet, "tasa_servicio.csv"))
  conteo_tx[, tasa_servicio := (mayores_1m*param_tasa_servicio$tx_mayor_1m + 
                               menores_1m*param_tasa_servicio$tx_menor_1m + 
                               otros*param_tasa_servicio$tx_otras)/conteo]
  
  #horas de jornada
  conteo_tx[, tipo_oficina := as.numeric(substr_right(llave, 1))]
  conteo_tx[, cnl_jornada := as.numeric(substr(llave, 1, 2))]
  param_horas_jornada <- fread(paste0(ruta_paramet, "horas_servicios.csv"))
  conteo_tx <- merge(conteo_tx, param_horas_jornada, by = c("tipo_oficina", "cnl_jornada"), all.x = T, all.y = F)
  
  #dias habiles
  aux <- staging[, .N, by = .(llave, cnl_fecha)]
  dias_habiles <- aux[, .N, by = llave]
  names(dias_habiles) <- c("llave", "dias_habiles")
  conteo_tx <- merge(conteo_tx, dias_habiles, by = "llave", all.x = T, all.y = F)
  
  #tasa de ocupacion
  conteo_tx[, t_ocup := conteo/(horas_jornada*num_cajeros*dias_habiles*tasa_servicio)]
  
  #variables relevantes
  conteo_tx <- conteo_tx[, .(llave, oficina_segun_terminal, t_ocup, conteo)]
  names(conteo_tx) <- c("llave", "oficina_segun_terminal", "t_ocup", "total_tx")
  
  return(conteo_tx)

}


#' Calculo de tasa de ocupacion por llave, para un mes especifico
#'
#' @param mes el mes de corte
#'
#' @return exporta en la ruta de resultados/historico/historico_mes la tabla en formato csv
#'
#' @examples
#' t_ocup_llaves("201812")
#' 
t_ocup_llaves <- function(mes) {
  
  t_ocup <- t_ocup_mensual(mes)
  t_ocup[, mes := mes]
  t_ocup[, jornada := substr(llave, 1, 2)]
  
  #pegarle informacion de gobierno de datos: region, zona, nombre de oficina
  dicc_gobierno <- fread(file = paste0(ruta_dicc, nombre_dic_gob, ".csv"), integer64 = "character", encoding = "Latin-1")
  dicc_gobierno[, region := stri_trans_general(dicc_gobierno$region,"Latin-ASCII")]
  dicc_gobierno[, zona := stri_trans_general(dicc_gobierno$zona,"Latin-ASCII")]
  dicc_gobierno[, nombre := stri_trans_general(dicc_gobierno$nombre,"Latin-ASCII")]
  dicc_gobierno[zona == "", zona := "{No hay informacion}"]
  dicc_gobierno[, codigo_oficina_numeric := as.numeric(str_extract(codigo_oficina, "\\-*\\d+\\.*\\d*"))]
  dicc_gobierno[, region_aux := str_extract_all(region, "\\{[^()]+\\}")]
  dicc_gobierno[, region := substring(region_aux, 2, nchar(region_aux)-1)]
  dicc_gobierno[, zona_aux := str_extract_all(zona, "\\{[^()]+\\}")]
  dicc_gobierno[, zona := substring(zona_aux, 2, nchar(zona_aux)-1)]
  dicc_gobierno <- dicc_gobierno[, .(codigo_oficina_numeric, codigo_oficina, nombre, region, zona)]
  dicc_gobierno[, nombre := paste0(codigo_oficina, " - ", nombre)]
  dicc_gobierno[, codigo_oficina := NULL]
  dicc_gobierno[, nombre := sapply(nombre, quitar_raros)]
  t_ocup <- merge(t_ocup, dicc_gobierno, by.x = "oficina_segun_terminal", by.y = "codigo_oficina_numeric", all.x = T, all.y = F)

  #pegar informacion de cajeros 
  dicc_cajeros <- fread(file = paste0(ruta_dicc, carp_dicc_caj, "dicc_cajeros_", mes, ".csv"))
  dicc_cajeros <- dicc_cajeros[, .(num_cajeros = mode(num_cajeros)), by = llave]
  t_ocup <- merge(t_ocup, dicc_cajeros, by = "llave", all.x = T, all.y = F)
  t_ocup <- t_ocup[!is.na(num_cajeros)]
  
  t_ocup <- t_ocup[, .(llave, jornada, region, zona, nombre, mes, total_tx, t_ocup, num_cajeros)]

  fwrite(t_ocup, paste0(ruta_result, "historico_", mes, "/ocup_llaves_", mes, ".csv"))
  pprint(paste0("Se ha exportado en la ruta de resultados del historico, el archivo de ocupacion por llaves para el mes ", mes, "."))

}

#' Generacion de la tabla de tasas de ocupacion mensual por TERMINAL, a partir del insumo de staging
#'
#' @param mes el mes de corte
#'
#' @return exporta en la ruta de resultados/historico/historico_mes la tabla en formato csv
#' 
#' @examples
#' t_ocup_mensual_terminal(mes = "201812")
#' 
t_ocup_mensual_terminal <- function(mes) {
  
  #cargue de staging como insumo principal para el calculo de la t_ocup
  staging <- fread(file = paste0(ruta_staging, "staging_", mes, ".csv"), integer64 = "character", showProgress = F)
  
  #numero de tx en general, y dependiendo del valor, por llave, oficina Y TERMINAL
  conteo_tx <- staging[, .(conteo     = .N,
                           mayores_1m = sum(valor_tx == "mayor a 1m"),
                           menores_1m = sum(valor_tx == "menor a 1m"),
                           otros      = sum(valor_tx == "otro")),
                       by = .(numero_caja, llave, oficina_segun_terminal)][order(llave, numero_caja)]
  
  #no se pega informacion de numero de cajeros, porque simplemente es 1
  
  #creacion de la tasa de servicio POR LLAVE y pegarla
  param_tasa_servicio <- fread(paste0(ruta_paramet, "tasa_servicio.csv"))
  aux <- staging[, .(conteo = .N,
                     mayores_1m = sum(valor_tx == "mayor a 1m"),
                     menores_1m = sum(valor_tx == "menor a 1m"),
                     otros      = sum(valor_tx == "otro")),
                 by = .(llave)][order(llave)]
  aux[, tasa_servicio := (mayores_1m*param_tasa_servicio$tx_mayor_1m + 
                          menores_1m*param_tasa_servicio$tx_menor_1m + 
                          otros*param_tasa_servicio$tx_otras)/conteo]
  aux <- aux[, .(llave, tasa_servicio)]
  conteo_tx <- merge(conteo_tx, aux, by = "llave", all.x = T, all.y = F)
  
  #horas de jornada
  conteo_tx[, tipo_oficina := as.numeric(substr_right(llave, 1))]
  conteo_tx[, cnl_jornada := as.numeric(substr(llave, 1, 2))]
  param_horas_jornada <- fread(paste0(ruta_paramet, "horas_servicios.csv"))
  conteo_tx <- merge(conteo_tx, param_horas_jornada, by = c("tipo_oficina", "cnl_jornada"), all.x = T, all.y = F)
  
  #dias habiles para cada numero de caja y llave
  aux <- staging[, .N, by = .(llave, cnl_fecha, numero_caja)]
  dias_habiles <- aux[, .N, by = .(llave, numero_caja)]
  names(dias_habiles) <- c("llave", "numero_caja", "dias_habiles")
  conteo_tx <- merge(conteo_tx, dias_habiles, by = c("llave", "numero_caja"), all.x = T, all.y = F)
  
  #tasa de ocupacion
  conteo_tx[, t_ocup := conteo/(horas_jornada*dias_habiles*tasa_servicio)]
  
  #jornada
  conteo_tx[, jornada := substr(llave, 1, 2)]
  
  #mes
  conteo_tx[, mes := mes]
  
  #tx
  conteo_tx[, tx := conteo]
  
  #variables relevantes
  conteo_tx <- conteo_tx[, .(numero_caja, llave, oficina_segun_terminal, mes, jornada, tx, tasa_servicio, t_ocup)][order(mes, jornada, llave, numero_caja)]
  
  #pegarle informacion de gobierno de datos: region, zona, nombre de oficina
  dicc_gobierno <- fread(file = paste0(ruta_dicc, nombre_dic_gob, ".csv"), integer64 = "character", encoding = "Latin-1")
  dicc_gobierno[, region := stri_trans_general(dicc_gobierno$region,"Latin-ASCII")]
  dicc_gobierno[, zona := stri_trans_general(dicc_gobierno$zona,"Latin-ASCII")]
  dicc_gobierno[, nombre := stri_trans_general(dicc_gobierno$nombre,"Latin-ASCII")]
  dicc_gobierno[zona == "", zona := "{No hay informacion}"]
  dicc_gobierno[, codigo_oficina_numeric := as.numeric(str_extract(codigo_oficina, "\\-*\\d+\\.*\\d*"))]
  dicc_gobierno[, region_aux := str_extract_all(region, "\\{[^()]+\\}")]
  dicc_gobierno[, region := substring(region_aux, 2, nchar(region_aux)-1)]
  dicc_gobierno[, zona_aux := str_extract_all(zona, "\\{[^()]+\\}")]
  dicc_gobierno[, zona := substring(zona_aux, 2, nchar(zona_aux)-1)]
  dicc_gobierno <- dicc_gobierno[, .(codigo_oficina_numeric, codigo_oficina, nombre, region, zona)]
  dicc_gobierno[, nombre := paste0(codigo_oficina, " - ", nombre)]
  dicc_gobierno[, codigo_oficina := NULL]
  dicc_gobierno[, nombre := sapply(nombre, quitar_raros)]

  conteo_tx <- merge(conteo_tx, dicc_gobierno, by.x = "oficina_segun_terminal", by.y = "codigo_oficina_numeric", all.x = T, all.y = F)
  conteo_tx <- conteo_tx[, .(llave, oficina_segun_terminal, numero_caja, mes, jornada, region, zona, nombre, tx, tasa_servicio, t_ocup)][order(llave, numero_caja)]
  
  fwrite(conteo_tx, paste0(ruta_result, "historico_", mes, "/ocup_terminales_", mes, ".csv"))
  pprint(paste0("Se ha exportado en la ruta de resultados del historico, el archivo de ocupacion por terminal para el mes ", mes, "."))

}


#' Calculo de tasa de ocupacion por jornada, para un mes especifico
#'
#' @param mes el mes de corte
#'
#' @return exporta en la ruta de resultados/historico/historico_mes la tabla en formato csv
#'
#' @examples
#' t_ocup_jornada("201812")
#' 
t_ocup_jornada <- function(mes) {
  
  t_ocup <- t_ocup_mensual(mes)
  
  t_ocup[, jornada := substr(llave, 1, 2)]
  
  #construccion auxiliar para identificar cuantas transacciones ocurren por rango de tasa de ocupacion
  t_ocup[, menor_70 := ifelse(t_ocup < 0.7, 1, 0)]
  t_ocup[, entre_70_90 := ifelse(t_ocup >= 0.7 & t_ocup <= 0.9, 1, 0)]
  t_ocup[, mayor_90 := ifelse(t_ocup > 0.9, 1, 0)]
  
  t_ocup_jornada <- t_ocup[, .(mes = mes, 
                               menor_70 = sum(t_ocup < 0.7),
                               entre_70_90 = sum(t_ocup >= 0.7 & t_ocup <= 0.9),
                               mayor_90 = sum(t_ocup > 0.9),
                               porc_menor_70 = sum(t_ocup < 0.7)/.N,
                               porc_entre_70_90 = sum(t_ocup >= 0.7 & t_ocup <= 0.9)/.N,
                               porc_mayor_90 = sum(t_ocup > 0.9)/.N,
                               tx_to_menor70 = sum(total_tx*menor_70),
                               tx_to_entre70y90 = sum(total_tx*entre_70_90),
                               tx_to_mayor90 = sum(total_tx*mayor_90)), 
                           by = jornada][order(mes, jornada)]
  
  fwrite(t_ocup_jornada, paste0(ruta_result, "historico_", mes, "/ocup_jornada_", mes, ".csv"))
  pprint(paste0("Se ha exportado en la ruta de resultados del historico, el archivo de ocupacion por jornada para el mes ", mes, "."))

}



#' Calculo de tasa de ocupacion por region y jornada, para un mes especifico
#'
#' @param mes el mes de corte
#'
#' @return exporta en la ruta de resultados/historico/historico_mes la tabla en formato csv
#'
#' @examples
#' t_ocup_region("201812")
#' 
t_ocup_region <- function(mes) {
  
  t_ocup <- t_ocup_mensual(mes)
  
  dicc_gobierno <- fread(file = paste0(ruta_dicc, nombre_dic_gob, ".csv"), integer64 = "character", encoding = 'Latin-1')
  dicc_gobierno[, region := stri_trans_general(dicc_gobierno$region,"Latin-ASCII")]
  dicc_gobierno[, zona := stri_trans_general(dicc_gobierno$zona,"Latin-ASCII")]
  dicc_gobierno[, nombre := stri_trans_general(dicc_gobierno$nombre,"Latin-ASCII")]
  dicc_gobierno[, codigo_oficina_numeric := as.numeric(str_extract(codigo_oficina, "\\-*\\d+\\.*\\d*"))]
  dicc_gobierno[, region_aux := str_extract_all(region, "\\{[^()]+\\}")]
  dicc_gobierno[, region := substring(region_aux, 2, nchar(region_aux)-1)]
  dicc_gobierno <- dicc_gobierno[, .(codigo_oficina_numeric, region)]
  
  t_ocup <- merge(t_ocup, dicc_gobierno,
                  by.x = "oficina_segun_terminal",
                  by.y = "codigo_oficina_numeric", all.x = T, all.y = F)
  
  t_ocup[, jornada := substr(llave, 1, 2)]
  
  
  #construccion auxiliar para identificar cuantas transacciones ocurren por rango de tasa de ocupacion
  t_ocup[, menor_70 := ifelse(t_ocup < 0.7, 1, 0)]
  t_ocup[, entre_70_90 := ifelse(t_ocup >= 0.7 & t_ocup <= 0.9, 1, 0)]
  t_ocup[, mayor_90 := ifelse(t_ocup > 0.9, 1, 0)]
  
  t_ocup_region  <- t_ocup[, .(mes = mes, 
                               menor_70 = sum(t_ocup < 0.7),
                               entre_70_90 = sum(t_ocup >= 0.7 & t_ocup <= 0.9),
                               mayor_90 = sum(t_ocup > 0.9),
                               porc_menor_70 = sum(t_ocup < 0.7)/.N,
                               porc_entre_70_90 = sum(t_ocup >= 0.7 & t_ocup <= 0.9)/.N,
                               porc_mayor_90 = sum(t_ocup > 0.9)/.N,
                               tx_to_menor70 = sum(total_tx*menor_70),
                               tx_to_entre70y90 = sum(total_tx*entre_70_90),
                               tx_to_mayor90 = sum(total_tx*mayor_90)), 
                           by = .(jornada, region)][order(mes, jornada, region)]
  
  t_ocup_region$region <- 
    stri_trans_general(t_ocup_region$region,"Latin-ASCII")

  fwrite(t_ocup_region, paste0(ruta_result, "historico_", mes, "/ocup_region_", mes, ".csv"))
  pprint(paste0("Se ha exportado en la ruta de resultados del historico, el archivo de ocupacion por region y jornada para el mes ", mes, "."))

}



#' Calculo de tasa de ocupacion por zona y jornada, para un mes especifico
#'
#' @param mes el mes de corte
#'
#' @return exporta en la ruta de resultados/historico/historico_mes la tabla en formato csv
#'
#' @examples
#' t_ocup_zona("201812")
#' 
t_ocup_zona <- function(mes) {
  
  t_ocup <- t_ocup_mensual(mes)
  
  dicc_gobierno <- fread(file = paste0(ruta_dicc, nombre_dic_gob, ".csv"), integer64 = "character", encoding = "Latin-1")
  dicc_gobierno[, region := stri_trans_general(dicc_gobierno$region,"Latin-ASCII")]
  dicc_gobierno[, zona := stri_trans_general(dicc_gobierno$zona,"Latin-ASCII")]
  dicc_gobierno[, nombre := stri_trans_general(dicc_gobierno$nombre,"Latin-ASCII")]
  
  dicc_gobierno[zona == "", zona := "{No hay informacion}"]
  
  dicc_gobierno[, codigo_oficina_numeric := as.numeric(str_extract(codigo_oficina, "\\-*\\d+\\.*\\d*"))]
  dicc_gobierno[, region_aux := str_extract_all(region, "\\{[^()]+\\}")]
  dicc_gobierno[, region := substring(region_aux, 2, nchar(region_aux)-1)]
  dicc_gobierno[, zona_aux := str_extract_all(zona, "\\{[^()]+\\}")]
  dicc_gobierno[, zona := substring(zona_aux, 2, nchar(zona_aux)-1)]
  dicc_gobierno <- dicc_gobierno[, .(codigo_oficina_numeric, region, zona)]
  
  t_ocup <- merge(t_ocup, dicc_gobierno,
                  by.x = "oficina_segun_terminal",
                  by.y = "codigo_oficina_numeric", all.x = T, all.y = F)
  
  t_ocup[, jornada := substr(llave, 1, 2)]
  
  #construccion auxiliar para identificar cuantas transacciones ocurren por rango de tasa de ocupacion
  t_ocup[, menor_70 := ifelse(t_ocup < 0.7, 1, 0)]
  t_ocup[, entre_70_90 := ifelse(t_ocup >= 0.7 & t_ocup <= 0.9, 1, 0)]
  t_ocup[, mayor_90 := ifelse(t_ocup > 0.9, 1, 0)]
  
  t_ocup_zona    <- t_ocup[, .(mes = mes, 
                               menor_70 = sum(t_ocup < 0.7),
                               entre_70_90 = sum(t_ocup >= 0.7 & t_ocup <= 0.9),
                               mayor_90 = sum(t_ocup > 0.9),
                               porc_menor_70 = sum(t_ocup < 0.7)/.N,
                               porc_entre_70_90 = sum(t_ocup >= 0.7 & t_ocup <= 0.9)/.N,
                               porc_mayor_90 = sum(t_ocup > 0.9)/.N,
                               tx_to_menor70 = sum(total_tx*menor_70),
                               tx_to_entre70y90 = sum(total_tx*entre_70_90),
                               tx_to_mayor90 = sum(total_tx*mayor_90)), 
                           by = .(jornada, region, zona)][order(mes, jornada, region, zona)]
  
  t_ocup_zona$zona <- 
    stri_trans_general(t_ocup_zona$zona, "Latin-ASCII")
  
  fwrite(t_ocup_zona, paste0(ruta_result, "historico_", mes, "/ocup_zona_", mes, ".csv"))
  pprint(paste0("Se ha exportado en la ruta de resultados del historico, el archivo de ocupacion por zona y jornada para el mes ", mes, "."))
  
}



#' Generacion de la tabla de tasas de ocupacion mensual por llaves por HORAS, a partir del insumo de staging
#'
#' @param mes el mes de corte
#'
#' @return exporta en la ruta de resultados/historico/historico_mes la tabla en formato csv
#' 
#' @examples
#' t_ocup_horas(mes = "201812")
#' 
t_ocup_horas <- function(mes) {
  
  #cargue de staging como insumo principal para el calculo de la t_ocup
  staging <- fread(file = paste0(ruta_staging, "staging_", mes, ".csv"), integer64 = "character", showProgress = F)
  
  staging$hora_tx <- factor(staging$hora_tx, levels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20", "20-21", "21-22", "22-23", "23-24"))
  
  #numero de tx en general, y dependiendo del valor, por llave
  conteo_tx <- staging[, .(conteo     = .N,
                           mayores_1m = sum(valor_tx == "mayor a 1m"),
                           menores_1m = sum(valor_tx == "menor a 1m"),
                           otros      = sum(valor_tx == "otro")),
                       by = .(llave, oficina_segun_terminal, hora_tx)][order(llave, hora_tx)]
  
  #pegar informacion de cajeros para NUMERO DE CAJEROS TEORICO
  dicc_cajeros <- fread(file = paste0(ruta_dicc, carp_dicc_caj, "dicc_cajeros_", mes, ".csv"))
  dicc_cajeros <- dicc_cajeros[, .(num_cajeros = mode(num_cajeros)), by = llave]
  names(dicc_cajeros) <- c("llave", "num_cajeros_teorico")
  
  conteo_tx <- merge(conteo_tx, dicc_cajeros,
                     by = "llave", all.x = T, all.y = F)
  conteo_tx$num_cajeros_teorico <- as.numeric(conteo_tx$num_cajeros_teorico)
  #if (sum(is.na(conteo_tx$num_cajeros)) > 0) {stop(paste(c("Las siguientes llaves no aparecen en el diccionario de cajeros:", unique(staging[!(llave %in% dicc_cajeros$llave)]$llave)), collapse = " "))}
  conteo_tx <- conteo_tx[!(is.na(num_cajeros_teorico))]
  rm(dicc_cajeros)
  
  #informacion de cajeros OBSERVADO, CALCULADO CON TERMINALES (NUMERO DE CAJA), PARA CADA HORA
  aux <- staging[, .N, by = .(llave, cnl_fecha, hora_tx, numero_caja)]
  aux2 <- aux[, .N, by = .(llave, cnl_fecha, hora_tx)]
  
  cajeros_promedio <- aux2[, .(total_cajas = sum(N),
                               num_dias = uniqueN(cnl_fecha)), by = .(llave, hora_tx)]
  cajeros_promedio[, num_cajeros_obs := total_cajas/num_dias]
  cajeros_promedio <- cajeros_promedio[, .(llave, hora_tx, num_cajeros_obs)]
  conteo_tx <- merge(conteo_tx, cajeros_promedio, by = c("llave", "hora_tx"), all.x = T, all.y = F)
  
  #creacion de la tasa de servicio POR LLAVE y pegarla
  param_tasa_servicio <- fread(paste0(ruta_paramet, "tasa_servicio.csv"))
  aux <- staging[, .(conteo = .N,
                     mayores_1m = sum(valor_tx == "mayor a 1m"),
                     menores_1m = sum(valor_tx == "menor a 1m"),
                     otros      = sum(valor_tx == "otro")),
                     by = .(llave)][order(llave)]
  aux[, tasa_servicio := (mayores_1m*param_tasa_servicio$tx_mayor_1m + 
                          menores_1m*param_tasa_servicio$tx_menor_1m + 
                          otros*param_tasa_servicio$tx_otras)/conteo]
  aux <- aux[, .(llave, tasa_servicio)]
  conteo_tx <- merge(conteo_tx, aux, by = "llave", all.x = T, all.y = F)

  #horas de jornada es uno para todas
  
  #dias habiles
  aux <- staging[, .N, by = .(llave, cnl_fecha)]
  dias_habiles <- aux[, .N, by = llave]
  names(dias_habiles) <- c("llave", "dias_habiles")
  conteo_tx <- merge(conteo_tx, dias_habiles, by = "llave", all.x = T, all.y = F)
  
  #tasa de ocupacion teorica
  conteo_tx[, t_ocup_teorica := conteo/(num_cajeros_teorico*tasa_servicio*dias_habiles)]
  #tasa de ocupacion observada
  conteo_tx[, t_ocup_observada := conteo/(num_cajeros_obs*tasa_servicio*dias_habiles)]
  
  #transacciones promedio por hora
  conteo_tx[, tx_promedio := conteo/dias_habiles]
  
  #jornada
  conteo_tx[, jornada := substr(llave, 1, 2)]
  
  #mes
  conteo_tx[, mes := mes]
  
  #variables relevantes
  conteo_tx <- conteo_tx[, .(llave, oficina_segun_terminal, jornada, mes, hora_tx, tx_promedio, tasa_servicio, num_cajeros_teorico, t_ocup_teorica, num_cajeros_obs, t_ocup_observada)][order(mes, jornada, llave, hora_tx)]
  
  fwrite(conteo_tx, paste0(ruta_result, "historico_", mes, "/ocup_horas_", mes, ".csv"))
  pprint(paste0("Se ha exportado en la ruta de resultados del historico, el archivo de ocupacion por horas por llave para el mes ", mes, "."))

}


#' Exporta en la ruta de resultados del historico, para cada mes las tablas de tasa de ocupacion
#'
#' @param mes el mes de corte
#'
#' @return exporta en csv en la ruta resultados/historico/historico_mes los archivos
#'
#' @examples
#' calculo_hist_mensual("201812")
#' 
calculo_hist_mensual <- function(mes) {
  
  dir.create(paste0(ruta_result, "historico_", mes))
  pprint(paste0("Se ha creado la carpeta del mes ", mes,", donde van los resultados."))
  
  dicc_cajeros <- fread(paste0(ruta_dicc, carp_dicc_caj, "dicc_cajeros_", mes, ".csv"))
  dicc_cajeros <- dicc_cajeros[, .(num_cajeros = mode(num_cajeros)), by = llave]
  if (sum(is.na(dicc_cajeros$num_cajeros)) == nrow(dicc_cajeros)) {
    stop(paste0("El archivo de diccionario de cajeros para el mes ", mes, " no esta completo. Por favor completelo."))
  }
  rm(dicc_cajeros)
  
  t_ocup_llaves(mes)
  t_ocup_jornada(mes)
  t_ocup_region(mes)
  t_ocup_zona(mes)
  t_ocup_horas(mes)
  t_ocup_mensual_terminal(mes)

}

#' Genera las tablas de resultados POR MESES para los meses que no estan, pero si hay informacion en staging
#'
#' @return exporta las carpetas con tablas mencionadas en la carpeta resultados/historico
#'
#' @examples
#' resultados_mes_historico()
#' 
resultados_mes_historico <- function() {

  comparador_t_ocup <- 
    comparador(inicial_path = ruta_staging, 
               final_path = ruta_result)
  
  if (length(comparador_t_ocup) == 0){
    pprint("Ya se han generado todas las carpetas de resultados del historico, relacionadas a las tablas de staging. \n Si quiere sobreescribir resultados, por favor borre las carpetas que desea sobreescribir. Debe borrar la carpeta, no los archivos dentro de la carpeta.")
  } else {
    pprint(paste0(c("Se van a generar los resultados del historico, relacionados a los siguientes meses: ", comparador_t_ocup)))
    for (mes in comparador_t_ocup){
      calculo_hist_mensual(mes)
    }
  }
}




#' Coge los ultimos 12 archivos del historico para cada tipo de T.O. y los consolida en un unico archivo
#' Nota: el ultimo mes corresponde al mes mas reciente de la carpeta data/canales
#'
#' @return exporta en la ruta resultados/historico/historico/historico_ultimo_ano el historico consolidado
#'
#' @examples
#' agrupar_results_hist(path = "data/canales")
#' 
agrupar_results_hist <- function() {
  
  n_meses <- 12
  #Tambien habria que cambiar la linea que define t_ocup como rbind(archivos)
  
  files <- list.files(ruta_canales)
  position <- as.vector(sapply(files, extraer_numeros))
  position <- sort(position)
  mes <- as.character(position[length(position)])
  
  meses <- mes
  for (i in 1:(n_meses-1)){
    meses <- c(mes_atras_n(mes, i, sep = ""), meses)
  }

  archivos <- c("llaves",
                "horas",
                "jornada",
                "region",
                "terminales",
                "zona")
  
  for (archivo_copia in archivos){
    for (mes_copia in meses){
        assign(paste0("archivo_menos_", -(match(mes_copia, meses)-n_meses)), 
               fread(paste0(ruta_result, "historico_", mes_copia, "/ocup_", archivo_copia, "_", mes_copia, ".csv")))
    }

      t_ocup <- rbind(archivo_menos_11, archivo_menos_10, archivo_menos_9, archivo_menos_8, archivo_menos_7, archivo_menos_6, archivo_menos_5, archivo_menos_4, archivo_menos_3, archivo_menos_2, archivo_menos_1, archivo_menos_0)
      
      fwrite(t_ocup, paste0(ruta_tablas_bi, "/t_ocup_", archivo_copia, ".csv"))
      pprint(paste0("Se ha exportado el archivo acumulado a ", n_meses," meses de la tasa de ocupacion por ", archivo_copia, "."))
      rm(t_ocup)
  }
}


