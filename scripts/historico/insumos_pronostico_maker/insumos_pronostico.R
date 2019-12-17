
#' Genera en la carpeta data/insumos_pronostico el archivo por llaves para un mes de cajeros y mix transaccional
#'
#' @param mes el mes de corte
#'
#' @return exporta en la carpeta el insumo para el pronostico
#'
#' @examples
#' insumo_pron_mes("201812")
#' 
insumo_pron_mes <- function(mes) {
  
  staging <- fread(file = paste0(ruta_staging, "staging_", mes, ".csv"), showProgress = F)
  staging[, mes := mes]
  
  #agrupar staging para tener el resultado
  result <- staging[, .(porc_menor_1m = sum(valor_tx == "menor a 1m")/.N,
                        porc_mayor_1m = sum(valor_tx == "mayor a 1m")/.N,
                        porc_otros    = sum(valor_tx == "otro")/.N),
                    by = .(mes, llave, oficina_segun_terminal)][order(mes, llave)]
  
  #pegar informacion de cajeros 
  dicc_cajeros <- fread(file = paste0(ruta_dicc, carp_dicc_caj, "dicc_cajeros_", mes, ".csv"))
  dicc_cajeros <- dicc_cajeros[, .(num_cajeros = mode(num_cajeros)), by = llave]
  result <- merge(result, dicc_cajeros, by = "llave", all.x = T, all.y = F)

  result <- result[!is.na(num_cajeros)]
  
  result[, jornada := substr(llave, 1, 2)]
  
  #nombre
  dicc_gobierno <- fread(file = paste0(ruta_dicc, nombre_dic_gob, ".csv"), integer64 = "character", encoding = "Latin-1")
  dicc_gobierno[, nombre := stri_trans_general(dicc_gobierno$nombre,"Latin-ASCII")]
  dicc_gobierno[, region := stri_trans_general(dicc_gobierno$region,"Latin-ASCII")]
  dicc_gobierno[, zona := stri_trans_general(dicc_gobierno$zona,"Latin-ASCII")]
  dicc_gobierno[zona == "", zona := "{No hay informacion}"]
  dicc_gobierno[, codigo_oficina_numeric := as.numeric(str_extract(codigo_oficina, "\\-*\\d+\\.*\\d*"))]
  dicc_gobierno[, region_aux := str_extract_all(region, "\\{[^()]+\\}")]
  dicc_gobierno[, region := substring(region_aux, 2, nchar(region_aux)-1)]
  dicc_gobierno[, zona_aux := str_extract_all(zona, "\\{[^()]+\\}")]
  dicc_gobierno[, zona := substring(zona_aux, 2, nchar(zona_aux)-1)]
  dicc_gobierno <- dicc_gobierno[, .(codigo_oficina_numeric, codigo_oficina, nombre, region, zona)]
  dicc_gobierno[, nombre := paste0(codigo_oficina, " - ", nombre)]
  dicc_gobierno[, nombre := sapply(nombre, quitar_raros)]
  dicc_gobierno[, codigo_oficina := NULL]
  result <- merge(result, dicc_gobierno, by.x = "oficina_segun_terminal", by.y = "codigo_oficina_numeric", all.x = T, all.y = F)
  
  result <- result[, .(llave, jornada, nombre, region, zona, mes, num_cajeros, porc_menor_1m, porc_mayor_1m, porc_otros)][order(mes, llave)]
  
  fwrite(result, paste0(ruta_ins_pron, carpeta_mes, "mes_", mes, ".csv"))
  pprint(paste0("Se ha exportado el insumo por mes para la ejecucion del pronostico, para el mes ", mes, "." ))

}


#' Genera en la carpeta data/insumos_pronostico el archivo por llaves Y HORAS para un mes de cajeros y
#' mix transaccional
#'
#' @param mes el mes de corte
#'
#' @return exporta en la carpeta el insumo para el pronostico
#'
#' @examples
#' insumo_pron_horas("201812")
#' 
insumo_pron_horas <- function(mes) {
  
  staging <- fread(file = paste0(ruta_staging, "staging_", mes, ".csv"), showProgress = F)
  staging[, mes := mes]
  staging$hora_tx <- factor(staging$hora_tx, levels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20", "20-21", "21-22", "22-23", "23-24"))
  
  #tabla de cajeros promedio, para pegar a la tabla por horas
  aux <- staging[, .N, by = .(llave, oficina_segun_terminal, cnl_fecha, hora_tx, numero_caja)]
  aux2 <- aux[, .N, by = .(llave, oficina_segun_terminal, cnl_fecha, hora_tx)]
  cajeros_promedio <- aux2[, .(total_cajas = sum(N),
                               num_dias = uniqueN(cnl_fecha)), by = .(llave, oficina_segun_terminal, hora_tx)]
  cajeros_promedio[, num_cajeros_obs := total_cajas/num_dias]
  cajeros_promedio <- cajeros_promedio[, .(llave, oficina_segun_terminal, hora_tx, num_cajeros_obs)]
  cajeros_promedio[, mes := mes]
  #cajeros_promedio[, jornada := substr(llave, 1, 2)]
  
  #nombre
  #dicc_gobierno <- fread(file = paste0(ruta_dicc, nombre_dic_gob, ".csv"), integer64 = "character")
  #dicc_gobierno[, nombre := stri_trans_general(dicc_gobierno$nombre,"Latin-ASCII")]
  #dicc_gobierno[, codigo_oficina_numeric := as.numeric(str_extract(codigo_oficina, "\\-*\\d+\\.*\\d*"))]
  #dicc_gobierno <- dicc_gobierno[, .(codigo_oficina_numeric, codigo_oficina, nombre)]
  #dicc_gobierno[, nombre := paste0(codigo_oficina, " - ", nombre)]
  #dicc_gobierno[, codigo_oficina := NULL]
  #cajeros_promedio<- merge(cajeros_promedio, dicc_gobierno, by.x = "oficina_segun_terminal", by.y = "codigo_oficina_numeric", all.x = T, all.y = F)
  
  ###proporcion de transacciones en ese mes, hora y llave
  #numero de tx por llave
  por_llave <- staging[, .(tx_total = .N), by = llave]
  #numero de tx por llave y horario
  por_llave_hora <- staging[, .(tx_hora = .N), by = .(llave, hora_tx)]
  #pegarlas
  por_llave_hora <- merge(por_llave_hora, por_llave, by = "llave", all.x = T, all.y = F)
  por_llave_hora[, prop_tx := tx_hora/tx_total]
  por_llave_hora[, tx_hora := NULL]
  por_llave_hora[, tx_total := NULL]
  result <- merge(cajeros_promedio, por_llave_hora, by = c("llave", "hora_tx"), all.x = T, all.y = F)
  
  result <- result[, .(llave, mes, hora_tx, num_cajeros_obs, prop_tx)][order(mes, llave, hora_tx)]
  
  fwrite(result, paste0(ruta_ins_pron, carpeta_horas, "hora_", mes, ".csv"))
  pprint(paste0("Se ha exportado el insumo por horas para la ejecucion del pronostico, para el mes ", mes, "." ))

}


#' Genera los insumos para el pronostico a nivel mes-llave en la carpeta data/insumos_pronostico/mes
#'
#' @return genera en la carpeta final_path, los insumos mencionados
#'
#' @examples
#' insumo_pronostico_mes()
#' 
insumo_pronostico_mes <- function() {

  comparador_insumo_mes <- 
    comparador(inicial_path = ruta_staging, 
               final_path = paste0(ruta_ins_pron, carpeta_mes))
  
  if (length(comparador_insumo_mes) == 0){
    pprint("Ya se han creado todas las tablas de insumo de pronostico a nivel mes.")
  } else {
    pprint(paste0(c("Se van a generar las tablas de insumo de pronostico por mes, relacionadas a los siguientes meses: ", comparador_insumo_mes)))
    for (mes in comparador_insumo_mes){
      insumo_pron_mes(mes)
    }
  }
}



#' Genera los insumos para el pronostico a nivel mes-llave-horario en la carpeta data/insumos_pronostico/horas
#'
#' @return genera en la carpeta final_path, los insumos mencionados
#'
#' @examples
#' insumo_pronostico_horas()
#' 
insumo_pronostico_horas <- function() {
  
  comparador_insumo_hora <- 
    comparador(inicial_path = ruta_staging, 
               final_path = paste0(ruta_ins_pron, carpeta_horas))
  
  if (length(comparador_insumo_hora) == 0){
    pprint("Ya se han creado todas las tablas de insumo de pronostico a nivel horas.")
  } else {
    pprint(paste0(c("Se van a generar las tablas de insumo de pronostico por horas, relacionadas a los siguientes meses: ", comparador_insumo_hora)))
    for (mes in comparador_insumo_hora){
      insumo_pron_horas(mes)
    }
  }
}


#' Genera los insumos para el pronostico - por mes-llave y por mes-llave-horario
#'
#' @return exporta los insumos en las carpetas respectivas de data/insumos_pronosticos
#'
#' @examples
#' insumos_pronostico
insumos_pronostico <- function(){
  insumo_pronostico_mes()
  insumo_pronostico_horas()
}