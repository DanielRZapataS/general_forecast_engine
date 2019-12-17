
#' Generacion de la tabla de staging a partir del insumo de canales
#'
#' @param mes el mes de corte
#'
#' @return exporta en la ruta de data/staging la tabla en formato csv
#'
#' @examples
#' generacion_staging(mes = "201812")
#' 
generacion_staging <- function(mes) {
  
  canales <- fread(file = paste0(ruta_canales, "canales_", mes, ".csv"), integer64 = "character", showProgress = F)
  pprint(paste0("Se ha cargado la data de canales del mes ", mes,". Empieza el procesamiento..."))
  names(canales) <- tolower(names(canales))
  
  #1: filtro por servicio
  dicc_serv <- fread(file = paste0(ruta_paramet, "transacciones_caja.csv"), integer64 = "character")
  canales <- canales[cnl_serv %in% dicc_serv$cnl_serv]
  rm(dicc_serv)
  
  #2: filtro por estado
  dicc_estado <- fread(file = paste0(ruta_paramet, "estados_transaccionales.csv"), integer64 = "character")
  canales <- canales[cnl_estado %in% dicc_estado$cnl_estado]
  rm(dicc_estado)
  
  #variables auxiliares para el procesamiento
  canales[, ultimos_dos_terminal := substr_right(cnl_terminal, 2)]
  canales[, ultimos_una_terminal := substr_right(cnl_terminal, 1)]
  canales[, primeras_dos_terminal := substr(cnl_terminal, 1, 2)]
  canales[, weekday := wday(as.Date(cnl_fecha))]
  
  #traer diccionario de filtros
  dicc_filtros <- fread(file = paste0(ruta_paramet, "filtros_terminal.csv"), colClasses = "character")
  dicc_filtros[ultimos_dos_terminal %in% 1:9, ultimos_dos_terminal := paste0("0", ultimos_dos_terminal)]
  #filtro primeros dos caracteres
  canales <- canales[(primeras_dos_terminal %in% dicc_filtros$primeras_dos_terminal)] 
  #filtro caracteres finales
  canales <- canales[ultimos_dos_terminal %in% dicc_filtros$ultimos_dos_terminal | ultimos_una_terminal %in% dicc_filtros$ultimos_una_terminal] 
  #quitar windows del codigo de la terminal: nuevo campo
  canales[, cnl_terminal_sinwindows := gsub(dicc_filtros$windows[1], "", cnl_terminal)]
  for (i in 2:length(dicc_filtros$windows)){
    canales[, cnl_terminal_sinwindows := gsub(dicc_filtros$windows[i], "", cnl_terminal_sinwindows)]
  }
  
  #para los que terminan en 1:19, quitar los ultimos dos digitos. para las que terminan en E, V, quitar la letra
  canales[, cnl_terminal_sinwindows_sinfinal := ifelse(ultimos_una_terminal %in% dicc_filtros$ultimos_una_terminal,
                                                       substr(cnl_terminal_sinwindows, 1, nchar(cnl_terminal_sinwindows) - 1),
                                                       substr(cnl_terminal_sinwindows, 1, nchar(cnl_terminal_sinwindows) - 2))]
  #extraer numero de oficina
  canales[, oficina_segun_terminal := as.numeric(str_extract(cnl_terminal_sinwindows_sinfinal, "\\-*\\d+\\.*\\d*"))]
  
  #hay algunas que son solo servidores, porque no tienen al final el codigo de si es cajero o no
  canales[, ncarac := nchar(cnl_terminal)]
  canales <- canales[ncarac > 7]
  
  #paso siguiente: cruzar con gobierno de datos
  dicc_gobierno <- fread(file = paste0(ruta_dicc, nombre_dic_gob, ".csv"), integer64 = "character", encoding = "Latin-1")
  dicc_gobierno[, region := stri_trans_general(dicc_gobierno$region,"Latin-ASCII")]
  dicc_gobierno[, zona := stri_trans_general(dicc_gobierno$zona,"Latin-ASCII")]
  dicc_gobierno[, nombre := stri_trans_general(dicc_gobierno$nombre,"Latin-ASCII")]
  dicc_gobierno[, nombre := sapply(nombre, quitar_raros)]
  dicc_gobierno[, tipo_oficina := as.numeric(str_extract(tipo_oficina, "\\-*\\d+\\.*\\d*"))]
  dicc_gobierno[, codigo_oficina_numeric := as.numeric(str_extract(codigo_oficina, "\\-*\\d+\\.*\\d*"))]
  
  staging <- merge(canales, dicc_gobierno, 
                  by.x = "oficina_segun_terminal",
                  by.y = "codigo_oficina_numeric",
                  all.x = T,
                  all.y = F)
  
  #filtros sin archivo de gobierno
  staging <- staging[tipo_oficina %in% dicc_filtros$tipo_oficina]
  
  #horarios adicional los sabados: la jornada pasa a ser 23
  staging[cnl_jornada == 22 & weekday == 7,
          cnl_jornada := 23]
  
  #creacion de la llave
  staging[, oficina_segun_terminal := str_pad(oficina_segun_terminal, 4, pad = "0")]
  staging[, tipo_oficina := substr_right(tipo_oficina, 1)]
  staging[, llave := paste0(cnl_jornada, oficina_segun_terminal, tipo_oficina)]
  
  #creacion hora en categorica
  staging[, hora_tx := ifelse(nchar(cnl_hora) == 6,
                             paste0(substr(cnl_hora, 1, 2), "-", as.numeric(substr(cnl_hora, 1, 2)) + 1),
                             paste0(substr(cnl_hora, 1, 1), "-", as.numeric(substr(cnl_hora, 1, 1)) + 1))]
  staging$hora_tx <- factor(staging$hora_tx, levels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20", "20-21", "21-22", "22-23", "23-24"))
  
  #valor transaccion categorica
  staging[, cnl_total := ifelse(is.na(cnl_total), cnl_efectivo, cnl_total)]
  staging[is.na(cnl_cheques) | cnl_cheques == 0,
         cnl_efectivo := ifelse (is.na(cnl_efectivo) | cnl_efectivo == 0, cnl_total, cnl_efectivo)]
  staging[cnl_efectivo >= 0, 
         valor_tx := ifelse(cnl_efectivo >= 1000000, "mayor a 1m",
                              ifelse(cnl_efectivo < 1000000, "menor a 1m", "error"))]
  staging[is.na(cnl_efectivo), valor_tx := "otro"]
  
  #numero de cajero
  suppressWarnings(staging[, numero_caja := as.numeric(substr_right(cnl_terminal_sinwindows, 2))])
  staging[is.na(numero_caja), numero_caja := 1]
  
  variables <- c("llave",
                 "cnl_jornada",
                 "oficina_segun_terminal",
                 "tipo_oficina",
                 "cnl_terminal",
                 "cnl_terminal_sinwindows",
                 "numero_caja",
                 "cnl_usuario",
                 "cnl_fecha",
                 "hora_tx",
                 "valor_tx")
  
  staging <- staging[, mget(variables)]
  
  fwrite(staging, paste0(ruta_staging, "staging_", mes, ".csv"))
  pprint(paste0("Se ha exportado la tabla de staging relacionada al mes ", mes, "."))
  
  insumo_dicc_cajeros <- data.table(llave = unique(staging$llave), num_cajeros = "")
  fwrite(insumo_dicc_cajeros, paste0(ruta_dicc, carp_dicc_caj, "dicc_cajeros_", mes_adelante_n(mes, n = 1, sep = ""), "_insumo.csv"))
  pprint(paste0("Se ha exportado la tabla de diccionario de cajeros relacionada al mes ", mes_adelante_n(mes, n = 1, sep = ""), ". \n ", "Por favor, completela para la corrida del proximo mes."))
  
  gc()
  
  #return(staging)
}



#' Genera las tablas de staging para los meses que no estan, pero si hay informacion en canales
#'
#' @return exporta las tablas mencionadas en la carpeta data/staging
#'
#' @examples
#' staging()
#' 
staging <- function() {
  
  comparador_staging <- 
    comparador(inicial_path = ruta_canales, 
               final_path = ruta_staging)
  
  if (length(comparador_staging) == 0){
    pprint("Ya se han creado todas las tablas de staging relacionadas a la data de canales.")
  } else {
    pprint(paste0(c("Se van a generar las tablas de staging relacionadas a los siguientes meses: ", comparador_staging)))
    for (mes in comparador_staging){
      generacion_staging(mes)
    }
  }

}
