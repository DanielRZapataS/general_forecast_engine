
#' Crea en la carpeta de series temporales, el archivo historico de tx por fecha
#'
#' @return genera en la carpeta el archivo mencionado
#'
series_temporales <- function() {

  #considerar todos los archivos de staging
  files_staging <- list.files(ruta_staging)
  position <- as.vector(sapply(files_staging, extraer_numeros))
  mes <- as.character(max(position))
  
  pprint("Se van a cargar las tablas de staging para generar la serie temporal.")
  
  time_series_list <- list()
  
  file_series <- list.files(ruta_serie)
  
  #Si no hay archivo de series temporales en la carpeta
  if (length(file_series) == 0){
    
    #cargar todos los archivos de staging y sacar cantidad de transacciones por llave y fecha
    for(i in 1:length(files_staging)){
      pprint(paste("Cargando", files_staging[i]))
      staging <- fread(paste0(ruta_staging, as.character(files_staging[i])), showProgress = F) 
      time_series_aux <- staging[, .N, by = .(llave, cnl_fecha)][order(llave, cnl_fecha)]
      names(time_series_aux) <- c("llave", "fecha", "txs")
      time_series_list[[i]] <- time_series_aux
    }
    #pegar la informacion
    time_series <- rbindlist(time_series_list)
    
    #list.files(ruta_serie, full.names = T) %>% file.remove()
    #pprint("Se ha eliminado el archivo anterior de series temporales")
    #sobreescribir el archivo
    fwrite(time_series, paste0(ruta_serie, "serie_temporal_", mes, ".csv"))
    pprint(paste0("Se ha generado el archivo de series temporales, con corte del mes ", mes))  
    
  } else {
    
    serie_actual <- fread(paste0(ruta_serie, file_series))
    meses_existentes <- as.numeric(unique(paste0(year(serie_actual$fecha), str_pad(month(serie_actual$fecha), 2, pad = 0))))
    
    meses_faltantes <- position[position %!in% meses_existentes]
    
    if (length(meses_faltantes) == 0){
      pprint("El archivo de series temporales ya incorpora todos los meses existentes en staging.")
    } else {
      
      for (i in 1:length(meses_faltantes)){
        pprint(paste0("Cargando staging_", meses_faltantes[i], ".csv"))
        staging <- fread(paste0(ruta_staging, "staging_", as.character(meses_faltantes[i]), ".csv"), showProgress = F) 
        time_series_aux <- staging[, .N, by = .(llave, cnl_fecha)][order(llave, cnl_fecha)]
        names(time_series_aux) <- c("llave", "fecha", "txs")
        time_series_list[[i]] <- time_series_aux
      }
      #pegar la informacion
      time_series <- rbindlist(time_series_list)
      
      time_series <- rbind(serie_actual, time_series)
      time_series <- time_series[order(llave, fecha)]
      
      list.files(ruta_serie, full.names = T) %>% file.remove()
      pprint("Se ha eliminado el archivo anterior de series temporales")
      
      #sobreescribir el archivo
      fwrite(time_series, paste0(ruta_serie, "serie_temporal_", mes, ".csv"))
      pprint(paste0("Se ha generado el archivo de series temporales, con corte del mes ", mes))  
    }
  }
}

