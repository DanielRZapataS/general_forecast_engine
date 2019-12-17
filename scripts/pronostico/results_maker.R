
#' Import forecast results
#'
#' @param pathResults : Forecast results
#' @param key_searcher : import general or simple forecast
#'
#' @return
#' @export
#'
#' @examples
import_forecast_results <- function(pathResults, key_searcher) {
  final_results <- list()
  files <- list.files(pathResults)
  files <- grep(key_searcher, files, value = T)
  position <- sapply(strsplit(files, "o"), "[[", 1) %>% as.numeric
  location <- data.table(files = files , position = position)
  location <- location[order(position)]
  iter1 <- c(0, location$position)
  through <- seq(1, length(iter1) - 1)
  
  for (i in through) {
    along <- seq(iter1[i] + 1, iter1[i + 1])
    upload <- os.path.join(pathResults, location[i, files])
    results <- readRDS(upload)
    for (j in along) {
      final_results[[j]] <- results[[j]]
    }
  }
  return(final_results)
}




#' Forecast Results copiler
#'
#' @param meses_minimos
#' @param month_to_process
#' @param aux_tables_ruta
#' @param pronosticos_transacciones_ruta
#' @param ruta_serie
#' @param meta_horarios_ruta
#' @param ruta_dicc
#' @param ruta_ins_pron
#' @param ruta_tablas_bi
#'
#' @return
#' @export
#'
#' @examples
forecast_results <- function(meses_minimos = 5,
                             month_to_process = month_to_process,
                             aux_tables_ruta,
                             pronosticos_transacciones_ruta,
                             ruta_serie,
                             meta_horarios_ruta,
                             ruta_dicc,
                             ruta_ins_pron,
                             ruta_tablas_bi) {
  #### Folders ####
  aux_tables_folder <-
    os.path.join(aux_tables_ruta, paste0("aux_tables_", month_to_process))
  
  forecast_folder <-
    os.path.join(
      pronosticos_transacciones_ruta,
      paste0("pronosticos_transacciones_", month_to_process)
    )
  
  tablas_bi_folder <- ruta_tablas_bi
  ##### series temporales y cargue de meta data pronósticos ####
  
  print("Cargando series temporales y meta data de pronosticos")
  series_temporal <-
    get_path(ruta_serie , month_to_process) %>% fread()
  meta_pronostico <-
    os.path.join(meta_horarios_ruta, "meta_pronostico.csv") %>% fread()
  holidays <-
    get_path(ruta_dicc, "festivos.csv") %>% fread()
  holidays[, fecha := as.Date(fecha, format = "%d/%m/%Y")]
  holidays[, holiday := 1]
  
  ##### separating good offices series from bad ones ####
  
  # engine forecast offices
  offices <- unique(meta_pronostico$llave)
  total <- length(offices)
  
  office_60 <- series_temporal[, .N, by = llave][order(N)][N <= 100][,llave]
  office_meses_minimos <- unique(meta_pronostico[meses < meses_minimos, llave])
  
  ##### separating good offices series from bad ones ####
  
  # forecast simple offices
  offices_simple <-
    unique(c(office_60, office_meses_minimos))
  total_simple <- length(offices_simple)
  
  # engine general forecast offices
  offices <- offices[offices %!in% offices_simple]
  total <- length(offices)
  
  
  ##### forecast dates ####
  first_date_forecast <-
    paste0(get_month(0), "01") %>% as.Date(format = c("%Y%m%d"))
  last_date_forecast <-
    floor_date(first_date_forecast + months(12), "month") - days(1)
  
  ##### importing resukts ####
  results_general <-
    import_forecast_results(forecast_folder, "offices_general")
  results_simple <-
    import_forecast_results(forecast_folder, "offices_simple")

  #### Metric table of general offices ####
  metrics_table <-
    lapply(results_general, "[[", "metrics_table") %>% rbindlist()
  
  
  fwrite(metrics_table,
         os.path.join(aux_tables_folder, "metrics_table.csv"))
 
  
  ##### Counting models ####
  counting_models <- metrics_table[, .N, by = model_name][order(-N)]
  
  write.csv(counting_models,
            os.path.join(aux_tables_folder, "models.csv"))
  
  #### working days ####
  #se cambia lo de zapata jeje
  #dias_hb <- dates_maker(first_date_forecast, last_date_forecast, 2:6)
  #dias_hb <- data.table(fecha = dias_hb[dias_hb %!in% holidays$fecha])
  #dias_hb[, mes := substr(fecha, 1, 7)]
  #dias_hb <- dias_hb[, .(dias_hb = .N) , by = mes]
  #creacion de tabla de horarios
  horarios <- fread(os.path.join(meta_horarios_ruta, "meta_pronostico.csv"), 
                    select = c("llave", "horario_definitivo"))
  for(i in 1:7){
    horarios[, paste0("",i):= 0]
    horarios[grepl(i, horario_definitivo),paste0("",i):= 1 ]
  }
  horarios[, horario_definitivo:= NULL]
  horarios = melt(horarios, "llave", variable.name = "dia", value.name = "trabaja")[order(llave, dia)]
  horarios <- horarios[trabaja == 1]
  horarios[, trabaja := NULL]
  horarios[, dia := as.numeric(as.character(dia))]
  #fechas
  dias_hb <- dates_maker(first_date_forecast, last_date_forecast, 1:7)
  dias_hb <- data.table(fecha = dias_hb[dias_hb %!in% holidays$fecha])
  dias_hb[, mes := substr(fecha, 1, 7)]
  dias_hb[, dia := wday(fecha)]
  conteo_dias_hb <- dias_hb[, .N, .(mes, dia)]
  #cruce
  cruce <- as.data.table(left_join(horarios, conteo_dias_hb, by = "dia"))
  #agrupar
  dias_hb <- cruce[, .(dias_hb = sum(N)), by = .(llave, mes)]
  
  
  ##### Forecast results ####
  forecast_results <-
    lapply(results_general, "[[", "forecast_rec") %>% rbindlist()
  forecast_results_simple <-
    lapply(results_simple, "[[", "forecast_rec") %>% rbindlist()
  forecast_results <-
    rbindlist(list(forecast_results, forecast_results_simple))
  
  #### faltan los pronosticos simples 
  
  setnames(forecast_results, "office", "llave")
  forecast_results[, mes := substr(fecha, 1, 7)]
  # forecast_results[, mes := as.numeric(gsub("-", "", mes))]
  forecast_results <-
    merge(forecast_results, holidays, by = "fecha", all.x = T)
  setkey(forecast_results, llave, fecha)
  forecast_results[is.na(holiday), holiday := 0]
  forecast_results[, txs := forecast_rec]
  forecast_results[holiday == 1, txs := 0]
  
  #### Import offices information ####
  horas_pronostico <-
    get_path(os.path.join(ruta_ins_pron, "horas"), month_to_process) %>%
    fread()
  meses_pronostico <-
    get_path(os.path.join(ruta_ins_pron, "mes"), month_to_process) %>%
    fread()
  tasa_servicio <- get_path(ruta_paramet, "tasa_servicio") %>%
    fread()
  horas_servicio <- get_path(ruta_paramet, "horas_servicios") %>%
    fread()
  
  ##### forecast monthly ######
  forecast_monthly <-
    forecast_results[, .(txs = sum(txs)), by = .(llave, mes)]
  forecast_monthly[, tipo_oficina := as.numeric(substr(llave, 7, 8))]
  forecast_monthly[, cnl_jornada := as.numeric(substr(llave, 1, 2))]
  forecast_monthly <-
    merge(forecast_monthly,
          horas_servicio,
          by = c("tipo_oficina", "cnl_jornada"))
  forecast_monthly <-
    merge(forecast_monthly, meses_pronostico[, -"mes"], by = c("llave"))
  forecast_monthly[,
                   tasa_serv := tasa_servicio$tx_menor_1m * porc_menor_1m +
                     tasa_servicio$tx_mayor_1m * porc_mayor_1m +
                     tasa_servicio$tx_otras * porc_otros]
  #forecast_monthly <- merge(forecast_monthly, dias_hb, by = "mes")) - ANTES ZAPATA
  forecast_monthly <- merge(forecast_monthly, dias_hb, by = c("mes", "llave"))
  forecast_hourly <- copy(forecast_monthly)
  forecast_monthly[,
                   capacidad_mes := horas_jornada * num_cajeros * dias_hb *
                     tasa_serv]
  
  forecast_monthly[, tasa_ocu := txs / capacidad_mes]
  forecast_monthly <-
    merge(forecast_monthly, metrics_table[, .(llave = office, reliability)], by = "llave", all.x = T)
  forecast_monthly[is.na(reliability), reliability := 0]
  print("Oficinas con pronosticos confiables:")
  print(forecast_monthly[, .(confianza =unique(reliability)), by = llave][, .(numero_de_oficinas =  .N), by = confianza])
  
  # hist(forecast_monthly[mes == "2019-06", tasa_ocu])
  fwrite(forecast_monthly,
         os.path.join(tablas_bi_folder, "pronostico_mes.csv"))
  
  #### forecast hourly ####
  forecast_hourly <-
    merge(forecast_hourly,
          horas_pronostico[,-c("mes")],
          by = "llave",
          allow.cartesian = T)
  
  forecast_hourly[, txs_hora := txs * prop_tx]
  forecast_hourly[, capacidad_hora_teo := num_cajeros * dias_hb * tasa_serv]
  forecast_hourly[, capacidad_hora_obs := num_cajeros_obs * dias_hb * tasa_serv]
  forecast_hourly[, tasa_ocu_teo := txs_hora / capacidad_hora_teo]
  forecast_hourly[, tasa_ocu_obs := txs_hora / capacidad_hora_obs]
  forecast_hourly[, hora_pico := ifelse(max(txs_hora) == txs_hora, 1, 0),
                  by = .(llave, mes)]
  forecast_hourly <-
    merge(forecast_hourly, metrics_table[, .(llave = office, reliability)], by = "llave", all.x = T)
  forecast_hourly[is.na(reliability), reliability := 0]
  
  fwrite(forecast_hourly,
         os.path.join(tablas_bi_folder, "pronostico_hora.csv"))
  
  
}

library(data.table)
ID <- c(1,2,2,1)
uniqueN(ID)


x <- data.table(id = c(1,1,2,2,1,1), val1 = 1:6, val2=letters[6:1])

z <- x[,print(.SD), by=id]

x[,.SD[20], by=id]
