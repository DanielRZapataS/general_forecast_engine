

#' Forecast comercial office branches starter
#'
#' @param meses_minimos 
#' @param trainig_cuts 
#' @param month_to_process 
#' @param aux_tables_ruta 
#' @param pronosticos_transacciones_ruta 
#' @param ruta_serie 
#' @param meta_horarios_ruta 
#' @param ruta_dicc 
#'
#' @return
#' @export
#'
#' @examples
forecast_starter <- function(meses_minimos = 5,
                             trainig_cuts = c(train = 0.7,
                                              validate = 0.15,
                                              test = 0.15),
                             month_to_process = get_month(1),
                             aux_tables_ruta,
                             pronosticos_transacciones_ruta,
                             ruta_serie,
                             meta_horarios_ruta,
                             ruta_dicc) {
  if (!is.numeric(meses_minimos) | length(meses_minimos) > 1) {
    stop("Los meses minimos elegidos no son correctos")
  }
  
  ##### generating field folders ####
  
  aux_tables_folder <-
    os.path.join(aux_tables_ruta, paste0("aux_tables_", month_to_process))
  suppressWarnings(dir.create(aux_tables_folder))
  
  forecast_folder <-
    os.path.join(
      pronosticos_transacciones_ruta,
      paste0("pronosticos_transacciones_", month_to_process)
    )
  suppressWarnings(dir.create(forecast_folder))
  
  ##### series temporales y cargue de meta data pronósticos ####
  
  message("Cargando series temporales y meta data de pronosticos")
  series_temporal <-
    get_path(ruta_serie , month_to_process) %>% fread()
  meta_pronostico <-
    os.path.join(meta_horarios_ruta, "meta_pronostico.csv") %>% fread()
  holidays <-
    get_path(ruta_dicc, "festivos.csv") %>% fread()
  holidays[, fecha := as.Date(fecha, format = "%d/%m/%Y")]
  holidays[, holiday := 1]
  
  if (is.character(series_temporal) |
      is.character(meta_pronostico)) {
    stop("No se cargaron correctamente las series de oficinas o la meta data")
  }
  
  #### filtrando oficinas con meta data ####
  series_temporal <- series_temporal[llave %in% meta_pronostico$llave]
  ##### generando alertas ####
  
  offices <- unique(meta_pronostico$llave)
  total <- length(offices)
  
  office_60 <- series_temporal[, .N, by = llave][order(N)][N <= 100][,llave]
  office_meses_minimos <- unique(meta_pronostico[meses < meses_minimos, llave])
  
  ##### separating good offices series from bad ones ####
  
  # forecast simple offices
  offices_simple <-
    unique(c(office_60, office_meses_minimos))
  total_simple <- length(offices_simple)
  
  
  # engine forecast offices
  offices <- offices[offices %!in% offices_simple]
  total <- length(offices)
  
  message(
    paste(
      "Se pronosticaran",
      sum(total + total_simple),
      "series de oficinas, tiempo esperado",
      round((sum(total + total_simple) * 1.5) / 60, 0),
      "horas"
    )
  )
  if (length(offices_simple) > 1) {
    warning(
      paste(
        "Existen",
        length(offices_simple),
        paste0("(", round((
          length(offices_simple) / sum(total + total_simple)
        ) * 100, 2), "%)"),
        "oficinas con menos de",
        meses_minimos,
        "de meses utiles para pronosticar o 100 puntos temporales"
      )
    )
    
    warning(
      paste(
        "El pronostico que se proveera no es confiable para las siguientes oficinas:",
        offices_simple %>% as.character() %>% paste(collapse = ",")
      )
    )
    
    warning(
      paste(
        "Ver las oficinas no confiables en el archivo pronosticos_simples.csv en la ruta",
        aux_tables_folder
      )
    )
    
  }
  
  write.csv(
    offices_simple,
    os.path.join(aux_tables_folder, "pronosticos_simples.csv"),
    row.names = F
  )
  
  ##### forecast dates ####
  first_date_forecast <-
    paste0(get_month(0), "01") %>% as.Date(format = c("%Y%m%d"))
  last_date_forecast <-
    floor_date(first_date_forecast + months(12), "month") - days(1)
  
  # Start the clock!
  tic("Forecast engine time")
  
  message("Iniciando motor de pronostico")
  ##### starting forecast engine for normal offices ####
  
  results <- list()
  saver <- c(seq(0, length(offices), 50), length(offices))
  set.seed(1)
  
  # create progress bar
  pb <- tkProgressBar(
    title = "progress bar",
    min = 0,
    max = total,
    width = 300
  )
  #### star from where ? ####
  files <- list.files(forecast_folder)
  if (length(files) > 0) {
    files <- grep("general", files, value = T)
    position <-
      sapply(strsplit(files, "o"), "[[", 1) %>% as.numeric %>% sort
    position <- max(position) + 1
  } else{
    position <- 1
  }
  # j= which(offices == 2116015)
  message("Iniciando motor de pronostico general")
  ##### running forecast engine for each office nature ####
  if(position < total){
    for (j in position:total) {
      # saving results on a list
      results[[j]] <- forecast_engine(
          series_temporal,
          holidays,
          offices,
          j,
          meta_pronostico,
          months_to_take = "since_minimum",
          trainig_cuts = trainig_cuts,
          first_date_forecast,
          last_date_forecast,
          xreg_vector = c("holiday",
                          "holidays_pre",
                          "holidays_post",
                          "half_month")
        )
      
      setTkProgressBar(pb,
                       j,
                       label = paste(
                         round(j / total * 100, 2),
                         "% done (",
                         j,
                         "of",
                         total,
                         ")[general engine]"
                       ))
      gc()
      # export that list on groups
      if (j %in% saver | j == 1 | j == 5 | j == 10 | j == 20) {
        saveRDS(results,
                file = os.path.join(forecast_folder, paste0(j, "offices_general.rds")))
        results <- list()
      }
    }
  }
 
  
  #### starting simple forecastengine for office with less than minimum months ####
  results <- list()
  saver <-
    c(seq(0, length(offices_simple), 50), length(offices_simple))
  set.seed(1)
  
  #### star from where ? ####
  files <- list.files(forecast_folder)
  files <- grep("simple", files, value = T)
  if (length(files) > 0) {
    position <-
      sapply(strsplit(files, "o"), "[[", 1) %>% as.numeric %>% sort
    position <- max(position) + 1
  } else{
    position <- 1
  }
  
  # create progress bar
  pb <- tkProgressBar(
    title = "progress bar",
    min = 0,
    max = total_simple,
    width = 300
  )
  
  message("Iniciando motor de pronostico simple")
  ##### running forecast engine for each office nature ####
  if(position < total_simple){
    for (j in position:total_simple) {
      # saving results on a list
      results[[j]] <-
        simple_forecast(
          series_temporal,
          holidays,
          offices_simple,
          j,
          meta_pronostico,
          kaman_filter = c("all_series"),
          from = first_date_forecast,
          to = last_date_forecast
        )
      
      setTkProgressBar(pb,
                       j,
                       label = paste(
                         round(j / total_simple * 100, 2),
                         "% done (",
                         j,
                         "of",
                         total_simple,
                         ") [simple engine]"
                       ))
      gc()
      # export that list on groups
      if (j %in% saver | j == 1 | j == 5) {
        saveRDS(results,
                file = os.path.join(forecast_folder, paste0(j, "offices_simple.rds")))
        results <- list()
      }
    }
  }
  
  
  toc()
}
