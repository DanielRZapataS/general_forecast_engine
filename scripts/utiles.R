# Sets the locale depending on the system that executes the code
if(Sys.info()["sysname"] == "Linux"){
  Sys.setlocale(category = "LC_TIME", locale = "en_US.utf-8")
} else if(Sys.info()["sysname"] == "Windows"){
  Sys.setlocale(category = "LC_ALL", locale = "English")
} else{
  stop(paste("Configure locales for system with name:", Sys.info()["sysname"]))
}


#' Function which is intended for printing strings in the R console using the C syntax
pprint <- function(...){cat(sprintf(...), "\n")}


#' Canonizes a path given a kind of slash.
#' @param path: path to be canonized (character)
#' @param slash: slash symbol to be used to build the path (character; "/" by default for
#' assuring multi-plataform compatibilities) (character)
#' @return: the path canonized (character)
normalize_path = function(path, slash="/"){
  path = sub(pattern = "\\/\\/", replacement = slash, path)
  path = sub(pattern = "\\/", replacement = slash, path)
  path = sub(pattern = "\\\\", replacement = slash, path)
  return(path)
}

#' Builds a path from chunks
#' @params ...: All the chunks of paths to be loaded. 
#' @return: the path joined and normalized (character)
os.path.join <- function(...){
  normalize_path(file.path(...), slash = "/")
}


#' upload files  
#' @return path of file searched 
get_path <- function(path, key_searcher){
  files <- list.files(path)
  searched_file <- grep(key_searcher, files, value = TRUE)
  searched_path <- os.path.join(path, searched_file)
  return(searched_path)
}


#' Path to process
#'
#' Compare two data folders and return which files do not exist on final path
#' but exist on inicial path
#' @param inicial_path 
#' @param final_path 
#'
#' @return inicial fields paths
#' @export
#'
#' @examples
paths_to_process <- function(inicial_path, final_path) {
  
  #inicial
  files_inicial <- list.files(inicial_path, full.names = T)
  position_inicial <-
    sapply(str_extract_all(files_inicial, "[0-9]+"), "[[", 1) %>% as.numeric
  files_inicial <-
    data.frame(files = files_inicial , position = position_inicial)
  
  #final
  files_final <- list.files(final_path, full.names = T)
  position_final <- sapply(str_extract_all(files_final, "[0-9]+"), "[[", 1) %>% as.numeric
  files_final <-
    data.frame(files = files_final, position = position_final)
  
  #compare
  compare <-
    files_inicial[which(files_inicial$position %!in% files_final$position), ]
  
  
  return(compare$file)
}

#' calculate month 
#'
#' @param month_back : numbers of month lag of current day 
#'
#' @return path of file searched 
get_month <- function(month_back){
  current_month <- today() %>% format(., "%Y-%m-%d")
  previous_month <- floor_date(as.Date(current_month) - months(month_back), "month")
  previous_month <- substr(previous_month, 1,7)
  previous_month <- gsub("-", "", previous_month)
  return(previous_month)
}

#' Make Dates calendar for validation and test data
#' @param from : train_cut for validation data or test_cut for test data (Date)
#' @param to : test_cut for validation data (Date)
dates_maker <- function(from , to, weekly_schedule){
  dates <- data.table(fecha = seq.Date(from = from, to = to, by = "day"))
  dates[, weekday := wday(fecha)]
  dates <- dates[weekday %in% weekly_schedule,]
  return(dates$fecha)
}
#' Checks if a library is currently installed, installs it if not, and imports it.
#' @return: None (void)
import <- function(...){
  gb = lapply(..., function(x){
    if(!x %in% installed.packages()){
      pprint("Library '%s' not found. Installing...", x)
      install.packages(x)
    }
    library(x, character.only = TRUE)
    pprint("Library '%s' imported", x)
  })
}

#' Loads in the global environment the libraries. If their are not installe,
#' it installs them and after that it imports them.
#' @return: None (void)
load_common_libraries <- function(){
  import("jsonlite")
  import("dplyr")
  import("data.table")
  import("xts")
  import("fracdiff")
  import("imputeTS")
  import("xts")
  import("TTR")
  import("stringr")
  import("lubridate")
  import("ggplot2")
  import("stringi")
  import("imputeTS")
  import("tcltk")
  import("tictoc")
  import("forecast")
  import("zoo")
  import("randomForest")
  import("Metrics")
  import("lubridate")
  import("tm")
  import("tidyverse")
} 

load_paths <- function() {
  staging_ruta <<- "data/staging"
  ts_ruta <- "data/serie_temporal"
  month_to_process <<- get_month(1)
  meta_horarios_ruta <<- "data/insumos_pronostico/metadata"
  diccionarios_ruta <<- "data/diccionarios"
  aux_tables_ruta <<- "resultados/pronosticos/aux_tables"
  pronosticos_transacciones_ruta <<-
    "resultados/pronosticos/pronosticos_transacciones"
  ruta_tablas_bi <<- "resultados/tablas_bi"
  ruta_canales   <<- "data/canales/"
  ruta_staging   <<- "data/staging/"
  ruta_serie     <<- "data/serie_temporal/"
  ruta_ins_pron  <<- "data/insumos_pronostico/"
  ruta_paramet   <<- "data/parametros/"
  ruta_dicc      <<- "data/diccionarios/"
  carp_dicc_caj  <<- "dicc_cajeros/"
  carp_meta      <<- "metadata/"
  carp_horarios  <<- "horarios/"
  nombre_dic_gob <<- "dicc_oficinas_gob_Original2"
  ruta_result    <<- "resultados/historico/"
  carpeta_mes    <<- "mes/"
  carpeta_horas  <<- "horas/"
  nomb_festivos  <<- "festivos"
}

demanda_transaccional <- function(){
  #Ejecucion del script de la seccion del historico
  historico()
  # ejecucción motor de pronóstico
  forecast_starter(meses_minimos = 5,
                   trainig_cuts = c(train = 0.7,
                                    validate = 0.15,
                                    test = 0.15),
                   month_to_process,
                   aux_tables_ruta,
                   pronosticos_transacciones_ruta,
                   ruta_serie,
                   meta_horarios_ruta,
                   ruta_dicc
  )
  # ejecucción de resultados 
  forecast_results(meses_minimos = 5,
                   month_to_process,
                   aux_tables_ruta,
                   pronosticos_transacciones_ruta,
                   ruta_serie,
                   meta_horarios_ruta,
                   ruta_dicc,
                   ruta_ins_pron,
                   ruta_tablas_bi)
  
  #Arreglo de la variable de horas en los archivos de bi, para ayudar a los formatos
  arreglo_formato_horas()
  
}


#' Sets the environment of the by importing the necessary modules, loading the 
#' necessary libraries and loading the parametrized paths
#' @return: None (void)
set_environment <- function(){
  load_common_libraries()
  load_paths()
  # historic analysis functions
  source("scripts/historico/staging_maker/auxiliar.R")
  source("scripts/historico/staging_maker/staging_maker.R")
  source("scripts/historico/results_maker/results_maker.R")
  source("scripts/historico/insumos_pronostico_maker/insumos_pronostico.R")
  source("scripts/historico/insumos_pronostico_maker/meta_maker.R")
  source("scripts/historico/insumos_pronostico_maker/series_temporales_maker.R")
  source("scripts/historico/historico.R")
  
  #Forecast analysis 
  forecast <- "scripts/pronostico"
  source(os.path.join(forecast, "data_maker.R"))
  source(os.path.join(forecast, "forecast_engine.R"))
  source(os.path.join(forecast, "forecast_starter.R"))
  source(os.path.join(forecast, "models.R"))
  source(os.path.join(forecast, "recursive_forecast.R"))
  source(os.path.join(forecast, "results_maker.R"))
  source(os.path.join(forecast, "simple_forecast.R"))
  
  #Otros
  source("scripts/otros/arreglo_horas.R")
  
}
