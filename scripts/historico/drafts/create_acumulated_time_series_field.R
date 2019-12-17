
## solo crear el primer mes a mano 
# staging_201711 <- get.path(staging_path, "201711") %>% fread()
# time_series <- staging_201711[, .N, by = .(llave, cnl_fecha)][order(llave, cnl_fecha)]
# 
# names(time_series) <- c("llave", "fecha", "txs")
# 
# fwrite(time_series, os.path.join(ts_path, "serie_temporal_201711.csv"))

##########


#' Create acumulate time series field
#'
#' @param staging_path : staging folder path
#' @param ts_path : time series folder path
#' @param month_to_process : current month to process
#'
#' @return : current acumulate time series 
#'
create_acumulated_time_series_field <- function(staging_path,
                                              ts_path,
                                              month_to_process){
  #  
  month_to_process <- str_extract_all(month_to_process, "[0-9]+") %>% as.numeric()
  
  if(month_to_process < get_month(1)){
    stop(" Time series field is already created")
  }

  if(month_to_process <= str_extract_all(list.files(ts_path), "[0-9]+")%>% as.numeric()){
    stop(" Time series field is already created")
  }
  
  # Load previous time series data 
  print("Load previous time series data ")
  time_series <- list.files(ts_path, full.names = T) %>% fread()
  
  # compare staging folder with time series folder 
  print("compare staging folder with time series folder")
  staging_fileds_paths <-  paths_to_process(staging_path, ts_path)
  
  # load staging fields 
  time_series_list <- list()
  for(i in 1:length(staging_fileds_paths)){
    print(paste("Loading", staging_fileds_paths[i]))
    staging <- as.character(staging_fileds_paths[i]) %>% fread()
    time_series_aux <- staging[, .N, by = .(llave, cnl_fecha)][order(llave, cnl_fecha)]
    names(time_series_aux) <- c("llave", "fecha", "txs")
    time_series_list[[i]] <- time_series_aux
  }
  
  time_series_list <- rbindlist(time_series_list)
  time_series <- rbindlist(list(time_series, time_series_list))
  
  
  # remove previos acumulate time series 
  print("Remove previous acumulate time series")
  list.files(ts_path, full.names = T) %>% file.remove()
  
  fwrite(time_series,
         os.path.join(ts_path, paste0("serie_temporal_", month_to_process, ".csv")))
  
}
