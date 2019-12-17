
#' Obtain simple ets forecast from a series
#'
#' @param j 
#' @param kaman_filter 
#' @param frequency_office 
#' @param from 
#' @param to 
#'
#' @return
simple_forecast <- function(series_temporal,
                            holidays,
                            offices_simple,
                            j,
                            meta_pronostico,
                            kaman_filter = c("all_series"),
                            from = first_date_forecast,
                            to = last_date_forecast
) {
  print(paste("forecasting office:", offices_simple[j]))
  ##### data of office ######
  dataset <- series_temporal[llave == offices_simple[j],
                             .(txs = as.numeric(txs), fecha)]
  
  ##### extract meta focasting data of office ####
  weekly_schedule <-
    meta_pronostico[llave == offices_simple[j], horario_definitivo]
  weekly_schedule <-
    as.integer(unlist(strsplit(weekly_schedule, ",")))
  
  frequency_office <-
    meta_pronostico[llave == offices_simple[j], periodicidad]
  
  ##### Making variables ####
  dataset[, fecha := as.Date(fecha)]
  dataset[, ':='(day = day(fecha),
                 month = month(fecha),
                 year = year(fecha))]
  
  dates_range <-
    range(dataset$fecha)  #get max and min dates in base
  
  calendar <-
    seq.Date(from = dates_range[1], to = dates_range[2],
             by = "day") # sequence of calendar dates
  calendar <- data.table(fecha = calendar)
  calendar[, day := day(fecha)]
  calendar[, month := month(fecha)]
  calendar[, year := year(fecha)]
  
  # include all calendar dates in main base. Then replace NAs
  dataset <- merge(
    dataset,
    calendar,
    by = c("fecha", "day", "month", "year"),
    all.y = TRUE
  )
  rm(calendar)
  
  # include holidays dates as dummy variable
  dataset <-
    merge(dataset, holidays, by = "fecha", all.x = T)
  dataset[is.na(holiday), holiday := 0]
  ##### building a propper time series ####
  dataset[, weekday := wday(fecha)]
  dataset <-
    dataset[weekday %in% weekly_schedule]
  
  ##### applyng kalman's filter
  kaman_filter <- match.arg(kaman_filter)
  if(kaman_filter == "all_series"){
    # applying kalma's filter on all series
    dataset[,txs := na.interpolation(txs)]
  }
  if(kaman_filter == "not_holidays"){
    # applying kalma's filter on not holidays dates
    dataset[holiday == 0, txs := na.interpolation(txs)]
    # making series equal to zero on holidays
    dataset[holiday == 1, txs := 0]
  }
  
  dates_forecast <-
    dates_maker(from = from, to = to, weekly_schedule)
  
  train_ts <- ts(dataset$txs, frequency = frequency_office)
  ets <- ets(train_ts)
  horizon <- length(dates_forecast)
  forecast_ts  <- forecast(ets, h = horizon)
  forecast_ts <- as.vector(forecast_ts$mean)
  return(list(forecast_rec = data.table(office = offices_simple[j],
                                        fecha = dates_forecast,
                                        forecast_rec = forecast_ts), 
              model_name = "ets-simple"))
}


