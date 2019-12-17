#' Recursive forecast
#'
#' @param dataset
#' @param rf_results
#' @param ts_results
#' @param mcb_results
#' @param is_test
#' @param dataset_fechas_list
#' @param holidays
#' @param xreg_vector
#' @param weekly_schedule
#' @param frequency_office
#' @param from
#' @param to
#'
#' @return
#' @export
#'
#' @examples
recursive_forecast <- function(dataset,
                               rf_results,
                               ts_results,
                               mcb_results,
                               is_test = TRUE,
                               dataset_fechas_list,
                               holidays,
                               xreg_vector,
                               weekly_schedule,
                               frequency_office,
                               from = first_date_forecast,
                               to = last_date_forecast,
                               office =  offices[j]) {
  if (is_test == FALSE) {
    if (!is.Date(from) | !is.Date(to)) {
      stop("Insert dates on from and to argument to forecast")
    }
  }
  ##### dates of forecast ####
  if (is_test) {
    dates_forecast <- dataset_fechas_list$test$fecha
    dataset_rec <-
      dataset[fecha < range(dates_forecast)[1], .(txs, fecha)]
  } else{
    dates_forecast <-
      dates_maker(from = from, to = to, weekly_schedule)
    dataset_rec <-
      dataset[fecha < range(dates_forecast)[1], .(txs, fecha)]
  }
  
  
  ##### Random forest forecast ####
  forecast_rf <- c()
  for (i in 1:length(dates_forecast)) {
    matrix_rec <- data.table(txs = c(forecast_rf, NA),
                             fecha = dates_forecast[1:i])
    l = list(dataset_rec, matrix_rec)
    matrix_rec <- rbindlist(l, use.names = TRUE)
    matrix_rec <- data_maker(matrix_rec,
                             holidays,
                             weekly_schedule = weekly_schedule,
                             kaman_filter = "all_series")
    matrix_rec <-
      matrix_rec[fecha >= dates_forecast[1],!c("fecha")]
    forecast_rf <- predict(rf_results$model, matrix_rec)
  }
  
  ##### time series forecast ####
  
  #### xreg matrix forecast
  if(ts_results$model_name == "ETS-simple"){
    horizon <- length(dates_forecast)
    data_ts <- ts(dataset_rec$txs, frequency = frequency_office)
    ts_model <-  ets(
        data_ts,
        model = ts_results$model,
        use.initial.values = TRUE
      )
    
    forecast_ts  <-
      forecast(ts_model, h = horizon)
    forecast_ts <- as.vector(forecast_ts$mean)
    
  }else{
  
  matrix_xreg <- data.table(fecha = dates_forecast)
  matrix_xreg[, day := as.numeric(substr(fecha, 9, 10))]
  matrix_xreg <-
    merge(matrix_xreg, holidays, by = "fecha", all.x = T)
  matrix_xreg[is.na(holiday), holiday := 0]
  matrix_xreg[, holidays_pre := shift(holiday, 1, 0, "lead")]
  matrix_xreg[, holidays_post := shift(holiday, 1, 0, "lag")]
  matrix_xreg[, half_month := ifelse(day <= 15, 1, 0)]
  matrix_xreg[, weekday := wday(fecha)]
  matrix_xreg <-
    matrix_xreg[weekday %in% weekly_schedule]
  matrix_xreg <- matrix_xreg[, mget(xreg_vector)]
  matrix_xreg <- as.matrix(matrix_xreg)
  
  #### xreg matrix regression
  matrix_xreg_fit <- data.table(fecha = dataset_rec$fecha)
  matrix_xreg_fit[, day := as.numeric(substr(fecha, 9, 10))]
  matrix_xreg_fit <-
    merge(matrix_xreg_fit, holidays, by = "fecha", all.x = T)
  matrix_xreg_fit[is.na(holiday), holiday := 0]
  matrix_xreg_fit[, holidays_pre := shift(holiday, 1, 0, "lead")]
  matrix_xreg_fit[, holidays_post := shift(holiday, 1, 0, "lag")]
  matrix_xreg_fit[, half_month := ifelse(day <= 15, 1, 0)]
  matrix_xreg_fit[, weekday := wday(fecha)]
  matrix_xreg_fit <-
    matrix_xreg_fit[weekday %in% weekly_schedule]
  matrix_xreg_fit <- matrix_xreg_fit[, mget(xreg_vector)]
  matrix_xreg_fit <- as.matrix(matrix_xreg_fit)
  
  horizon <- length(dates_forecast)
  data_ts <- ts(dataset_rec$txs, frequency = frequency_office)
  ts_models <- list(
    ARIMA = Arima(data_ts, model = ts_results$models$ARIMA,
                  xreg = matrix_xreg_fit),
    ETS = ets(
      data_ts,
      model = ts_results$models$ETS,
      use.initial.values = TRUE
    ),
    NeuralNetwork = nnetar(
      data_ts,
      model = ts_results$models$NeuralNetwork,
      xreg = matrix_xreg_fit
    ),
    TBATS = tbats(data_ts, model = ts_results$models$TBATS,
                  xreg = matrix_xreg_fit)
  )
  
  ts_model <- ts_models[[ts_results$model]]
  forecast_ts  <-
    forecast(ts_model, h = horizon, xreg = matrix_xreg)
  forecast_ts <- as.vector(forecast_ts$mean)
  }
  
  ##### Model comnbination forecast ####
  forecast_cb <- 0.5 * forecast_ts + 0.5 * forecast_rf
  
  ##### best model ####
  rmse_competition <- c(rf_results$rmse_val,
                        ts_results$rmse_val,
                        mcb_results$rmse_val)
  
  model <- which.min(rmse_competition)
  
  ##### final forecast ####
  if (model == 1) {
    rmse_train <- rf_results$rmse_train
    rmse_val <- rf_results$rmse_val
    fit_train <- rf_results$fit_train
    fit_val <- rf_results$fit_val
    model_name <- rf_results$model_name
    forecast_rec <- forecast_rf
  }
  if (model == 2) {
    rmse_train <- ts_results$rmse_train
    rmse_val <- ts_results$rmse_val
    fit_train <- ts_results$fit_train
    fit_val <- ts_results$fit_val
    model_name <- ts_results$model_name
    forecast_rec <- forecast_ts
  }
  if (model == 3) {
    rmse_train <- mcb_results$rmse_train
    rmse_val <- mcb_results$rmse_val
    fit_train <- mcb_results$fit_train
    fit_val <- mcb_results$fit_val
    model_name <- mcb_results$model_name
    forecast_rec <- forecast_cb
  }
  
  ##
  if (is_test == TRUE) {
    txs_test <- dataset[fecha %in% dates_forecast,
                        txs]
    error <- txs_test - forecast_rec
    
    metrics_table <- data.table(
      office = office,
      model_name = model_name,
      rmse_train = rmse_train,
      rmse_val = rmse_val,
      rmse_test = rmse(txs_test, forecast_rec),
      mae_test = mae(txs_test, forecast_rec),
      mape_test = mape(txs_test, forecast_rec),
      smape_test = smape(txs_test, forecast_rec),
      max_under_forecast = max(error),
      max_over_forecast = min(error)
    )
    metrics_table[, reliability := ifelse(smape_test < 0.2, 1, 0)]
    
    return(
      list(
        metrics_table = metrics_table,
        fit_train = fit_train,
        fit_val = fit_val,
        forecast_rec = data.table(office = office,
                                  fecha = dates_forecast,
                                  forecast_rec = forecast_rec)
      )
    )
  }
  
  if (is_test == FALSE) {
    metrics_table <- data.table(
      office = office,
      model_name = model_name,
      rmse_train = rmse_train,
      rmse_val = rmse_val
    )
    return(
      list(
        metrics_table = metrics_table,
        fit_train = fit_train,
        fit_val = fit_val,
        forecast_rec = data.table(office = office,
                                  fecha = dates_forecast,
                                  forecast_rec = forecast_rec)
      )
    )
  }
}



