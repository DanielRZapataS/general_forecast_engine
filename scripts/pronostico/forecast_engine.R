#' Forecast Engine
#'
#' @param series_temporal 
#' @param holidays 
#' @param offices 
#' @param j 
#' @param meta_pronostico 
#' @param months_to_take 
#' @param trainig_cuts 
#' @param first_date_forecast 
#' @param last_date_forecast 
#' @param xreg_vector 
#'
#' @return
#' @export
#'
#' @examples
                                              
forecast_engine <-
  function(series_temporal,
           holidays,
           offices,
           j,
           meta_pronostico,
           months_to_take = c("since_minimum", "selected_months"),
           trainig_cuts = c(train = 0.7,
                            validate = 0.15,
                            test = 0.15),
           first_date_forecast,
           last_date_forecast,
           xreg_vector = c("holiday",
                           "holidays_pre",
                           "holidays_post",
                           "half_month")) {
    print(paste("forecasting office:", offices[j]))
    ##### data of office ######
    dataset <- series_temporal[llave == offices[j],
                               .(txs = as.numeric(txs), fecha)]
    
    ##### extract meta focasting data of office ####
    weekly_schedule <-
      meta_pronostico[llave == offices[j], horario_definitivo]
    weekly_schedule <-
      as.integer(unlist(strsplit(weekly_schedule, ",")))
    
    frequency_office <-
      meta_pronostico[llave == offices[j], periodicidad]
    
    months_data <-
      meta_pronostico[llave == offices[j], meses_considerar]
    # months_data <- gsub("&", "|", months_data)
    
    minimum_month <-
      as.Date(paste0(meta_pronostico[llave == offices[j], mes_inicio], "-01"))
    
    ###### months filter ####
    months_to_take <- match.arg(months_to_take)
    dataset[, year_month := substr(fecha, 1, 7)]
    
    if (months_to_take == "since_minimum") {
      dataset[, year_month := as.Date(paste0(year_month, "-01"))]
      dataset <- dataset[year_month >= minimum_month, .(txs, fecha)]
    }
    if (months_to_take == "selected_months") {
      dataset[, month_selected := 
                ifelse(str_detect(year_month, months_data), 1, 0)]
      dataset <- dataset[month_selected == 1, .(txs, fecha)]
    }
    
    ##### Making variables ####
    dataset <- data_maker(dataset,
                          holidays,
                          weekly_schedule = weekly_schedule,
                          kaman_filter = "all_series")
    
    ##### spliting dataset ####
    # set.seed(123)
    dataset_list <-
      split_chunk_model(dataset[,!"fecha"], trainig_cuts, allow_random = FALSE)
    dataset_fechas_list <-
      split_chunk_model(dataset[, .(fecha)], trainig_cuts, allow_random = FALSE)
    
    ##### Random forest model ####
    mtry <-
      seq((ncol(dataset_list$train) - 1) / 3, 
          ncol(dataset_list$train) * 0.8, 6) %>%
      round(0)
    nodesize <- seq(3, 9, 3)
    rf_results <-
      randomForest_train(dataset_list$train, mtry , nodesize,
                         dataset_list$validate)

    
    
      
    ##### Time series model ####
    ts_results <-
      ts_models_train(
        matrix_train = dataset_list$train,
        matrix_val = dataset_list$validate,
        frequency_office = frequency_office,
        xreg_vector = xreg_vector
      )
    
      ##### model combination ####
      mcb_results <- model_combination(rf_results, ts_results, dataset_list)
      
      ##### forecas on test 
      test_results <- recursive_forecast(dataset,
                                         rf_results,
                                         ts_results,
                                         mcb_results,
                                         is_test = TRUE,
                                         dataset_fechas_list,
                                         holidays,
                                         xreg_vector,
                                         weekly_schedule,
                                         frequency_office,
                                         office =  offices[j])
      forecast_results <- recursive_forecast(dataset,
                                             rf_results,
                                             ts_results,
                                             mcb_results,
                                             is_test = FALSE,
                                             dataset_fechas_list,
                                             holidays,
                                             xreg_vector,
                                             weekly_schedule,
                                             frequency_office,
                                             from = first_date_forecast, 
                                             to = last_date_forecast,
                                             office =  offices[j])
      
      metrics_table <- test_results$metrics_table
      fit_train <- test_results$fit_train
      fit_val <- test_results$fit_val
      forecast_test <- test_results$forecast_rec
      forecast_rec <- forecast_results$forecast_rec
      
      return(list(metrics_table = metrics_table, 
                  fit_train = fit_train,
                  fit_val = fit_val, forecast_test = forecast_test,
                  forecast_rec = forecast_rec))
 
  }
