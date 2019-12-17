#'Train multiple Random Forest models chosen from random forest parameters to
#'forecast one step ahead, and return Best Random forest model with the
#' lowest recursive forecast error on validation transactions dates.

#' @param matrix_train: Matrix with target and variables on training.
#' @param mtry: Vector of possible values for number of variables randomly
#' sampled as candidates at each split.(numeric)
#' @param nodesize: Vector of possible values for minimum size of terminal
#' nodes. (numeric)
#' @param sampsize: Vector of possible values of the size of sample to
#' draw. (numeric)
#' @param target_val: Matrix on  validation. (numeric)
#' @return List containing best random forest, rmse, and recursive forecast

randomForest_train <- function(matrix_train, mtry, nodesize,
                               matrix_val) {
  # Create a data frame containing all combinations
  hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize)
  
  RFmodel <- list()
  # Write a loop over the rows of hyper_grid to train the grid of models
  for (i in 1:nrow(hyper_grid)) {
    # Train a Random Forest model
    RFmodel[[i]] <- randomForest(
      formula = txs ~ .,
      data = matrix_train,
      mtry = hyper_grid$mtry[i],
      nodesize = hyper_grid$nodesize[i],
      ntree = 2000,
      importance = T,
      keep.forst = T
    )
    
    
  }
  
  # Identify optimal set of hyperparmeters based on valdation error Evaluate the
  # grid Number of potential models in the grid
  num_models <- length(RFmodel)
  
  # Create an empty vector to store RMSE values
  rmse_values_RF <- c()
  
  # save recursive forecast
  predics_val <- list()
  
  for (i in 1:num_models) {
    # Retreive the i^th model from the list
    model <- RFmodel[[i]]
    
    # Generate predictions on grade_valid
    pred <- predict(object = model,
                    newdata = matrix_val)
    predics_val[[i]] <- pred
    
    # Compute validation RMSE and add to the
    rmse_values_RF[i] <- rmse(actual = matrix_val$txs,
                              predicted = pred)
    
    
  }
  
  names(rmse_values_RF) <- c(paste0("RF", rep(1:nrow(hyper_grid))))
  
  # Identify the model with smallest validation set RMSE
  RF_model <- RFmodel[[which.min(rmse_values_RF)]]
  rmse_train <- rmse(actual = matrix_train$txs,
                     predicted = predict(RF_model))
  rmse_val <- rmse_values_RF[which.min(rmse_values_RF)]
  fit_train <- predict(RF_model)
  fit_val <-  predics_val[[which.min(rmse_values_RF)]]
  
  return(
    list(
      rmse_train = rmse_train,
      rmse_val = rmse_val,
      fit_train = fit_train,
      fit_val = fit_val,
      model_name = paste0(RF_model$call)[1],
      model = RF_model
    )
  )
  
  
}


#'Group of time series models (ARIMA, ETS, NN,TBATS) models from forecast
#'packages, adjusted to forecast one step ahead, and return best time series
#'model with the lowest on step ahead forecast error on validation transactions
#'dates.

#' @param matrix_train:
#' @param matrix_val:
#' @param frequency_office:
#' @param xreg_vector:
#' @return list containing best time series model, rmse, and fit values
ts_models_train <-
  function(matrix_train,
           matrix_val,
           frequency_office = 5,
           xreg_vector = c("holiday",
                           "holidays_pre",
                           "holidays_post",
                           "half_month")) {
    out <- tryCatch(
      {
        matrix_tv <- rbindlist(list(matrix_train, matrix_val))
        matrix_trainxreg <-  matrix_train[, mget(xreg_vector)]
        matrix_valxreg <- matrix_tv[, mget(xreg_vector)]
        
        matrix_trainxreg <- as.matrix(matrix_trainxreg)
        matrix_valxreg <- as.matrix(matrix_valxreg)
        
        train_ts <- ts(matrix_train$txs, frequency = frequency_office)
        ts_models <- list(
          ARIMA = auto.arima(train_ts, xreg = matrix_trainxreg),
          ETS = ets(train_ts),
          NeuralNetwork = nnetar(train_ts, xreg = matrix_trainxreg),
          TBATS = tbats(train_ts, xreg = matrix_trainxreg)
        )
        
        train_fittes <- sapply(ts_models, fitted)
        nn_pivot <- train_fittes[, 3] %>% na.omit %>% length()
        nn_pivot <- nrow(matrix_train) - nn_pivot + 1
        train_fittes <- train_fittes[nn_pivot:nrow(matrix_train),]
        rmse_train <-  apply(train_fittes, 2, rmse,
                             matrix_train$txs[nn_pivot:nrow(matrix_train)])
        
        val_ts <- ts(matrix_tv$txs, frequency = frequency_office)
        ts_validation <- list(
          ARIMA = Arima(val_ts, model = ts_models$ARIMA, xreg = matrix_valxreg),
          ETS = ets(val_ts, model = ts_models$ETS, use.initial.values=TRUE),
          NeuralNetwork = nnetar(val_ts, model = ts_models$NeuralNetwork,
                                 xreg = matrix_valxreg),
          TBATS = tbats(val_ts, model = ts_models$TBATS,
                        xreg = matrix_valxreg)
        )
        
        val_fittes <- sapply(ts_validation , fitted)
        val_fittes <-
          val_fittes[(nrow(matrix_tv) - nrow(matrix_val) + 1):nrow(matrix_tv),]
        rmse_val <-  apply(val_fittes, 2, rmse, matrix_val$txs)
        
        
        model <- which.min(rmse_val)
        fit_train <- train_fittes[, model]
        fit_val <- val_fittes[, model]
        rmse_train <- rmse_train[model]
        rmse_val <- rmse_val[model]
        model_name <-  names(ts_models)[model]
        
        
        return(
          list(
            rmse_train = rmse_train,
            rmse_val = rmse_val,
            fit_train = fit_train,
            fit_val = fit_val,
            model_name = model_name,
            model = model,
            models = ts_models
          )
        )
      },
      error=function(cond) {
        message("ARIMA model caused a error in office, only ETS model used")
        message("Here's the original error message:")
        message(cond)
        
        # Choose a return value in case of error
        
        train_ts <- ts(matrix_train$txs, frequency = frequency_office)
        ets_model <-  ets(train_ts)
        
        train_fittes <- ets_model$fitted
        
        rmse_train <-  rmse(train_ts, train_fittes)
        
        val_ts <- ts(matrix_tv$txs, frequency = frequency_office)
        ets_validation <-
          ets(val_ts, model = ets_model, use.initial.values = TRUE)
        val_fittes <- ets_validation$fitted
        val_fittes <-
          val_fittes[(nrow(matrix_tv) - nrow(matrix_val) + 1):nrow(matrix_tv)]
        
        rmse_val <- rmse(matrix_val$txs, val_fittes)
        
        
        model <- ets_model
        fit_train <- train_fittes
        fit_val <- val_fittes
        rmse_train <- rmse_train
        rmse_val <- rmse_val
        model_name <-  "ETS-simple"
        
        
        return(
          list(
            rmse_train = rmse_train,
            rmse_val = rmse_val,
            fit_train = fit_train,
            fit_val = fit_val,
            model_name = model_name,
            model = model
          )
        )
        
      },
      warning=function(cond) {
        message("ARIMA model caused a warning in office only ETS model used")
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        train_ts <- ts(matrix_train$txs, frequency = frequency_office)
        ets_model <-  ets(train_ts)
        
        train_fittes <- ets_model$fitted
        
        rmse_train <-  rmse(train_ts, train_fittes)
        
        val_ts <- ts(matrix_tv$txs, frequency = frequency_office)
        ets_validation <-
          ets(val_ts, model = ets_model, use.initial.values = TRUE)
        val_fittes <- ets_validation$fitted
        val_fittes <-
          val_fittes[(nrow(matrix_tv) - nrow(matrix_val) + 1):nrow(matrix_tv)]
        
        rmse_val <- rmse(matrix_val$txs, val_fittes)
        
        
        model <- ets_model
        fit_train <- train_fittes
        fit_val <- val_fittes
        rmse_train <- rmse_train
        rmse_val <- rmse_val
        model_name <-  "ETS-simple"
        
        
        return(
          list(
            rmse_train = rmse_train,
            rmse_val = rmse_val,
            fit_train = fit_train,
            fit_val = fit_val,
            model_name = model_name,
            model = model
          )
        )
      }
    )    
    return(out)
  }

#' Model combination
#'
#' @param model1 
#' @param model2 
#' @param dataset_list 
#'
#' @return

model_combination <- function(model1,
                              model2,
                              dataset_list) {
  fit_train1 <- model1$fit_train
  fit_val1 <- model1$fit_val
  model_name1 <- model1$model_name
  
  fit_train2 <- model2$fit_train
  fit_val2 <- model2$fit_val
  model_name2 <- model2$model_name
  
  ts_train <- dataset_list$train$txs
  ts_val <- dataset_list$validate$txs
  
  
  pivot <- length(fit_train1) - length(fit_train2) + 1
  fit_train1  = fit_train1[pivot:length(fit_train1)]
  
  ts_train <- ts_train[pivot:length(ts_train)]
  
  fit_train <- 0.5 * fit_train1 + 0.5 * fit_train2
  fit_val <- 0.5 * fit_val1 + 0.5 * fit_val2
  
  rmse_train <- rmse(ts_train, fit_train)
  rmse_val <- rmse(ts_val, fit_val)
  
  return(
    list(
      rmse_train = rmse_train,
      rmse_val = rmse_val,
      fit_train = fit_train,
      fit_val = fit_val,
      model_name = paste(model_name1, model_name2, sep = "-")
    )
  )
}

