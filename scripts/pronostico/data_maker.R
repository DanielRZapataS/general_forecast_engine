#' Data maker
#'
#' @param dataset : dataset from an office (data.table)
#' @param holidays : holidays table
#' @param weekly_schedule : numeric vector indicating weekly schedule of office
#' @param kaman_filter : Apply kalman's filter to fill NA observations. If
#'   kalman_filter = "not_holidays", it's applied to all data points excepts
#'   holidays
#' @references "https://cran.r-project.org/web/packages/imputeTS/imputeTS.pdf"
#' @return : 
data_maker <- function(dataset,
                       holidays,
                       weekly_schedule = c(2, 3, 4, 5, 6),
                       kaman_filter = c("all_series", "not_holidays")) {
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
    dataset[,txs := na_interpolation(txs)]
  }
  if(kaman_filter == "not_holidays"){
    # applying kalma's filter on not holidays dates
    dataset[holiday == 0, txs := na_interpolation(txs)]
    # making series equal to zero on holidays
    dataset[holiday == 1, txs := 0]
  }
  
  #### lag variables ####
  dataset[, ':='(txs_lag1 = shift(txs, 1, 0, "lag"))]
  dataset[, ':='(txs_lag2 = shift(txs, 2, 0, "lag"))]
  dataset[, ':='(txs_lag3 = shift(txs, 3, 0, "lag"))]
  dataset[, ':='(txs_lag4 = shift(txs, 4, 0, "lag"))]
  dataset[, ':='(txs_lag5 = shift(txs, 5, 0, "lag"))]
  dataset[, ':='(txs_lag6 = shift(txs, 6, 0, "lag"))]
  dataset[, ':='(txs_lag7 = shift(txs, 7, 0, "lag"))]
  dataset[, ':='(txs_lag8 = shift(txs, 8, 0, "lag"))]
  dataset[, ':='(txs_lag9 = shift(txs, 9, 0, "lag"))]
  dataset[, ':='(txs_lag10 = shift(txs, 10, 0, "lag"))]
  dataset[, ':='(txs_lag11 = shift(txs, 11, 0, "lag"))]
  dataset[, ':='(txs_lag12 = shift(txs, 12, 0, "lag"))]
  dataset[, ':='(txs_lag13 = shift(txs, 13, 0, "lag"))]
  dataset[, ':='(txs_lag14 = shift(txs, 14, 0, "lag"))]
  dataset[, ':='(txs_lag15 = shift(txs, 15, 0, "lag"))]
  dataset[, ':='(txs_lag16 = shift(txs, 16, 0, "lag"))]
  dataset[, ':='(txs_lag17 = shift(txs, 17, 0, "lag"))]
  dataset[, ':='(txs_lag18 = shift(txs, 18, 0, "lag"))]
  dataset[, ':='(txs_lag19 = shift(txs, 19, 0, "lag"))]
  dataset[, ':='(txs_lag20 = shift(txs, 20, 0, "lag"))]
  
  
  data_cut <- dataset$txs_lag20 != 0
  data_cut <- min(which(data_cut  == TRUE))
  date_cut <- dataset[data_cut, ]$fecha
  
  dataset[, holidays_pre := shift(holiday, 1, 0, "lead")]
  dataset[, holidays_post := shift(holiday, 1, 0, "lag")]
  
  dataset[, half_month := ifelse(day <= 15, 1, 0)]
  
  # last 5 days mean
  dataset[, last5_mean := rollmean(txs, 5, align = "right",
                                   fill = 0)]
  dataset[, last5_mean := shift(last5_mean, 1, 0, "lag")]
  
  # last 10 days mean
  dataset[, last10_mean := rollmean(txs, 10, align = "right",
                                    fill = 0)]
  dataset[, last10_mean := shift(last10_mean, 1, 0, "lag")]
  
  #las 20 days mean
  dataset[, last20_mean := rollmean(txs, 20, align = "right",
                                    fill = 0)]
  dataset[, last20_mean := shift(last20_mean, 1, 0, "lag")]
  
  factors <-
    c("weekday", "day",  "month", "year")
  dataset[, (factors) := lapply(.SD, as.factor), .SDcols = factors]
  levels(dataset$weekday) <- weekly_schedule
  levels(dataset$day) <- 1:31
  levels(dataset$month) <- 1:12
  levels(dataset$year) <- c(2017:2040)
  pivot <-
    which(colnames(dataset) == "txs")
  neworder <-
    c("txs", colnames(dataset)[-pivot])
  setcolorder(dataset, neworder)
  
    dataset <- dataset[fecha >= date_cut, ]
    # dataset[, fecha := NULL]
  return(dataset)
}

#' Split dataset into train, validation and test 
#'
#' @param dataset 
#' @param trainig_cuts 
#'
#' @return a list 

split_chunk_model <- function(dataset,
                              trainig_cuts = c(train = 0.7,
                                               validate = 0.15,
                                               test = 0.15), 
                              allow_random = FALSE) {
  if (!is.numeric(trainig_cuts) |
      length(trainig_cuts) != 3 |
      sum(trainig_cuts < 1) < 3) {
    stop("Los decimales de cortes de entrenamiento no son correctos")
  }
  g <-  cut(seq(nrow(dataset)),
            nrow(dataset) * cumsum(c(0, trainig_cuts)),
            labels = names(trainig_cuts))
  if(allow_random){g <- sample(g)}
  res <-  split(dataset, g)
  return(res)
}
