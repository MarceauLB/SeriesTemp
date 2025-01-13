# Create the data for the table
metrics <- c("MAE", "MSE", "RMSE")
predicted_days <- c(5, 22, 250)
data <- data.frame(
  Metrics = rep(metrics, each = 3),
  PredictedDays = rep(predicted_days, times = 3),
  ARIMA = c(1.57, 3.78, 3.30, 3.33, 17.30, 16.44, 1.83, 4.16, 4.05),
  GARCH = c(1.52, 3.47, 5.52, 3.07, 14.46, 55.15, 1.75, 3.80, 7.43),
  LSTM = c(5.31, 15.74, 45.94, 35.74, 310.22, 2273.84, 5.94, 17.61, 45.94)
)


data[data$PredictedDays==5,]
