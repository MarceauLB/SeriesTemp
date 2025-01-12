# Advanced Time Series - Project

# Authors: 
# JARRY Antoine
# MALLICK Gaël
# MAGHAMES Alexandre
# LE BOT Marceau
# BRAULT Tom

#------------------------------------------------------------------------
# Preliminary settings
#------------------------------------------------------------------------

# Clear the environment
rm(list=ls())

# Load libraries 
library(quantmod)
library(tseries)
library(forecast)
library(evd)
library(fracdiff) # For FARIMA models

# Set Seed 
set.seed(13012025)

# Define the ticker symbol
ticker <- "GOOGL"

# Dates
start_date <- "2007-01-01"
end_date <- "2024-12-31"
# We select a large range of dates which could be better for predictions
# in the following

# Import data 
getSymbols(ticker, src = "yahoo",from=start_date,to=end_date)

#------------------------------------------------------------------------
# Question 1: Data Preprocessing
#------------------------------------------------------------------------

# We start by displaying every available time series
names(GOOGL) = c("open","high","low","close","volume","adjusted")

any(is.na(GOOGL)) # no missing value in the time series

par(mfrow=c(1,1))
plot(GOOGL$open, main = "Open Prices", col = "darkblue")
plot(GOOGL$high, main = "High Prices", col = "darkblue")
plot(GOOGL$low, main = "Low Prices", col = "darkblue")
plot(GOOGL$close, main = "Close Prices", col = "darkblue")
plot(GOOGL$volume, main = "Volume Close Prices", col = "darkblue")
plot(GOOGL$adjusted, main = "Adjusted", col = "darkblue")


global_google_ts = ts(GOOGL)
plot(global_google_ts, main = "Time series of each available variable")
# Except the volume time series, all the other time series exhibit the same general trend.

#------------------------------------------------------------------------
# Question 2: Exploratory Data Analysis (EDA)
#------------------------------------------------------------------------
# Divising data until 2018 only
start_date <- "2007-01-01"
end_date <- "2018-12-31"
# We select a large range of dates which could be better for predictions

# Import data 
getSymbols(ticker, src = "yahoo",from=start_date,to=end_date)
names(GOOGL) = c("open","high","low","close","volume","adjusted")


# Focus on Opening Prices
google_open_prices = GOOGL$open
ts_serie <- ts(google_open_prices)
plot(ts_serie, main = "Initial time series of Google opening prices", col = "darkblue")
plot(google_open_prices, main = "Initial time series of Google opening prices", col = "darkblue")


# The trend of the time series has gets wigglier as time increases. This means that the variance increases over time 
# so we will consider a log-transformation to make the variance constant.
log_ts <- log(ts_serie)
plot(log_ts, main = "Log-transformed time series",col="darkblue")


### Trend and seasonality analysis
par(mfrow = c(1,2))
acf(log_ts, main = "ACF of the log-transformed time series",lag.max = 100)
pacf(log_ts, main = "PACF of the log-transformed time series",lag.max=100)
dev.off()
# The ACF decreases slowly, suggesting a non-stationarity for the time series.

# Augmented Dickey-Fuller test:
adf.test(log_ts)
# H0 : non-stationary series (existence of a unit root) 
# H1 : stationary series 
# p-value is 0.1938 : we cannot reject H0 (with confidence level 5%). 
# The series is not stationary

# KPSS test
kpss.test(log_ts)
# H0 : stationary series
# H1 : non-stationary series 
# The KPSS test rejects stationarity (with confidence 95% as p-value = 0.01)

# We difference it 
serie_diff <- diff(log_ts)
plot(serie_diff, main = "Time series of the log-returns of Google opening prices",col="darkblue")

# ADF test
adf.test(serie_diff)
# p-value = 0.01. The time series seems stationary.

# KPSS test
kpss.test(serie_diff)
# p-value = 0.1 -> stationarity

par(mfrow = c(1,1))
acf(serie_diff,lag.max = 100, main="ACF of the diff-log-time series")
pacf(serie_diff,lag.max = 100, main="PACF of the diff-log-time series")
dev.off()
# The ACF cuts off quickly: suggests once again stationarity has been reached.

# We only have one significant peak for the ACF.
# The PACF appears more wiggly, with all spikes close to the significancy level
# or inferior to this level.
# An ARMA(0,1) might be a good first guess for the differenced series,
# that is, an ARIMA(0,1,1) for the log-time series.


# Both the ACF and PACF do not suggest any seasonality.
spec.pgram(serie_diff, col = "darkblue", main = "Smoothing periodogram of the log-returns of Google opening prices",
           span = 4)
# Again, no clear seasonality but note that a season might be hard to find with data for each working day


###################################################################################
# Train and Test datasets
google_open_prices$open
training_data <- subset(google_open_prices, index(google_open_prices) <= "2017-12-31")
train_data <- ts(training_data)
test_data <- subset(google_open_prices, index(google_open_prices) > "2017-12-31")
test_data <- ts(test_data)
train_diff <- serie_diff[1:2768]
test_diff <- serie_diff[2769:3018]
2768+250

#############################################################################################################################"
### Model 1 : Models ARIMA 
#############################################################################################################################"
acf(train_diff,lag.max=100)
pacf(train_diff,lag.max = 100)

# Check best ARIMA model for different values of p and q based on AIC and BIC
p_max <- 7
q_max <- 5
AIC_arima <- matrix(0,p_max,q_max)
BIC_arima <-  matrix(0,p_max,q_max)

for (i in 1:p_max){
  for (j in 1:q_max){
    model <- arima(train_diff, order = c((i-1),0,(j-1)))
    test <- Box.test(model$residuals, lag=30)
    if(test$p.value>0.01){ 
      # Box-Pierce test suggesting no autocorrelation between residuals
      AIC_arima[i,j] = AIC(model)
      BIC_arima[i,j] = BIC(model)}
    else{
      # Box-Pierce test suggesting autocorrelation between residuals (cases to exclude)
      AIC_arima[i,j] = Inf
      BIC_arima[i,j] = Inf
      }
  }
}

AIC_arima
index_best_arima_AIC = which(AIC_arima == min(AIC_arima), arr.ind = TRUE)
index_best_arima_AIC
#AIC => ARIMA(4,1,4) 

BIC_arima
index_best_arima_BIC = which(BIC_arima == min(BIC_arima), arr.ind = TRUE)
index_best_arima_BIC
# BIC => ARIMA(1,1,3)

# ARIMA 
best_model_arima = arima(train_diff, order=c(1,0,3))
best_model_arima
# significant AR1 parameter but with low value


suggested_model_arima <- auto.arima(train_diff)
suggested_model_arima
# ARIMA(3,0,2) suggested

# Further analysis of residuals: Ljung-Box test
# Proposed model
tsdiag(best_model_arima, gof.lag = 100, main = "Ljung-Box test on residuals")
# residuals appear centered (first plot), uncorrelated (second plot) 
# but Ljung-Box statistics lie under 0.05 threshold for lag >= 31.

residuals_arima = residuals(best_model_arima)

# Residuals
plot(residuals_arima, main = "Time series of residuals", ylab = "Residuals", xlab = "Time", type = "l",
     col = "darkblue")
abline(h=mean(residuals_arima), col = "red")

# ACF of residuals
acf(residuals_arima, main = "ACF of Residuals")

# Ljung-Box test p-values
ljung_box <- sapply(1:50, function(lag) Box.test(residuals_arima, lag = lag, type = "Ljung-Box")$p.value)
plot(1:50, ljung_box, type = "b", main = "Ljung-Box Test p-values", 
     xlab = "Lag", ylab = "p-value", col = "black")
abline(h = 0.05, col = "blue", lty = 2)

par(mfrow=c(2,1))
hist(best_model_arima$residuals, main = "Histogram of residuals with the proposed best ARIMA model",
     breaks = 200, xlim = c(-0.1,0.1), col = "darkblue") # should look normal: not very good
qqnorm(best_model_arima$residuals) # should be almost aligned with the first bissector:
# should look normal: not very good
# not the case (outliers?)
qqline(best_model_arima$residuals, col = "red")
dev.off()

# auto.arima model
tsdiag(suggested_model_arima, gof.lag = 50, main = "Ljung-Box test on residuals")

par(mfrow=c(2,1))
hist(suggested_model_arima$residuals, main = "Histogram of residuals with the ARIMA model with auto.arima",
     col = "darkblue",breaks=50)
qqnorm(suggested_model_arima$residuals)
qqline(suggested_model_arima$residuals, col = "red")
dev.off()
# exact same conclusions here

#------------------------------------------------------------------------
# Forecasting
#------------------------------------------------------------------------
# use function "predict"
par(mfrow=c(1,1))
plot(test_data,main = "Test Data", col="darkblue")

# We use here the ARIMA Model for making predictions
best_model_arima  # Display the model details

# First 5 data points from the test set
test_data[1:5]
p5 <- predict(best_model_arima, 5)
pred5 <- train_data[length(train_data)]*exp(cumsum(p5$pred))

# Calculate MAE, MSE, RMSE for 5-step forecast
mae_arima_5 <- mean(abs(test_data[1:5] - pred5))  # Mean Absolute Error
mse_arima_5 <- mean((test_data[1:5] - pred5)^2)  # Mean Squared Error
rmse_arima_5 <- sqrt(mse_arima_5)  # Root Mean Squared Error
mae_arima_5 
mse_arima_5 
rmse_arima_5

# First 22 data points from the test set
test_data[1:22]
p22 <- predict(best_model_arima, 22)
pred22 <- train_data[length(train_data)]*exp(cumsum(p22$pred))

mae_arima_22 <- mean(abs(test_data[1:22] - pred22))
mse_arima_22 <- mean((test_data[1:22] - pred22)^2)
rmse_arima_22 <- sqrt(mse_arima_22)
mae_arima_22
mse_arima_22 
rmse_arima_22

# First 250 data points from the test set
test_data[1:250]
p250 <- predict(best_model_arima, 250)
pred250 <- train_data[length(train_data)]*exp(cumsum(p250$pred))

mae_arima_250 <- mean(abs(test_data[1:250] - pred250))
mse_arima_250 <- mean((test_data[1:250] - pred250)^2)
rmse_arima_250 <- sqrt(mse_arima_250)
mae_arima_250
mse_arima_250 
rmse_arima_250

par(mfrow=c(1,1))

pred5_arima <- c(pred5, rep(NA, 250 - length(pred5)))
pred22_arima <- c(pred22, rep(NA, 250 - length(pred22)))
pred250_arima <- c(pred250, rep(NA, 250 - length(pred250)))

# Create a data frame combining actual and predicted values
create_data <- data.frame(
  test_data = test_data[1:250], 
  pred5 = pred5_arima, 
  pred22 = pred22_arima, 
  pred250 = pred250_arima
)
setwd("~/00_Ensai/serie_temporelle/SeriesTemp/data_analysis_r/out-predictions/")
write.csv(create_data, "forecast_predictions_arima.csv", row.names = FALSE)


#################################################################################
# Prédictions for ARIMA, GARCH and LSTM (5, 22 et 250 jours) 
#################################################################################
#Lire les données à partir du fichier CSV
data_arima <- read.csv("forecast_predictions_arima.csv")
data_garch <- read.csv("forecast_predictions_garch.csv")
data_lstm <- read.csv("forecast_predictions_lstm.csv")

#------------------------------------------------------------------------
# 1. Plot for 5 values
#------------------------------------------------------------------------
plot(data_arima$test_data[1:5], type="l", 
     ylim=c(min(c(data_arima$pred5[1:5], data_arima$test_data[1:5], data_garch$pred5[1:5], data_lstm$pred5[1:5])), 
            max(c(data_arima$pred5[1:5], data_arima$test_data[1:5], data_garch$pred5[1:5],data_lstm$pred5[1:5]))), 
     col="darkblue", 
     main="Actual vs Predicted for 5 Steps", 
     xlab="Time", 
     ylab="Value")
lines(data_arima$pred5[1:5], col="darkred", type="l")
lines(data_garch$pred5[1:5], col="darkgreen", type="l")
lines(data_lstm$pred5[1:5], col="darkorange", type="l")
legend(x=3.5,y=50, legend=c("Actual (Test Data)", "ARIMA Prediction", "GARCH Prediction","LSTM Prediction"), 
       col=c("darkblue", "darkred", "darkgreen","darkorange"), lty=1, cex=0.9)


#------------------------------------------------------------------------
# 2. Plot for 22 values
#------------------------------------------------------------------------
plot(data_arima$test_data[1:22], type="l",
     ylim=c(min(c(data_arima$test_data[1:22], data_arima$pred22[1:22], data_garch$pred22[1:22], data_lstm$pred22[1:22])), 
            max(c(data_arima$test_data[1:22], data_arima$pred22[1:22], data_garch$pred22[1:22], data_lstm$pred22[1:22]))), 
     col="darkblue", 
     main="Actual vs Predicted for 22 Steps", 
     xlab="Time", 
     ylab="Value")
lines(data_arima$pred22[1:22], col="darkred", type="l")
lines(data_garch$pred22[1:22], col="darkgreen", type="l")
lines(data_lstm$pred22[1:22], col="darkorange", type="l")
legend("bottomleft", legend=c("Actual (Test Data)", "ARIMA Prediction", "GARCH Prediction","LSTM Prediction"), 
       col=c("darkblue", "darkred", "darkgreen","darkorange"), lty=1, cex=0.9)


#------------------------------------------------------------------------
# 3. Plot for 250 values
#------------------------------------------------------------------------
plot(data_arima$test_data[1:250], type="l", 
     ylim=c(min(c(data_arima$test_data[1:250], data_arima$pred250[1:250], data_garch$pred250[1:250], data_lstm$pred250[1:250])), 
            max(c(data_arima$test_data[1:250], data_arima$pred250[1:250], data_garch$pred250[1:250],data_lstm$pred250[1:250]))), 
     col="darkblue", 
     main="Actual vs Predicted for 250 Steps", 
     xlab="Time", 
     ylab="Value")
lines(data_arima$pred250[1:250], col="darkred", type="l")
lines(data_garch$pred250[1:250], col="darkgreen", type="l")
lines(data_lstm$pred250[1:250], col="darkorange", type="l")
legend(x=150,y = 30, legend=c("Actual (Test Data)", "ARIMA Prediction", "GARCH Prediction","LSTM Prediction"), 
       col=c("darkblue", "darkred", "darkgreen","darkorange"), lty=1, cex=0.9)





### FARIMA model
# This model requires a stationary time series in input so we will use the 
# differenced series (otherwise, the log-series returns an error)

# Check best FARIMA model for different values of p and q based on AIC and BIC
p_max <- 7
q_max <- 5
AIC_farima <- matrix(0,p_max,q_max)
BIC_farima <-  matrix(0,p_max,q_max)

for (i in 1:p_max){
  for (j in 1:q_max){
    model <- fracdiff(train_diff, nar = i-1, nma = j-1)
    test <- Box.test(model$residuals, lag=30)
    if(test$p.value>0.01){ 
      # Box-Pierce test suggesting no autocorrelation between residuals
      AIC_farima[i,j] = AIC(model)
      BIC_farima[i,j] = BIC(model)}
    else{
      # Box-Pierce test suggesting autocorrelation between residuals (cases to exclude)
      AIC_farima[i,j] = Inf
      BIC_farima[i,j] = Inf
    }
  }
}

AIC_farima
index_best_farima_AIC = which(AIC_farima == min(AIC_farima), arr.ind = TRUE)
index_best_farima_AIC
# lower AIC => p = 2 and q = 4

BIC_farima
index_best_farima_BIC = which(BIC_farima == min(BIC_farima), arr.ind = TRUE)
index_best_arima_BIC
# lower BIC => p = 1 and q = 3

# For sparsity reasons, we will once again keep the second model with p = 1 and q = 3.

# Estimation and confidence interval of the fractional differencing parameter
best_model_farima = fracdiff(train_diff, nar = 1, nma = 1)
d_hat = best_model_farima$d
d_se = best_model_farima$stderror.dp[1]

conf_int_fractional_diff_param = c(d_hat -1.96*d_se, d_hat + 1.96*d_se)
conf_int_fractional_diff_param
# 0 belongs to the confidence interval, suggesting that a FARIMA model is not adapted
# to our data. An ARIMA model seems better here.



