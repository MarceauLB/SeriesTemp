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

google_open_prices$open
training_data <- subset(google_open_prices, index(google_open_prices) <= "2017-12-31")
test_data <- subset(google_open_prices, index(google_open_prices) > "2017-12-31")
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
tsdiag(best_model_arima, gof.lag = 50, main = "Ljung-Box test on residuals")
# residuals appear centered (first plot), uncorrelated (second plot) 
# but Ljung-Box statistics lie under 0.05 threshold for lag >= 37.

par(mfrow=c(2,1))
hist(best_model_arima$residuals, main = "Histogram of residuals with the proposed best ARIMA model",
     col = "darkblue") # should look normal: not very good
qqnorm(best_model$residuals) # should be almost aligned with the first bissector:
# not the case (outliers?)
qqline(best_model$residuals, col = "red")
dev.off()

# auto.arima model
tsdiag(suggested_model_arima, gof.lag = 50, main = "Ljung-Box test on residuals")

par(mfrow=c(2,1))
hist(best_model_arima$residuals, main = "Histogram of residuals with the ARIMA model with auto.arima",
     col = "darkblue")
qqnorm(best_model$residuals)
qqline(best_model$residuals, col = "red")
dev.off()
# exact same conclusions here

#------------------------------------------------------------------------
# Forecasting
#------------------------------------------------------------------------
# use function "predict"

# We use here the ARIMA Model for making predictions
best_model_arima  # Display the model details

# First 5 data points from the test set
test_diff[1:5]
p5 <- predict(best_model_arima, 5)

# Calculate MAE, MSE, RMSE for 5-step forecast
mae_arima_5 <- mean(abs(test_diff[1:5] - p5$pred))  # Mean Absolute Error
mse_arima_5 <- mean((test_diff[1:5] - p5$pred)^2)  # Mean Squared Error
rmse_arima_5 <- sqrt(mse_arima_5)  # Root Mean Squared Error
mae_arima_5 
mse_arima_5 
rmse_arima_5

# First 22 data points from the test set
test_diff[1:22]
p22 <- predict(best_model_arima, 22)

mae_arima_22 <- mean(abs(test_diff[1:22] - p22$pred))
mse_arima_22 <- mean((test_diff[1:22] - p22$pred)^2)
rmse_arima_22 <- sqrt(mse_arima_22)
mae_arima_22
mse_arima_22 
rmse_arima_22

# First 250 data points from the test set
test_diff[1:250]
p250 <- predict(best_model_arima, 250)
mae_arima_250 <- mean(abs(test_diff[1:250] - p250$pred))
mse_arima_250 <- mean((test_diff[1:250] - p250$pred)^2)
rmse_arima_250 <- sqrt(mse_arima_250)
mae_arima_250
mse_arima_250 
rmse_arima_250


plot(test_diff[1:22], type="l", 
     ylim=c(min(c(p22$pred, test_diff[1:22])), max(c(p5$pred, test_diff[1:22]))), 
     col="blue", 
     main="Actual vs Predicted for 5 Steps", 
     xlab="Time", 
     ylab="Value")
lines(c(p22$pred), col="red")




