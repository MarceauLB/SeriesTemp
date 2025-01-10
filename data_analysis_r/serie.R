setwd("~/00_Ensai/serie_temporelle/SeriesTemp/data_analysis_r/")
# projetseriestemp@outlook.fr - Vendredi2024!

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

# Import data 
getSymbols(ticker, src = "yahoo",from=start_date,to=end_date)

# Focus on Open Prices
google_open_prices = GOOGL$GOOGL.Open
ts_serie <- ts(google_open_prices)
plot(ts_serie, main = "Initial time series of Google opening prices", col = "darkblue")
plot(google_open_prices, main = "Initial time series of Google opening prices", col = "darkblue")


# The time series has a wigglier trend when time increases. This means that the variance increases over time 
# so we will consider a log-transformation to make the variance constant.
log_ts <- log(ts_serie)
plot(log_ts, main = "Log-transformed time series")

par(mfrow = c(1,2))
acf(log_ts, main = "ACF of the log-transformed time series")
pacf(log_ts, main = "PACF of the log-transformed time series")
dev.off()
# The ACF decreases slowly, suggesting a non-stationarity for the time series.

# Augmented Dickey-Fuller test:
adf.test(log_ts)
# H0 : non-stationary series (existence of a unit root) 
# H1 : stationary series 
# p-value is 0.057 : we cannot reject H0 (with confidence level 5%). 
# The series is not stationary

# We difference it 
serie_diff <- diff(log_ts)
plot(serie_diff, main = "Time series of the log-returns of Google opening prices")

adf.test(serie_diff)
# p-value = 0.01. The time series seems stationary.

par(mfrow = c(1,2))
acf(serie_diff,lag.max = 100)
pacf(serie_diff,lag.max = 100)
dev.off()
# The ACF decreases quickly this time, which suggests once again that
# stationarity has been reached.


res <- auto.arima(log_ts)
res