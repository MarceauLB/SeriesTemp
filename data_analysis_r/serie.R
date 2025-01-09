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

global_google_ts = ts(GOOGL)
plot(global_google_ts, main = "Time series of each available variable")
# Except the volume time series, all the other time series exhibit the same general trend.

#------------------------------------------------------------------------
# Question 2: Exploratory Data Analysis (EDA)
#------------------------------------------------------------------------

# Focus on Opening Prices
google_open_prices = GOOGL$open
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