setwd("~/00_Ensai/serie_temporelle/SeriesTemp/data_analysis_r/")

# Author: Alexandre Maghames 

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
start_date <- "2007-01-01"
end_date <- "2024-12-31"

#Import data 
getSymbols(ticker, src = "yahoo",from=start_date,to=end_date)

# Focus Open 
ts_serie <- ts(GOOGL$GOOGL.Open)
plot(ts_serie)
adf.test(ts_serie)
# H0 : la série est non stationnaire 
# H1 : la série est stationnaire 
# p-valeur à 0.9782 : on ne peut pas rejetter H0. 
# La série n'est pas stationnaire 
acf(ts_serie)
pacf(ts_serie)

# on va travailler sur la log(serie)
log_ts <- log(ts_serie)
plot(log_ts)
adf.test(log_ts) 
# p-val 0.056. On ne peut pas rejetter H0. La série est non stationnaire 

# on la différencie 
serie_diff <- diff(log_ts)
plot(serie_diff)
adf.test(serie_diff)
# p-val à 0.01. La série semble être stationnarisé. 
acf(serie_diff,lag.max = 100)
pacf(serie_diff,lag.max = 100)


res <- auto.arima(log_ts)
res







