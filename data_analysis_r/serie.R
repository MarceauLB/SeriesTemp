setwd("~/00_Ensai/serie_temporelle/SeriesTemp/data_analysis_r/")

rm(list=ls())

library(quantmod)
library(tseries)
library(forecast)

ticker <- "GOOGL"
getSymbols(ticker, src = "yahoo")

serie_open <- GOOGL$GOOGL.Open
ts_serie <- ts(serie_open$GOOGL.Open)

plot(ts_serie)
adf.test(ts_serie)
# H0 : la série est non stationnaire 
# H1 : la série est stationnaire 
# p-valeur à 0.9782 : on ne peut pas rejetter H0. 
# La série n'est pas stationnaire 
acf(ts_serie)
pacf(ts_serie)

# On la différencie donc :
serie_diff <- diff(ts_serie,na.pad = FALSE)
plot.ts(serie_diff)
adf.test(serie_diff)
# p-val à 0.01. La série semble être stationnarisé. 
acf(serie_diff,lag.max = 100)
pacf(serie_diff,lag.max = 100)

serie_diff2 <- diff(serie_diff,na.pad = FALSE)
plot.ts(serie_diff2)
adf.test(serie_diff2)
acf(serie_diff2,lag.max = 30)
pacf(serie_diff2,lag.max = 30)


res <- auto.arima(ts_serie)








