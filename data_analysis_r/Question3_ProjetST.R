# Load libraries 
library(TTR)
library(ggplot2)
library(quantmod)

# Clear the environment
rm(list = ls())


#------------------------------------------------------------------------
# Question 3: Some relevant features
#------------------------------------------------------------------------


# Define the ticker symbol
ticker <- "GOOGL"

# Dates
start_date <- "2007-01-01"
end_date <- "2007-03-31"

# Import data 
getSymbols(ticker, src = "yahoo", from = start_date, to = end_date)
names(GOOGL) <- c("open", "high", "low", "close", "volume", "adjusted")

# Focus on Opening Prices
google_open_prices <- GOOGL$open
google_open_prices <- as.numeric(google_open_prices)

# Calculate the moving average (MA) over 14 day
moving_average <- TTR::SMA(google_open_prices, n = 14)

# Calculate RSI over 14 day
rsi <- TTR::RSI(google_open_prices, n = 14)

# Calculate the Bollinger Bands over 14 day
bollinger <- TTR::BBands(google_open_prices, n = 14)

# Obtain the values of the last 14 days
last_14_prices <- tail(google_open_prices, 14)
last_ma <- tail(moving_average, 1)
last_rsi <- tail(rsi, 1)
last_upper_band <- tail(bollinger[, "up"], 1)
last_lower_band <- tail(bollinger[, "dn"], 1)

cat("MA:", last_ma, "\n")
cat("RSI:", last_rsi, "\n")
cat("Upper Bollinger Band:", last_upper_band, "\n")
cat("Loper Bollinger Band:", last_lower_band, "\n")

dates <- index(GOOGL)

# Data Frame
data_all <- data.frame(
  Date = dates,
  Price = google_open_prices
)

#Dates in english
Sys.setlocale("LC_TIME", "C")

#Visualisation
ggplot(data_all, aes(x = Date, y = Price)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = last_upper_band, color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = last_lower_band, color = "green", linetype = "dashed", size = 1) +
  labs(title = "Stock Prices with Bollinger Bands (Last Observation)",
       x = "Date",
       y = "Price") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal()
