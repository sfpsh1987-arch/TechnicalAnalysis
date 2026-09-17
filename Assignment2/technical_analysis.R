library(quantmod)
library(TTR)

load_stock_data <- function(file_path) {
  symbols <- scan(file_path, what = "character")
  stock_data <- list()
  
  for (symbol in symbols) {
    data <- getSymbols(
      symbol,
      src = "yahoo",
      auto.assign = FALSE
    )
    stock_data[[symbol]] <- data
  }
  
  return(stock_data)
}
# Function to calculate basic statistics
calculate_statistics <- function(stock_data) {
  
  close_prices <- Cl(stock_data)
  
  # Mode function
  get_mode <- function(x) {
    x <- na.omit(x)
    values <- unique(x)
    values[which.max(tabulate(match(x, values)))]
  }
  
  statistics <- list(
    Mean = mean(close_prices, na.rm = TRUE),
    Median = median(close_prices, na.rm = TRUE),
    Mode = get_mode(as.numeric(close_prices)),
    Standard_Deviation = sd(close_prices, na.rm = TRUE),
    Moving_Average = SMA(close_prices, n = 20)
  )
  
  return(statistics)
}
# Function to generate trading signals
generate_signal <- function(stock_data) {
  
  close_prices <- Cl(stock_data)
  moving_average <- SMA(close_prices, n = 20)
  
  latest_price <- as.numeric(last(close_prices))
  latest_ma <- as.numeric(last(na.omit(moving_average)))
  
  if (latest_price > latest_ma) {
    signal <- "BUY"
  } else if (latest_price < latest_ma) {
    signal <- "SELL"
  } else {
    signal <- "HOLD"
  }
  
  return(signal)
}
# Function to display stock data
display_stock_data <- function(stock_name, stock_data) {
  cat("\n============================\n")
  cat("Stock:", stock_name, "\n")
  cat("============================\n")
  print(head(stock_data))
}