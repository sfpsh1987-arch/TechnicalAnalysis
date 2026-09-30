sma <- function(data, period) {
  
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Initialize a vector to store the SMA values
  sma_values <- numeric(length(data) - period + 1)
  
  # Calculate SMA for each window
  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    mean_value <- sum(current_window) / period
    sma_values[i] <- mean_value
  }
  
  return(sma_values)
}
# Test SMA
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

sma_result <- sma(data, period = 3)

print(sma_result)
ema <- function(data, period) {
  
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)
  
  # Initialize an empty array to store EMA values
  ema_values <- numeric(length(data))
  
  # Calculate EMA for the first data point
  ema_values[1] <- data[1]
  
  # Calculate EMA for subsequent data points
  for (i in 2:length(data)) {
    ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier +
      ema_values[i - 1]
  }
  
  return(ema_values)
}
