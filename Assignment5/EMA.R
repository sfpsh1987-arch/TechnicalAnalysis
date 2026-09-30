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
# Test EMA
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

ema_result <- ema(data, period = 3)

print(ema_result)