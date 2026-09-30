stoch_rsi <- function(data, period, k_period, d_period) {
  
  # Calculate the RSI
  rsi_values <- rsi(data, period)
  
  # Remove NA values for min and max calculation
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  
  # Calculate the StochRSI
  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  
  # Calculate the %K line
  valid_k <- k_values[!is.na(k_values)]
  k_line <- sma(valid_k, k_period)
  
  # Calculate the %D line
  d_line <- sma(k_line, d_period)
  
  # Return the %K and %D lines
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}
# Test Stochastic RSI
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

stoch_rsi_result <- stoch_rsi(
  data,
  period = 5,
  k_period = 3,
  d_period = 3
)

print(stoch_rsi_result)