macd <- function(data, short_period, long_period, signal_period) {
  
  # Calculate the short-term and long-term EMA
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  
  # Calculate the MACD line
  macd_line <- short_ema - long_ema
  
  # Calculate the signal line
  signal_line <- ema(macd_line, signal_period)
  
  # Calculate the histogram
  histogram <- macd_line - signal_line
  
  # Return results
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  return(result)
}
  # Test MACD
  data <- c(100, 105, 110, 115, 120, 125, 130)
  
  macd_result <- macd(
    data,
    short_period = 3,
    long_period = 5,
    signal_period = 2
  )
  
  print(macd_result)
  
  stdev <- function(data) {
    
    # Calculate the mean of the data
    mean_value <- sum(data) / length(data)
    
    # Calculate the differences between data points and the mean
    diff_values <- data - mean_value
    
    # Calculate the squared differences
    squared_diff <- diff_values * diff_values
    
    # Calculate the variance
    variance <- sum(squared_diff) / length(squared_diff)
    
    # Calculate the standard deviation
    standard_deviation <- sqrt(variance)
    
    return(standard_deviation)
  }
 
