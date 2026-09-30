linreg <- function(regressionSource, regressionLength, regressionOffset) {
  
  # Calculate the total number of elements
  n <- length(regressionSource)
  
  # Check regressionLength
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  
  # Check regressionOffset
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  
  # Calculate starting and ending index
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  
  # Extract relevant portion
  source_subset <- regressionSource[start_index:end_index]
  
  # Calculate index values
  index_values <- 1:length(source_subset)
  
  # Calculate means
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  
  # Calculate numerator and denominator
  numerator <- sum((index_values - mean_index) *
                     (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  
  # Calculate slope and intercept
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  
  # Calculate predicted values
  predicted_values <- slope * index_values + intercept
  
  # Return results
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}
# Test Linear Regression
data <- c(10, 12, 15, 18, 20, 22, 25)

linreg_result <- linreg(
  data,
  regressionLength = 5,
  regressionOffset = 0
)

print(linreg_result)
