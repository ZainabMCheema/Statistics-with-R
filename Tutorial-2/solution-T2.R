# Generating data and calculating standard errors
sample_sizes <- c(10, 50, 250, 1250)
standard_errors <- numeric(length(sample_sizes))

for (i in seq_along(sample_sizes)) {
  data <- rnorm(sample_sizes[i], 0, 1)
  q <- quantile(data, c(0.25, 0.75))
  IQR <- q[2] - q[1]
  standard_errors[i] <- IQR / (1.349 * sqrt(length(data)))
}

# Creating a table
result_table <- data.frame(Sample_Size = sample_sizes, Standard_Error = standard_errors)
print(result_table)

# Creating a plot
plot(sample_sizes, standard_errors, type = "b", 
     xlab = "Sample Size (n)", ylab = "Standard Error",
     main = "Standard Error vs Sample Size")

df=read.csv('weirddata.csv')
head(df)

hist(df$x)
e=ecdf(df$x)
e
plot(e)

shapiro.test(df$x)
ad.test(df$x)
pearson.test(df$x)

calculate_QG <- function(data) {
  q1 <- quantile(data, 0.25) 
  q3 <- quantile(data, 0.75)  
  
  QG <- sqrt(q1 * q3)  
  
  return(QG)
}


calculate_QG(df$x)



num_bootstraps <- 100  
x_values=df$x
# Create an empty vector to store the bootstrap QG values
bootstrap_results <- numeric(num_bootstraps)

# Perform bootstrap resampling and calculate QG for each sample
for (i in 1:num_bootstraps) {
  # Resample with replacement from the 'x' column
  resampled_data <- sample(x_values, replace = TRUE)
  
  # Calculate QG for the resampled data
  QG_bootstrap <- calculate_QG(resampled_data)
  
  # Store the QG value in the results vector
  bootstrap_results[i] <- QG_bootstrap
}

# Calculate the confidence interval using quantiles
confidence_interval <- quantile(bootstrap_results, c(0.025, 0.975))

# Print the 95% confidence interval
print(confidence_interval)
