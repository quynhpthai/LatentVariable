data <- read.csv('linear_model_W_data.csv')



first3 <- data[, 1:3]

# Step 3: Get the number of rows (n) and create arrays to store the estimators for each row
n <- nrow(data_J3)  # Number of rows (observations)
alpha_hat <- numeric(n)  # To store alpha estimates for each row
beta_hat <- numeric(n)   # To store beta estimates for each row
tau_hat <- numeric(n)    # To store tau estimates for each row

# Step 4: Loop through each row and calculate the estimators
for (i in 1:n) {
  
  # For each row, extract the values of W1, W2, and W3
  W1 <- first3[i, "W1"]
  W2 <- first3[i, "W2"]
  W3 <- first3[i, "W3"]
  
  # MOM estimator for alpha (mean of W1, W2, W3 in the row)
  alpha_hat[i] <- mean(c(W1, W2, W3))  # Mean for the i-th row
  
  # Calculate the variances (for estimating tau) and covariances (for estimating beta)
  var_W1 <- var(first3$W1)
  var_W2 <- var(first3$W2)
  var_W3 <- var(first3$W3)
  
  cov_W1_W2 <- cov(first3$W1, first3$W2)
  cov_W1_W3 <- cov(first3$W1, first3$W3)
  cov_W2_W3 <- cov(first3$W2, first3$W3)
  
  # MOM estimators for beta
  beta_hat[1] <- sqrt((cov_W1_W2 * cov_W1_W3) / cov_W2_W3)
  beta_hat[2] <- sqrt((cov_W1_W2 * cov_W2_W3) / cov_W1_W3)
  beta_hat[3] <- sqrt((cov_W1_W3 * cov_W2_W3) / cov_W1_W2)
  
  # MOM estimator for tau
  tau_hat[i] <- sqrt(mean(c(var_W1, var_W2, var_W3)) - mean(beta_hat^2))  # Calculate tau for each row
}

cat("Estimated alpha values for each row:\n", alpha_hat[1:3], "\n")
cat("Estimated beta values for each row:\n", beta_hat[1:3], "\n")
cat("Estimated tau values for each row:\n", tau_hat[1:3], "\n")

