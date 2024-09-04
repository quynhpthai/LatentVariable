# Install the rstan package (if not installed)
# install.packages("rstan")

# Load the rstan library
library(rstan)
library(gdata)
library(bayesplot)

# Step 1: Read the data
data <- read.csv('linear_model_W_data.csv')

# Step 2: Define the data for Stan (Use only the first three columns)
data_J3 <- data[, 1:3]

stan_data <- list(
  n = nrow(data_J3),  # Number of rows in the dataset
  J = 3,              # Number of variables (J = 3 for W1, W2, W3)
  W = as.matrix(data_J3),  # Convert the data frame to a matrix
  sigma_alpha = 1,     # Hyperparameter for alpha_j
  sigma_beta = 1,      # Hyperparameter for beta_j
  sigma_tau = 1        # Hyperparameter for tau_j
)

# Step 3: Define the Stan model (paste the Stan code from above)
stan_code <- "
data {
  int<lower=1> n;  
  int<lower=1> J;  
  matrix[n, J] W;  
  real<lower=0> sigma_alpha;  
  real<lower=0> sigma_beta;   
  real<lower=0> sigma_tau;    
}

parameters {
  vector[J] alpha;            
  vector[J] beta;             
  real<lower=0> tau[J];       
  vector[n] theta;            
}

model {
  alpha ~ normal(0, sigma_alpha);  
  beta ~ normal(0, sigma_beta);    
  tau ~ normal(0, sigma_tau);      

  for (i in 1:n) {
    W[i] ~ normal(alpha + beta * theta[i], tau);  
  }
}"

# Step 4: Compile and fit the Stan model
fit <- stan(model_code = stan_code, data = stan_data, iter = 2000, chains = 4)

# Step 5: Print the summary of the posterior distributions
print(summary(fit)$summary)

# Extract posterior samples
posterior_samples <- extract(fit)

# Print the posterior means and credible intervals for alpha, beta, and tau
alpha_post <- posterior_samples$alpha
beta_post <- posterior_samples$beta
tau_post <- posterior_samples$tau

# Print results
cat("Posterior mean for alpha_j:\n", apply(alpha_post, 2, mean), "\n")
cat("Posterior mean for beta_j:\n", apply(beta_post, 2, mean), "\n")
cat("Posterior mean for tau_j:\n", apply(tau_post, 2, mean), "\n")
