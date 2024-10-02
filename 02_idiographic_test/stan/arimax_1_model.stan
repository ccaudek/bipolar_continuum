data {
  int<lower=1> N;   // Number of observations
  int<lower=1> K;   // Number of predictors
  matrix[N, K] X;   // Predictor matrix
  vector[N] y;      // Response variable
}

parameters {
  vector[K] beta;   // Coefficients for predictors
  real<lower=0> sigma; // Standard deviation of residuals
}

model {
  y ~ normal(X * beta, sigma);  // Simple linear regression model
}

generated quantities {
  vector[N] yrep;
  for (n in 1:N)
    yrep[n] = normal_rng(X[n] * beta, sigma);  // Predicted values
}
