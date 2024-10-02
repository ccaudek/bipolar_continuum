data {
  int<lower=1> N;           // Number of observations
  int<lower=1> K;           // Number of exogenous predictors
  matrix[N, K] X;           // Matrix of predictors (exogenous variables)
  vector[N] y;              // Response variable (UCS)
  vector[N] lag_y;          // Lagged response variable (lagged UCS)
}

parameters {
  real alpha_raw;                  // Uncentered intercept
  real<lower=-1, upper=1> phi_raw; // Uncentered AR(1) coefficient
  vector[K] beta_raw;              // Uncentered regression coefficients
  real<lower=0> sigma;             // Standard deviation (positive)
}

transformed parameters {
  real alpha = 0 + 1 * alpha_raw;      // Re-centered intercept
  real phi = 0 + 0.5 * phi_raw;        // Re-centered AR(1) coefficient
  vector[K] beta = 0 + 1 * beta_raw;   // Re-centered regression coefficients
}

model {
  // Priors for the raw parameters (non-centered)
  alpha_raw ~ normal(0, 1);           // Prior for intercept
  phi_raw ~ normal(0, 0.5);           // Prior for AR(1) coefficient
  beta_raw ~ normal(0, 1);            // Prior for regression coefficients
  sigma ~ cauchy(0, 2.5);             // Prior for standard deviation
  
  // Likelihood
  y ~ normal(alpha + X * beta + phi * lag_y, sigma);
}

generated quantities {
  // Predicted values (useful for posterior predictive checks)
  vector[N] y_pred;
  for (n in 1:N) {
    y_pred[n] = normal_rng(alpha + X[n] * beta + phi * lag_y[n], sigma);
  }
}
