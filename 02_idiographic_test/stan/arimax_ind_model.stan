data {
  int<lower=1> N;       // Number of observations
  vector[N] CS;         // Predictor (CS)
  vector[N] UCS;        // Response (UCS)
}

parameters {
  real alpha;           // Intercept
  real gamma;           // Slope for CS
  real<lower=0> sigma;  // Standard deviation
}

model {
  // Prior
  gamma ~ normal(-1, 1);  // Prior reflecting the expected negative relationship
  alpha ~ normal(0, 1);
  sigma ~ cauchy(0, 2.5);

  // Likelihood
  UCS ~ normal(alpha + gamma * CS, sigma);
}
