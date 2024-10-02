data {
  int<lower=1> N;            // Number of observations
  vector[N] CS;              // Predictor (CS)
  vector[N] UCS;             // Response (UCS)
  vector[N] neg_aff_Moment;  // Covariate 1 (neg_aff_Moment)
  vector[N] context_Moment;  // Covariate 2 (context_Moment)
}

parameters {
  real alpha;                // Intercept
  real gamma_CS;             // Slope for CS
  real gamma_neg_aff;        // Slope for neg_aff_Moment
  real gamma_context;        // Slope for context_Moment
  real<lower=0> sigma;       // Standard deviation
}

model {
  // Priors for parameters
  gamma_CS ~ normal(0, 1);  // Prior reflecting the expected negative relationship between UCS and CS
  gamma_neg_aff ~ normal(0, 1);  // Prior for neg_aff_Moment
  gamma_context ~ normal(0, 1);  // Prior for context_Moment
  alpha ~ normal(0, 1);
  sigma ~ cauchy(0, 2.5);

  // Likelihood
  UCS ~ normal(alpha + gamma_CS * CS + gamma_neg_aff * neg_aff_Moment + gamma_context * context_Moment, sigma);
}

generated quantities {
  vector[N] UCS_pred;
  for (n in 1:N) {
    UCS_pred[n] = normal_rng(alpha + gamma_CS * CS[n] + gamma_neg_aff * neg_aff_Moment[n] + gamma_context * context_Moment[n], sigma);
  }
}
