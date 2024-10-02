data {
  int<lower=1> N;                // Number of observations
  vector[N] CS;                  // Predictor (CS)
  vector[N] UCS;                 // Response (UCS)
  vector[N] neg_aff_Moment;       // Covariate 1 (negative affect)
  vector[N] context_Moment;       // Covariate 2 (context evaluation)
  vector[N] lag_CS_same_day;      // Lagged CS within the same day
}

parameters {
  real alpha_raw;                 // Raw intercept (non-centered)
  real gamma_CS_raw;              // Raw slope for CS
  real gamma_neg_aff_raw;         // Raw slope for neg_aff_Moment
  real gamma_context_raw;         // Raw slope for context_Moment
  real gamma_interaction_raw;     // Raw slope for the interaction term between CS and neg_aff_Moment
  real phi_raw;                   // Raw AR(1) coefficient
  real<lower=0> sigma;            // Standard deviation
  real<lower=2> nu;               // Degrees of freedom for Student-t distribution
}

transformed parameters {
  real alpha = 0 + 1 * alpha_raw;  // Centered intercept
  real gamma_CS = 0 + 1 * gamma_CS_raw;  // Centered slope for CS
  real gamma_neg_aff = 0 + 1 * gamma_neg_aff_raw;  // Centered slope for neg_aff_Moment
  real gamma_context = 0 + 1 * gamma_context_raw;  // Centered slope for context_Moment
  real gamma_interaction = 0 + 1 * gamma_interaction_raw;  // Centered slope for the interaction term
  real phi = 0.5 * phi_raw;       // Centered AR(1) coefficient
}

model {
  // Priors
  alpha_raw ~ normal(0, 1);
  gamma_CS_raw ~ normal(0, 1);   // Prior for CS
  gamma_neg_aff_raw ~ normal(0, 1);  // Prior for neg_aff_Moment
  gamma_context_raw ~ normal(0, 1);  // Prior for context_Moment
  gamma_interaction_raw ~ normal(0, 1);  // Prior for the interaction term
  phi_raw ~ normal(0, 1);         // AR(1) coefficient
  sigma ~ cauchy(0, 2.5);         // Prior for standard deviation
  nu ~ gamma(2, 0.1);             // Prior for degrees of freedom

  // Likelihood with Student-t distribution
  UCS ~ student_t(nu, 
                  alpha + 
                  gamma_CS * CS + 
                  gamma_neg_aff * neg_aff_Moment + 
                  gamma_context * context_Moment + 
                  gamma_interaction * CS .* neg_aff_Moment +  // Interaction term
                  phi * lag_CS_same_day, 
                  sigma);
}

generated quantities {
  vector[N] y_pred;
  vector[N] log_lik;  // Log-likelihood for each observation
  for (n in 1:N) {
    // Prediction for UCS
    y_pred[n] = student_t_rng(nu, alpha + 
                              gamma_CS * CS[n] + 
                              gamma_neg_aff * neg_aff_Moment[n] + 
                              gamma_context * context_Moment[n] + 
                              gamma_interaction * CS[n] * neg_aff_Moment[n] + 
                              phi * lag_CS_same_day[n], 
                              sigma);
    // Log-likelihood for each observation
    log_lik[n] = student_t_lpdf(UCS[n] | nu, alpha + 
                                gamma_CS * CS[n] + 
                                gamma_neg_aff * neg_aff_Moment[n] + 
                                gamma_context * context_Moment[n] + 
                                gamma_interaction * CS[n] * neg_aff_Moment[n] + 
                                phi * lag_CS_same_day[n], 
                                sigma);
  }
}
