data {
  int<lower=0> N; // Total number of observations
  int<lower=0> P; // Number of participants
  int<lower=0> D; // Number of days
  int<lower=0> M; // Number of measurements per day per participant
  array[N] int<lower=1, upper=P> participant; // Participant index for each observation
  array[N] int<lower=1, upper=D> day; // Day index for each observation
  array[N] int<lower=1, upper=M> measurement; // Measurement index for each observation
  array[N] real CS; // Compassionate Self measures
  array[N] real UCS; // Uncompassionate Self measures

  // Scaled and centered negative affect variables
  array[N] real neg_aff_Moment; // Negative affect moment-centered
  array[N] real neg_aff_Day; // Negative affect day-centered
  array[N] real neg_aff_Person; // Negative affect person-centered

  // Scaled and centered context evaluation variables
  array[N] real context_eval_Moment; // Context evaluation moment-centered
  array[N] real context_eval_Day; // Context evaluation day-centered
  array[N] real context_eval_Person; // Context evaluation person-centered
}

parameters {
  // Fixed effects
  real alpha_ucs; // Intercept for UCS
  real beta_cs; // Overall effect of CS on UCS
  real beta_interaction; // Effect of interaction between CS and NA

  // Coefficients for the three negative affect components
  real beta_neg_aff_Moment;
  real beta_neg_aff_Day;
  real beta_neg_aff_Person;

  // Coefficients for the three context evaluation components
  real beta_context_eval_Moment;
  real beta_context_eval_Day;
  real beta_context_eval_Person;

  // Random intercepts
  vector[P] z_participant; // Random intercept for participants
  vector[D] z_day; // Random intercept for days
  vector[M] z_measurement; // Random intercept for measurements

  // Random slopes for CS at the participant level
  vector[P] z_participant_slope_cs;

  // Random slopes for NA at the participant level
  vector[P] z_participant_slope_na;

  // Variance parameters
  real<lower=0> sigma_participant; // SD of participant intercepts
  real<lower=0> sigma_day; // SD of day intercepts
  real<lower=0> sigma_measurement; // SD of measurement intercepts
  real<lower=0> sigma_participant_slope_cs; // SD of participant slopes for CS
  real<lower=0> sigma_participant_slope_na; // SD of participant slopes for NA
  real<lower=0> sigma_ucs; // Error term for UCS model

  real<lower=0> nu; // Degrees of freedom for t-distribution
}

model {
  // Priors for fixed effects
  alpha_ucs ~ normal(0, 1);
  beta_cs ~ normal(0, 1);
  beta_interaction ~ normal(0, 1); // Prior for interaction term
  beta_neg_aff_Moment ~ normal(0, 1);
  beta_neg_aff_Day ~ normal(0, 1);
  beta_neg_aff_Person ~ normal(0, 1);
  beta_context_eval_Moment ~ normal(0, 1);
  beta_context_eval_Day ~ normal(0, 1);
  beta_context_eval_Person ~ normal(0, 1);

  // Priors for random effects (latent variables)
  z_participant ~ normal(0, 1);
  z_day ~ normal(0, 1);
  z_measurement ~ normal(0, 1);
  z_participant_slope_cs ~ normal(0, 1);
  z_participant_slope_na ~ normal(0, 1);

  // Priors for variances
  sigma_participant ~ exponential(1);
  sigma_day ~ exponential(1);
  sigma_measurement ~ exponential(1);
  sigma_participant_slope_cs ~ exponential(1);
  sigma_participant_slope_na ~ exponential(1);
  sigma_ucs ~ exponential(1);
  nu ~ gamma(2, 0.1);

  // Likelihood for UCS using a t-distribution
  for (n in 1:N) {
    UCS[n] ~ student_t(
      nu,
      // Random intercepts
      alpha_ucs +
      sigma_participant * z_participant[participant[n]] +
      sigma_day * z_day[day[n]] +
      sigma_measurement * z_measurement[measurement[n]] +
      
      // Main effect of CS with individual random slope
      (beta_cs + sigma_participant_slope_cs * z_participant_slope_cs[participant[n]]) * CS[n] +
      
      // Main effect of NA (including random slope for NA)
      (beta_neg_aff_Moment + sigma_participant_slope_na * z_participant_slope_na[participant[n]]) * neg_aff_Moment[n] +
      beta_neg_aff_Day * neg_aff_Day[n] +
      beta_neg_aff_Person * neg_aff_Person[n] +
      
      // Interaction term between CS and NA (Moment-level)
      beta_interaction * CS[n] * neg_aff_Moment[n] +
      
      // Main effect of context evaluation components
      beta_context_eval_Moment * context_eval_Moment[n] +
      beta_context_eval_Day * context_eval_Day[n] +
      beta_context_eval_Person * context_eval_Person[n],
      
      // Error term
      sigma_ucs
    );
  }
}

generated quantities {
  array[N] real pred_UCS; // Posterior predictions for UCS
  array[N] real log_lik; // Log-likelihood for UCS

  for (n in 1:N) {
    pred_UCS[n] = student_t_rng(
      nu,
      alpha_ucs +
      sigma_participant * z_participant[participant[n]] +
      sigma_day * z_day[day[n]] +
      sigma_measurement * z_measurement[measurement[n]] +
      (beta_cs + sigma_participant_slope_cs * z_participant_slope_cs[participant[n]]) * CS[n] +
      (beta_neg_aff_Moment + sigma_participant_slope_na * z_participant_slope_na[participant[n]]) * neg_aff_Moment[n] +
      beta_neg_aff_Day * neg_aff_Day[n] +
      beta_neg_aff_Person * neg_aff_Person[n] +
      beta_interaction * CS[n] * neg_aff_Moment[n] +
      beta_context_eval_Moment * context_eval_Moment[n] +
      beta_context_eval_Day * context_eval_Day[n] +
      beta_context_eval_Person * context_eval_Person[n],
      sigma_ucs
    );

    log_lik[n] = student_t_lpdf(
      UCS[n] |
      nu,
      alpha_ucs +
      sigma_participant * z_participant[participant[n]] +
      sigma_day * z_day[day[n]] +
      sigma_measurement * z_measurement[measurement[n]] +
      (beta_cs + sigma_participant_slope_cs * z_participant_slope_cs[participant[n]]) * CS[n] +
      (beta_neg_aff_Moment + sigma_participant_slope_na * z_participant_slope_na[participant[n]]) * neg_aff_Moment[n] +
      beta_neg_aff_Day * neg_aff_Day[n] +
      beta_neg_aff_Person * neg_aff_Person[n] +
      beta_interaction * CS[n] * neg_aff_Moment[n] +
      beta_context_eval_Moment * context_eval_Moment[n] +
      beta_context_eval_Day * context_eval_Day[n] +
      beta_context_eval_Person * context_eval_Person[n],
      sigma_ucs
    );
  }
}
