// Nomothetic analysis
//
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
  real alpha_cs; // Intercept for CS
  real beta_neg_aff_Moment; // Overall effect of moment-centered negative affect on UCS
  real beta_neg_aff_Day; // Overall effect of day-centered negative affect on UCS
  real beta_neg_aff_Person; // Overall effect of person-centered negative affect on UCS
  real beta_cs_context; // Effect of context evaluation on CS
  real beta_ucs_context; // Effect of context evaluation on UCS

  // Random intercepts
  vector[P] z_participant; // Random intercept for participants
  vector[D] z_day; // Random intercept for days
  vector[M] z_measurement; // Random intercept for measurements

  // Random slopes for negative affect at the participant level
  vector[P] z_participant_slope_na;

  // Variance parameters
  real<lower=0> sigma_participant; // SD of participant intercepts
  real<lower=0> sigma_day; // SD of day intercepts
  real<lower=0> sigma_measurement; // SD of measurement intercepts
  real<lower=0> sigma_participant_slope_na; // SD of participant slopes for NA
  real<lower=0> sigma_ucs; // Error term for UCS model
  real<lower=0> sigma_cs; // Error term for CS model

  real<lower=0> nu; // Degrees of freedom for t-distribution
}

model {
  // Priors for fixed effects
  alpha_ucs ~ normal(0, 1);
  alpha_cs ~ normal(0, 1);
  beta_neg_aff_Moment ~ normal(0, 1);
  beta_neg_aff_Day ~ normal(0, 1);
  beta_neg_aff_Person ~ normal(0, 1);
  beta_cs_context ~ normal(0, 1);
  beta_ucs_context ~ normal(0, 1);

  // Priors for random effects
  z_participant ~ normal(0, 1);
  z_day ~ normal(0, 1);
  z_measurement ~ normal(0, 1);
  z_participant_slope_na ~ normal(0, 1);

  // Priors for variances
  sigma_participant ~ exponential(1);
  sigma_day ~ exponential(1);
  sigma_measurement ~ exponential(1);
  sigma_participant_slope_na ~ exponential(1);
  sigma_ucs ~ exponential(1);
  sigma_cs ~ exponential(1);
  nu ~ gamma(2, 0.1);

  // Likelihood for UCS using a t-distribution
  for (n in 1:N) {
    UCS[n] ~ student_t(
      nu,
      alpha_ucs +
      sigma_participant * z_participant[participant[n]] +
      sigma_day * z_day[day[n]] +
      sigma_measurement * z_measurement[measurement[n]] +
      
      // Main effect of negative affect
      (beta_neg_aff_Moment + sigma_participant_slope_na * z_participant_slope_na[participant[n]]) * neg_aff_Moment[n] +
      beta_neg_aff_Day * neg_aff_Day[n] +
      beta_neg_aff_Person * neg_aff_Person[n] +
      
      // Effect of context evaluation
      beta_ucs_context * context_eval_Moment[n] +
      beta_ucs_context * context_eval_Day[n] +
      beta_ucs_context * context_eval_Person[n],
      
      sigma_ucs
    );
  }

  // Likelihood for CS using a t-distribution
  for (n in 1:N) {
    CS[n] ~ student_t(
      nu,
      alpha_cs +
      sigma_participant * z_participant[participant[n]] +
      sigma_day * z_day[day[n]] +
      sigma_measurement * z_measurement[measurement[n]] +
      
      // Effect of context evaluation (opposite effect to UCS)
      beta_cs_context * context_eval_Moment[n] +
      beta_cs_context * context_eval_Day[n] +
      beta_cs_context * context_eval_Person[n],
      
      sigma_cs
    );
  }
}

generated quantities {
  array[N] real pred_UCS; // Posterior predictions for UCS
  array[N] real pred_CS; // Posterior predictions for CS
  array[N] real log_lik_ucs; // Log-likelihood for UCS
  array[N] real log_lik_cs; // Log-likelihood for CS

  for (n in 1:N) {
    pred_UCS[n] = student_t_rng(
      nu,
      alpha_ucs +
      sigma_participant * z_participant[participant[n]] +
      sigma_day * z_day[day[n]] +
      sigma_measurement * z_measurement[measurement[n]] +
      (beta_neg_aff_Moment + sigma_participant_slope_na * z_participant_slope_na[participant[n]]) * neg_aff_Moment[n] +
      beta_neg_aff_Day * neg_aff_Day[n] +
      beta_neg_aff_Person * neg_aff_Person[n] +
      beta_ucs_context * context_eval_Moment[n] +
      beta_ucs_context * context_eval_Day[n] +
      beta_ucs_context * context_eval_Person[n],
      sigma_ucs
    );

    pred_CS[n] = student_t_rng(
      nu,
      alpha_cs +
      sigma_participant * z_participant[participant[n]] +
      sigma_day * z_day[day[n]] +
      sigma_measurement * z_measurement[measurement[n]] +
      beta_cs_context * context_eval_Moment[n] +
      beta_cs_context * context_eval_Day[n] +
      beta_cs_context * context_eval_Person[n],
      sigma_cs
    );

    log_lik_ucs[n] = student_t_lpdf(
      UCS[n] |
      nu,
      alpha_ucs +
      sigma_participant * z_participant[participant[n]] +
      sigma_day * z_day[day[n]] +
      sigma_measurement * z_measurement[measurement[n]] +
      (beta_neg_aff_Moment + sigma_participant_slope_na * z_participant_slope_na[participant[n]]) * neg_aff_Moment[n] +
      beta_neg_aff_Day * neg_aff_Day[n] +
      beta_neg_aff_Person * neg_aff_Person[n] +
      beta_ucs_context * context_eval_Moment[n] +
      beta_ucs_context * context_eval_Day[n] +
      beta_ucs_context * context_eval_Person[n],
      sigma_ucs
    );

    log_lik_cs[n] = student_t_lpdf(
      CS[n] |
      nu,
      alpha_cs +
      sigma_participant * z_participant[participant[n]] +
      sigma_day * z_day[day[n]] +
      sigma_measurement * z_measurement[measurement[n]] +
      beta_cs_context * context_eval_Moment[n] +
      beta_cs_context * context_eval_Day[n] +
      beta_cs_context * context_eval_Person[n],
      sigma_cs
    );
  }
}
