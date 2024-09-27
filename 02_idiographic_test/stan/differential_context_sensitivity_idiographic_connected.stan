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
  // Fixed effects for CS and UCS
  real alpha_cs; // Intercept for CS
  real alpha_ucs; // Intercept for UCS

  // Shared participant-level intercept for CS and UCS
  vector[P] z_participant_shared; // Shared random intercept (per participant)

  // Coefficients for negative affect (for both CS and UCS)
  real beta_neg_aff_Moment_cs;
  real beta_neg_aff_Day_cs;
  real beta_neg_aff_Person_cs;
  real beta_neg_aff_Moment_ucs;
  real beta_neg_aff_Day_ucs;
  real beta_neg_aff_Person_ucs;

  // Coefficients for context evaluation (for both CS and UCS)
  real beta_context_eval_Moment_cs;
  real beta_context_eval_Day_cs;
  real beta_context_eval_Person_cs;
  real beta_context_eval_Moment_ucs;
  real beta_context_eval_Day_ucs;
  real beta_context_eval_Person_ucs;

  // Random intercepts for days and measurements for CS and UCS
  vector[D] z_day_cs;  // Random intercept for days for CS
  vector[D] z_day_ucs; // Random intercept for days for UCS
  vector[M] z_measurement_cs;  // Random intercept for measurements for CS
  vector[M] z_measurement_ucs; // Random intercept for measurements for UCS

  // Variance parameters
  real<lower=0> sigma_participant_shared;
  real<lower=0> sigma_day_cs; 
  real<lower=0> sigma_day_ucs; 
  real<lower=0> sigma_measurement_cs; 
  real<lower=0> sigma_measurement_ucs; 
  real<lower=0> sigma_cs; // Error term for CS model
  real<lower=0> sigma_ucs; // Error term for UCS model
}

model {
  // Priors for fixed effects
  alpha_cs ~ normal(0, 1);
  alpha_ucs ~ normal(0, 1);

  // Priors for shared participant-level intercept
  z_participant_shared ~ normal(0, 1);
  
  // Priors for negative affect coefficients
  beta_neg_aff_Moment_cs ~ normal(0, 1);
  beta_neg_aff_Day_cs ~ normal(0, 1);
  beta_neg_aff_Person_cs ~ normal(0, 1);
  beta_neg_aff_Moment_ucs ~ normal(0, 1);
  beta_neg_aff_Day_ucs ~ normal(0, 1);
  beta_neg_aff_Person_ucs ~ normal(0, 1);

  // Priors for context evaluation coefficients
  beta_context_eval_Moment_cs ~ normal(0, 1);
  beta_context_eval_Day_cs ~ normal(0, 1);
  beta_context_eval_Person_cs ~ normal(0, 1);
  beta_context_eval_Moment_ucs ~ normal(0, 1);
  beta_context_eval_Day_ucs ~ normal(0, 1);
  beta_context_eval_Person_ucs ~ normal(0, 1);

  // Priors for random intercepts
  z_day_cs ~ normal(0, 1);
  z_day_ucs ~ normal(0, 1);
  z_measurement_cs ~ normal(0, 1);
  z_measurement_ucs ~ normal(0, 1);

  // Priors for variance parameters
  sigma_participant_shared ~ exponential(1);
  sigma_day_cs ~ exponential(1);
  sigma_day_ucs ~ exponential(1);
  sigma_measurement_cs ~ exponential(1);
  sigma_measurement_ucs ~ exponential(1);
  sigma_cs ~ exponential(1);
  sigma_ucs ~ exponential(1);

  // Likelihood for CS
  for (n in 1:N) {
    CS[n] ~ normal(
      alpha_cs +
      sigma_participant_shared * z_participant_shared[participant[n]] +
      sigma_day_cs * z_day_cs[day[n]] +
      sigma_measurement_cs * z_measurement_cs[measurement[n]] +
      beta_neg_aff_Moment_cs * neg_aff_Moment[n] +
      beta_neg_aff_Day_cs * neg_aff_Day[n] +
      beta_neg_aff_Person_cs * neg_aff_Person[n] +
      beta_context_eval_Moment_cs * context_eval_Moment[n] +
      beta_context_eval_Day_cs * context_eval_Day[n] +
      beta_context_eval_Person_cs * context_eval_Person[n],
      sigma_cs
    );
  }

  // Likelihood for UCS
  for (n in 1:N) {
    UCS[n] ~ normal(
      alpha_ucs +
      sigma_participant_shared * z_participant_shared[participant[n]] +
      sigma_day_ucs * z_day_ucs[day[n]] +
      sigma_measurement_ucs * z_measurement_ucs[measurement[n]] +
      beta_neg_aff_Moment_ucs * neg_aff_Moment[n] +
      beta_neg_aff_Day_ucs * neg_aff_Day[n] +
      beta_neg_aff_Person_ucs * neg_aff_Person[n] +
      beta_context_eval_Moment_ucs * context_eval_Moment[n] +
      beta_context_eval_Day_ucs * context_eval_Day[n] +
      beta_context_eval_Person_ucs * context_eval_Person[n],
      sigma_ucs
    );
  }
}

generated quantities {
  array[N] real log_lik_cs; // Log-likelihood for CS
  array[N] real log_lik_ucs; // Log-likelihood for UCS

  for (n in 1:N) {
    log_lik_cs[n] = normal_lpdf(
      CS[n] |
      alpha_cs +
      sigma_participant_shared * z_participant_shared[participant[n]] +
      sigma_day_cs * z_day_cs[day[n]] +
      sigma_measurement_cs * z_measurement_cs[measurement[n]] +
      beta_neg_aff_Moment_cs * neg_aff_Moment[n] +
      beta_neg_aff_Day_cs * neg_aff_Day[n] +
      beta_neg_aff_Person_cs * neg_aff_Person[n] +
      beta_context_eval_Moment_cs * context_eval_Moment[n] +
      beta_context_eval_Day_cs * context_eval_Day[n] +
      beta_context_eval_Person_cs * context_eval_Person[n],
      sigma_cs
    );

    log_lik_ucs[n] = normal_lpdf(
      UCS[n] |
      alpha_ucs +
      sigma_participant_shared * z_participant_shared[participant[n]] +
      sigma_day_ucs * z_day_ucs[day[n]] +
      sigma_measurement_ucs * z_measurement_ucs[measurement[n]] +
      beta_neg_aff_Moment_ucs * neg_aff_Moment[n] +
      beta_neg_aff_Day_ucs * neg_aff_Day[n] +
      beta_neg_aff_Person_ucs * neg_aff_Person[n] +
      beta_context_eval_Moment_ucs * context_eval_Moment[n] +
      beta_context_eval_Day_ucs * context_eval_Day[n] +
      beta_context_eval_Person_ucs * context_eval_Person[n],
      sigma_ucs
    );
  }
}
