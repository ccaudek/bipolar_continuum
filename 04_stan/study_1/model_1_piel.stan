data {
  int<lower=0> N; // Total observations
  int<lower=0> P; // Participants
  int<lower=0> D; // Days
  int<lower=0> M; // Measurements per day
  array[N] int<lower=1, upper=P> participant;
  array[N] int<lower=1, upper=D> day;
  array[N] int<lower=1, upper=M> measurement;
  array[N] real CS;
  array[N] real UCS;
  array[N] real neg_affect;
  array[N] real context_eval;
}
parameters {
  real alpha_ucs;
  real beta_cs;
  array[2] real beta_covariates;
  vector[P] z_participant;
  vector[D] z_day;
  vector[M] z_measurement;
  vector[P] z_participant_slope_cs;
  real<lower=0> sigma_participant;
  real<lower=0> sigma_day;
  real<lower=0> sigma_measurement;
  real<lower=0> sigma_participant_slope_cs;
  real<lower=0> sigma_ucs;
  real<lower=0> nu;
}
model {
  alpha_ucs ~ normal(0, 1);
  beta_cs ~ normal(0, 1);
  beta_covariates ~ normal(0, 1);
  z_participant ~ normal(0, 1);
  z_day ~ normal(0, 1);
  z_measurement ~ normal(0, 1);
  z_participant_slope_cs ~ normal(0, 1);
  sigma_participant ~ exponential(1);
  sigma_day ~ exponential(1);
  sigma_measurement ~ exponential(1);
  sigma_participant_slope_cs ~ exponential(1);
  sigma_ucs ~ exponential(1);
  nu ~ gamma(2, 0.1);
  
  for (n in 1 : N) {
    UCS[n] ~ student_t(nu,
                       alpha_ucs
                       + (beta_cs
                          + sigma_participant_slope_cs
                            * z_participant_slope_cs[participant[n]])
                         * CS[n]
                       + beta_covariates[1] * neg_affect[n]
                       + beta_covariates[2] * context_eval[n]
                       + sigma_participant * z_participant[participant[n]]
                       + sigma_day * z_day[day[n]]
                       + sigma_measurement * z_measurement[measurement[n]],
                       sigma_ucs);
  }
}
