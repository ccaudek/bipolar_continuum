# Overview ----------------------------------------------------------------
# Associated project: Mindfulness paper on self-compassion EMA.
# Script purpose: Fit model on the relation between UCS and CS on the
#  filtered data of Study 2, by removing the careless responding trials
#  computed according to participant and occasion.
#
# Written by: Corrado Caudek (corrado.caudek@unifi.it)
# Version: Wed Jan  1 09:49:00 2025
# Last update: Mon Jan 13 09:33:33 2025
# Status: Final
# Notes:


# Prelims -----------------------------------------------------------------

if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  here, rio, tidyverse, cmdstanr, posterior, bayesplot, careless
)

# Set seed
set.seed(42)

theme_set(bayesplot::theme_default(base_size = 13, base_family = "sans"))
color_scheme_set("brightblue") # bayesplot

# Import filtered data. The measurements on occasions with outliers careless
# responding indices on two or more indices have been removed (less than 3% of
# the total).
filtered_data <- rio::import(
  here::here(
    "04_stan", "study_2", "without_occasion_cr.csv"
  )
)

unique(filtered_data$user_id) |> length()
# [1] 169

filtered_data <- filtered_data |>
  mutate(
    CS = scs_pos_1 + scs_pos_2 + scs_pos_3 + scs_pos_4,
    UCS = scs_neg_1 + scs_neg_2 + scs_neg_3 + scs_neg_4
  )

# Columns to standardize (excluding identifiers)
cols_to_standardize <- c(
  "CS", "UCS",
  "neg_aff_Moment", "neg_aff_Day", "neg_aff_Person",
  "context_Moment", "context_Day", "context_Person",
  "dec_Moment", "dec_Day", "dec_Person"
)

# Standardize within each user_id
standardized_data <- filtered_data %>%
  mutate(across(all_of(cols_to_standardize),
    ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
    .names = "{.col}"
  )) %>%
  ungroup()

# Prepare data for the Stan model
prepare_stan_data <- function(data) {
  # Ensure unique indices for participant, day, and measurement
  data <- data %>%
    mutate(
      participant = as.numeric(factor(user_id)), # Convert user_id to numeric indices
      day = as.numeric(factor(day)), # Ensure days are indexed sequentially
      measurement = time_window
    ) # Time window as measurement index

  # Get unique counts for participants, days, and measurements
  P <- length(unique(data$participant)) # Number of participants
  D <- max(data$day) # Number of days
  M <- max(data$measurement) # Measurements per day

  # Prepare the input data list for Stan
  stan_data <- list(
    N = nrow(data), # Total number of observations
    P = P, # Total participants
    D = D, # Total days
    M = M, # Measurements per day
    participant = data$participant, # Participant indices
    day = data$day, # Day indices
    measurement = data$measurement, # Measurement indices
    CS = data$CS, # CS variable
    UCS = data$UCS, # UCS variable
    neg_affect = data$neg_aff_Moment, # Moment-level negative affect
    context_eval = data$context_Moment, # Context evaluation at moment level
    dec_eval = data$dec_Moment # Decentering evaluation at moment level
  )

  return(stan_data)
}

# Generate the Stan data
stan_data <- prepare_stan_data(standardized_data)

# Verify the data structure
str(stan_data)

# Carica il modello Stan
model <- cmdstan_model(
  here::here("04_stan", "study_2", "model_1_mpath.stan")
)

# fit_vi <- model$variational(
#   data = stan_data,
#   seed = 123
# )

# Esegui il modello
if (!file.exists(
  here::here("04_stan", "study_2", "filtered_cr_study2_ucs_cs.qs")
)
) {
  fit_mcmc <- model$sample(
    data = stan_data,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000
  )
  qs::qsave(
    x = fit_mcmc,
    file = here::here("04_stan", "study_2", "filtered_cr_study2_ucs_cs.qs")
  )
} else {
  fit_mcmc <- qs::qread(
    here::here("04_stan", "study_1", "filtered_cr_study1_ucs_cs.qs")
  )
}

# Extract draws from the fit
posterior_draws <- fit_mcmc$draws()

# Summarize using posterior package with custom probabilities
posterior_summary <- summarise_draws(
  posterior_draws,
  ~ mean(.x),
  ~ sd(.x),
  ~ quantile2(.x, probs = c(0.055, 0.945)) # 89% CI
)
posterior_summary[2:7, ]
# variable           `~mean(.x)` `~sd(.x)`       q5.5     q94.5
# 1 lp_approx__          -185.      13.6     -207.      -164.
# 2 alpha_ucs               0.0880   0.00481    0.0803     0.0956
# 3 beta_cs                -0.419    0.00672   -0.429     -0.408
# 4 beta_covariates[1]      0.0278   0.00672    0.0170     0.0388
# 5 beta_covariates[2]      0.0177   0.00765    0.00548    0.0306
# 6 beta_covariates[3]     -0.0947   0.00736   -0.107     -0.0833

# Filter the posterior draws for parameters of interest
parameters_of_interest <-
  c(
    "beta_cs", "beta_covariates[1]", "beta_covariates[2]",
    "beta_covariates[3]"
  )

# Extract posterior draws
posterior_filtered <-
  fit_vi$draws(variables = parameters_of_interest, format = "df")

# Plot posterior distributions with 89% credible intervals
mcmc_areas(
  posterior_filtered,
  pars = parameters_of_interest,
  prob = 0.89 # Specify 89% credible interval
) +
  ggtitle("Posterior Distributions of Model's Parameters")


# eof ---
