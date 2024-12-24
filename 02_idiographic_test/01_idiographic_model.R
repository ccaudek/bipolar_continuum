# Run Stan script for testing the idiographic test of Neff's bipolar hypothesis. 


# Setup -------------------------------------------------------------------

# Load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, rio, tidyverse, cmdstanr, posterior, bayesplot)

# Set seed
set.seed(42)

# Load helper functions
source(here::here("R", "importing_cleaning_data.R"))


# Include Negative Affect -------------------------------------------------

study_1_df <- rio::import(here::here("data", "study_1_data.csv")) |>
  dplyr::select(
    "user_id", "day", "time_window", "scs_pos_1",
    "scs_neg_2", "scs_pos_3", "scs_neg_4", "scs_neg_5",
    "scs_pos_6", "scs_pos_7", "scs_neg_8",
    "neg_aff_Moment", "neg_aff_Day", "neg_aff_Person",
    "context_Moment", "context_Day", "context_Person"
  )


study_2_temp <- rio::import(here::here("data", "study_2_data.csv"))

x_scaled <- function(x) {
  (x - min(x)) / (max(x) - min(x)) * (5 - 1) + 1
} 

study_2_temp$neg_aff_raw <- study_2_temp$neg_aff
study_2_temp$neg_aff <- x_scaled(study_2_temp$neg_aff_raw)
study_2_temp$neg_aff_raw <- NULL

# Add negative affect scaled by occasion, day, person
study_2_temp <- center3L(
  dataname = study_2_temp,
  varname = neg_aff, 
  idname = user_id, 
  dayname = day
)

study_2_temp <- center3L(
  dataname = study_2_temp,
  varname = context, 
  idname = user_id, 
  dayname = day
)

study_2_df <- study_2_temp |>
  dplyr::select(
    "user_id", "day", "time_window", "scs_pos_1",
    "scs_neg_2", "scs_pos_3", "scs_neg_4", "scs_neg_5",
    "scs_pos_6", "scs_pos_7", "scs_neg_8",
    "neg_aff_Moment", "neg_aff_Day", "neg_aff_Person",
    "context_Moment", "context_Day", "context_Person"
  )

# Combine the two studies
df <- bind_rows(study_1_df, study_2_df)
length(unique(df$user_id))
# [1] 495

# Scale negative affect and context
df$neg_aff_Moment <- scale(df$neg_aff_Moment) |> as.numeric()
df$neg_aff_Day <- scale(df$neg_aff_Day) |> as.numeric()
df$neg_aff_Person <- scale(df$neg_aff_Person) |> as.numeric()
df$context_Moment <- scale(df$context_Moment) |> as.numeric()
df$context_Day <- scale(df$context_Day) |> as.numeric()
df$context_Person <- scale(df$context_Person) |> as.numeric()

# scale UCS in the opposite direction. I don't need this.
# Identify the columns that contain "scs_neg_" in their names
if(0) {
  neg_items <- grep("scs_neg_", colnames(df), value = TRUE)
  # Invert the values for all "scs_neg_" columns (assuming the maximum score is 3)
  df[neg_items] <- 3 - df[neg_items]
}


# Renaming the "scs_pos_" and "scs_neg_" columns in the desired sequence
colnames(df)[colnames(df) == "scs_pos_1"] <- "scs_pos_1"
colnames(df)[colnames(df) == "scs_pos_3"] <- "scs_pos_2"
colnames(df)[colnames(df) == "scs_pos_6"] <- "scs_pos_3"
colnames(df)[colnames(df) == "scs_pos_7"] <- "scs_pos_4"

colnames(df)[colnames(df) == "scs_neg_2"] <- "scs_neg_1"
colnames(df)[colnames(df) == "scs_neg_4"] <- "scs_neg_2"
colnames(df)[colnames(df) == "scs_neg_5"] <- "scs_neg_3"
colnames(df)[colnames(df) == "scs_neg_8"] <- "scs_neg_4"

# Check the updated column names
names(df)
# 
# df$scs_neg_1 <- df$scs_neg_1 - 3
# df$scs_neg_2 <- df$scs_neg_2 - 3
# df$scs_neg_3 <- df$scs_neg_3 - 3
# df$scs_neg_4 <- df$scs_neg_4 - 3

cor(df[, 4:11]) |> round(2)


# Generate input list for Stan --------------------------------------------

# Assuming the dataframe is called df
# Combining CS and UCS items into overall CS and UCS scores
df <- df %>%
  mutate(
    # Calculate Compassionate Self (CS) as the sum of positive items
    CS = scs_pos_1 + scs_pos_2 + scs_pos_3 + scs_pos_4,
    
    # Calculate Uncompassionate Self (UCS) as the sum of negative items
    UCS = scs_neg_1 + scs_neg_2 + scs_neg_3 + scs_neg_4
  )


df <- df[!is.na(df$time_window), ]



# Generate the list to be used as input for the Stan model
stan_data <- list(
  N = nrow(df),  # Total number of observations
  P = length(unique(df$user_id)),  # Number of participants
  D = length(unique(df$day)),  # Number of days
  M = length(unique(df$time_window)),  # Number of measurements per day per participant
  
  # Participant, day, and measurement indices
  participant = as.integer(as.factor(df$user_id)),  # Convert user_id to an integer index
  day = as.integer(as.factor(df$day)),  # Convert day to an integer index
  measurement = as.integer(as.factor(df$time_window)),  # Convert time_window to an integer index
  
  # Compassionate Self and Uncompassionate Self measures
  CS = df$CS,
  UCS = df$UCS,
  
  # Scaled and centered negative affect variables
  neg_aff_Moment = df$neg_aff_Moment,
  neg_aff_Day = df$neg_aff_Day,
  neg_aff_Person = df$neg_aff_Person,
  
  # Scaled and centered context evaluation variables
  context_eval_Moment = df$context_Moment,
  context_eval_Day = df$context_Day,
  context_eval_Person = df$context_Person
)

# Print a summary to ensure correctness
str(stan_data)

summary(stan_data$measurement)

stan_file <- here::here(
  "02_idiographic_test", 
  "stan",
  "idiographic_model.stan"
  )

mod <- cmdstan_model(stan_file)

fit <- mod$variational(
  data = stan_data,
  seed = 42
)

fit_mcmc <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  seed = 42
)

# Extract posterior samples
posterior_samples <- fit_mcmc$draws(format = "df")

# View the structure of the posterior samples
str(posterior_samples)

# Extract the relevant posterior samples for Neff's hypothesis
beta_cs_samples <- posterior_samples$`beta_cs`
beta_interaction_samples <- posterior_samples$`beta_interaction`
sigma_cs_slope_samples <- posterior_samples$`sigma_participant_slope_cs`

# Summary statistics for beta_cs
summary_beta_cs <- summary(beta_cs_samples)
hist(summary_beta_cs)

# Summary statistics for beta_interaction
summary_beta_interaction <- summary(beta_interaction_samples)
hist(summary_beta_interaction)

# Summary statistics for sigma_participant_slope_cs
summary_sigma_cs_slope <- summary(sigma_cs_slope_samples)
print(summary_sigma_cs_slope)

# Plot posterior distribution of beta_cs
mcmc_areas(as.matrix(posterior_samples), pars = c("beta_cs")) +
  ggtitle("Posterior Distribution of beta_cs") +
  xlab("Effect of CS on UCS") +
  ylab("Density")


posterior_summary <- fit$summary(
  variables = c(
    "beta_cs", "beta_interaction", "sigma_participant_slope_cs"
    )
  )
print(posterior_summary)
# variable                       mean   median      sd     mad      q5      q95
# <chr>                         <dbl>    <dbl>   <dbl>   <dbl>   <dbl>    <dbl>
#   1 beta_cs                    -0.459   -0.459   0.00686 0.00699 -0.470  -0.447  
# 2 beta_interaction           -0.00960 -0.00959 0.00404 0.00410 -0.0160 -0.00280
# 3 sigma_participant_slope_cs  0.226    0.226   0.00221 0.00215  0.223   0.230  

# Extract the individual slopes for CS (z_participant_slope_cs) from the posterior samples
z_participant_slope_cs_samples <- posterior_samples %>% 
  dplyr::select(starts_with("z_participant_slope_cs"))

# Calculate the mean slope for each individual
individual_slopes <- apply(as.matrix(z_participant_slope_cs_samples), 2, mean)

# Print summary of individual slopes
summary(individual_slopes)

# Extract the posterior samples for the fixed effect (beta_cs) and random slopes (z_participant_slope_cs)
beta_cs_samples <- posterior_samples$`beta_cs`
z_participant_slope_cs_samples <- posterior_samples %>% 
  dplyr::select(starts_with("z_participant_slope_cs"))

# Sum the fixed effect (beta_cs) and the random effect (z_participant_slope_cs) for each participant
# This gives you the total slope for each participant at each posterior draw
individual_total_slope_samples <- sweep(z_participant_slope_cs_samples, 1, beta_cs_samples, "+")

# Calculate the mean total slope for each participant
individual_total_slopes <- apply(as.matrix(individual_total_slope_samples), 2, mean)

# Print summary of the total individual slopes
summary(individual_total_slopes)

# Plot the distribution of total individual slopes (fixed + random effects)
ggplot(data.frame(slope = individual_total_slopes), aes(x = slope)) +
  geom_histogram(binwidth = 0.05, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Total Individual Slopes for CS on UCS",
       x = "Total Individual CS Effect on UCS (Fixed + Random Effects)", y = "Count")


mean(individual_total_slopes < 0)
# [1] 0.6606061

# Save fit.
# fit_mcmc$save_object("idiographic_fit.rds")


# 1. CS Effect on UCS (beta_cs) -------------------------------------------

# Extract beta_cs and display summary statistics
beta_cs_samples <- posterior_samples$`beta_cs`
# Calculate the 5.5th and 94.5th percentiles
lower_bound <- quantile(beta_cs_samples, 0.055)
upper_bound <- quantile(beta_cs_samples, 0.945)
avg <- mean(beta_cs_samples)
# Print the 89% credibility interval
cat("89% Credibility Interval for beta_cs:", avg, "-", lower_bound, "-", upper_bound, "\n")
# 89% Credibility Interval for beta_cs: -0.3798418 - -0.4065441 - -0.3517241 


# Parameter sigma_participant_slope_cs ------------------------------------

#' This parameter reflects individual variability in how CS influences UCS. 
#' A large value suggests that the relationship between CS and UCS varies 
#' significantly across individuals, which could indicate that Neff's 
#' hypothesis does not hold uniformly for everyone.

sigma_participant_slope_cs_samples <- posterior_samples$`sigma_participant_slope_cs`
# Calculate the 5.5th and 94.5th percentiles
lower_bound <- quantile(sigma_participant_slope_cs_samples, 0.055)
upper_bound <- quantile(sigma_participant_slope_cs_samples, 0.945)
avg <- mean(sigma_participant_slope_cs_samples)
# Print the 89% credibility interval
cat(" Posterior Mean and 89% Credibility Interval for beta_cs:",
    avg, "-", lower_bound, "-", upper_bound, "\n")


# 2. Interaction Between CS and NA (beta_interaction)
# Extract beta_interaction and display summary statistics
beta_interaction_samples <- posterior_samples$`beta_interaction`
lower_bound <- quantile(beta_interaction_samples, 0.055)
upper_bound <- quantile(beta_interaction_samples, 0.945)
avg <- mean(beta_interaction_samples)
# Print the 89% credibility interval
cat(" Posterior Mean and 89% Credibility Interval for beta_cs:",
    avg, "-", lower_bound, "-", upper_bound, "\n")

# Plot posterior distribution for beta_interaction
mcmc_areas(as.matrix(posterior_samples), pars = "beta_interaction") +
  ggtitle("Posterior Distribution of beta_interaction (CS-NA Interaction)") +
  xlab("Interaction Effect of CS and NA on UCS") +
  ylab("Density")

# 3. Extract and display summary statistics for negative affect components
neg_aff_params <- posterior_samples %>% 
  select(beta_neg_aff_Moment, beta_neg_aff_Day, beta_neg_aff_Person)
summary_neg_aff <- apply(neg_aff_params, 2, summary)
print(summary_neg_aff)


# 4 Extract and display summary statistics for context evaluation components
context_eval_params <- posterior_samples %>% 
  select(beta_context_eval_Moment, beta_context_eval_Day, beta_context_eval_Person)
summary_context_eval <- apply(context_eval_params, 2, summary)
print(summary_context_eval)




# Extract random slopes for CS and sum with fixed effect (beta_cs) to get total slopes
z_participant_slope_cs_samples <- posterior_samples %>% 
  dplyr::select(starts_with("z_participant_slope_cs"))
beta_cs_samples <- posterior_samples$`beta_cs`

# Calculate total individual slopes for CS
individual_total_slope_samples <- sweep(z_participant_slope_cs_samples, 1, beta_cs_samples, "+")
individual_total_slopes <- apply(as.matrix(individual_total_slope_samples), 2, mean)

# Summary of individual total slopes
summary_individual_total_slopes <- summary(individual_total_slopes)
print(summary_individual_total_slopes)

# Plot the distribution of total individual slopes
ggplot(data.frame(slope = individual_total_slopes), aes(x = slope)) +
  geom_histogram(binwidth = 0.05, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Total Individual Slopes for CS on UCS",
       x = "Total Individual CS Effect on UCS (Fixed + Random Effects)", y = "Count")

mean(summary_individual_total_slopes < 0)


# Extract the fixed effect for NA (for the Moment-level, Day-level, or Person-level, depending on which one you're examining)
beta_neg_aff_Moment_samples <- posterior_samples$`beta_neg_aff_Moment`

# Extract the random slopes for NA (for the Moment-level NA) from the posterior samples
z_participant_slope_na_samples <- posterior_samples %>% 
  dplyr::select(starts_with("z_participant_slope_na"))

# Sum the fixed effect (beta_neg_aff_Moment) and the random effect (z_participant_slope_na) to get total effect
individual_total_slope_na_samples <- sweep(z_participant_slope_na_samples, 1, beta_neg_aff_Moment_samples, "+")

# Calculate the mean total effect for each participant
individual_total_slopes_na <- apply(as.matrix(individual_total_slope_na_samples), 2, mean)

# Print the summary of total individual slopes for NA
summary_individual_total_slopes_na <- summary(individual_total_slopes_na)
print(summary_individual_total_slopes_na)

# Plot the distribution of total individual slopes for NA on UCS
ggplot(data.frame(slope = individual_total_slopes_na), aes(x = slope)) +
  geom_histogram(binwidth = 0.05, fill = "green", alpha = 0.7) +
  labs(title = "Distribution of Total Individual Slopes for NA on UCS (Fixed + Random Effects)",
       x = "Total Individual NA Effect on UCS", y = "Count")

# Check the proportion of participants for whom the total NA effect is positive
mean(individual_total_slopes_na > 0)


# Extract and display variance parameters (standard deviations)
variance_params <- posterior_samples %>% 
  dplyr::select(sigma_participant, sigma_day, sigma_measurement, 
                sigma_participant_slope_cs, sigma_participant_slope_na, sigma_ucs)

summary_variance_params <- apply(variance_params, 2, summary)
print(summary_variance_params)


# Extract residual variance (sigma_ucs) and display summary
sigma_ucs_samples <- posterior_samples$`sigma_ucs`
summary_sigma_ucs <- summary(sigma_ucs_samples)
print(summary_sigma_ucs)

# Plot posterior distribution of residual variance
mcmc_areas(as.matrix(posterior_samples), pars = "sigma_ucs") +
  ggtitle("Posterior Distribution of Residual Variance (sigma_ucs)") +
  xlab("Residual Variance in UCS") +
  ylab("Density")




# Extract the posterior predictions (assuming you named them `pred_UCS` in the generated quantities)
posterior_predictions <- fit_mcmc$draws(variables = "pred_UCS", format = "df")
observed_UCS <- df$UCS 

# Reshape posterior predictions into matrix format (rows = posterior draws, columns = observations)
ppc_data <- posterior_predictions %>% as.matrix()

# Check for NA values in observed UCS
missing_indices <- which(is.na(observed_UCS))

# Remove rows in ppc_data corresponding to missing values in observed_UCS
if (length(missing_indices) > 0) {
  ppc_data <- ppc_data[, -missing_indices]
}

# Ensure that the dimensions now match
print(length(observed_UCS))
print(ncol(ppc_data))

# Subset ppc_data to match the length of observed_UCS
ppc_data <- ppc_data[, 1:length(observed_UCS)]


temp <- c(observed_UCS, 0, 0, 0)
observed_UCS <- temp

# Ensure that the dimensions now match
print(length(observed_UCS))  # Should print 19397
print(ncol(ppc_data))  # Should now print 19397


# Posterior predictive check: density overlay
ppc_dens_overlay(y = observed_UCS, yrep = ppc_data) +
  ggtitle("Posterior Predictive Check: Observed vs. Predicted UCS")

# Posterior predictive check: histogram overlay
ppc_hist(y = observed_UCS, yrep = ppc_data) + xlim(-10, 10) +
  ggtitle("Posterior Predictive Check: Histogram of Observed vs. Predicted UCS")


n <- 200
obs <- sample(observed_UCS, n)
yrep <- ppc_data[, sample(1:19400, n)]
ppc_dens_overlay(y = obs, yrep = yrep)


library(loo)

# Extract log-likelihood from the posterior samples
log_lik <- fit_mcmc$draws(variables = "log_lik", format = "matrix")

# Make sure the dimensions match between the log-likelihood matrix and the number of observations
dim(log_lik)

# Compute LOO and Pareto k diagnostics
loo_result <- loo::loo(log_lik)

# Print the LOO result and Pareto k diagnostics
print(loo_result)

# View the Pareto k diagnostics
pareto_k <- loo_result$diagnostics$pareto_k
summary(pareto_k)


# ----------------


# Extract the fixed effect (beta_cs) and random slopes for CS (z_participant_slope_cs)
beta_cs_samples <- fit_mcmc$draws(variables = "beta_cs", format = "matrix")
z_participant_slope_cs_samples <- fit_mcmc$draws(variables = "z_participant_slope_cs", format = "matrix")

# Number of participants
num_participants <- length(unique(df$user_id))

# Ensure z_participant_slope_cs_samples has the correct number of participants
if (ncol(z_participant_slope_cs_samples) != num_participants) {
  stop("Number of random slopes doesn't match the number of participants.")
}

# Sum the fixed effect (beta_cs) and random slopes (z_participant_slope_cs) for each participant
# This gives you the total slope for each participant at each posterior draw
individual_total_slope_samples <- sweep(z_participant_slope_cs_samples, 1, beta_cs_samples, "+")

# Calculate the mean total slope for each participant
individual_total_slopes <- apply(individual_total_slope_samples, 2, mean)

# Create a dataframe with user_id and the posterior mean total slope for each participant
participant_slopes_df <- data.frame(
  user_id = unique(df$user_id),
  total_slope_cs_ucs = individual_total_slopes
)

# View the resulting dataframe
print(participant_slopes_df)


# R hat

# Extract the summary of the model, which includes Rhat values
fit_summary <- fit_mcmc$summary()

# View the structure of the summary to inspect its contents
str(fit_summary)

# Extract Rhat values from the summary
rhat_values <- fit_summary$rhat

# Calculate the maximum Rhat value
max_rhat <- max(rhat_values, na.rm = TRUE)  # Use na.rm = TRUE to ignore any NA values

# Calculate the mean Rhat value
mean_rhat <- mean(rhat_values, na.rm = TRUE)

# Print the maximum and mean Rhat values
cat("Maximum Rhat:", max_rhat, "\n")
cat("Mean Rhat:", mean_rhat, "\n")



