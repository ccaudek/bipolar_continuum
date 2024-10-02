#' Idiographic analysis of Neff's hypothesis



# Setup -------------------------------------------------------------------

# Load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, rio, tidyverse, cmdstanr, posterior, 
               bayesplot, purrr, vars, forecast, cluster)

# Set seed
set.seed(42)

# Load helper functions
source(here::here("R", "importing_cleaning_data.R"))


# Import and clean data ---------------------------------------------------

df <- get_data(reverse_coding_ucs = 0)

length(unique(df$user_id))
# [1] 495

df |> 
  dplyr::select(starts_with("scs_")) |> 
  cor() |> 
  round(2)

# Create a function to calculate the sum of columns starting with a specific pattern
sum_columns <- function(df, pattern) {
  df %>%
    dplyr::select(starts_with(pattern)) %>%
    rowSums(na.rm = TRUE)  # Sum across rows, ignoring NA values
}

# Apply the function to create the UCS and CS columns
df <- df %>%
  mutate(
    UCS = sum_columns(., "scs_neg_"),
    CS = sum_columns(., "scs_pos_")
  )

# Create a new column by combining day and time_window
df <- df %>%
  mutate(occasion = paste0(day, "_", time_window))

# Check the number of observations per user
if(0) {
  out <- df %>%
    group_by(user_id) %>%
    summarise(n_obs = n()) %>%
    dplyr::filter(n_obs >= 10)  # Filter users with at least 10 observations
}


# First, calculate the number of observations and variances for UCS and CS
occasion_summary <- df %>%
  dplyr::group_by(user_id, day, time_window) %>%
  dplyr::summarise(
    n_obs = n(),
    UCS_var = var(UCS, na.rm = TRUE),
    CS_var = var(CS, na.rm = TRUE),
    .groups = 'drop'
  )

# Filter out groups with fewer than 3 observations and zero variance in UCS or CS
occasion_filtered <- occasion_summary %>%
  dplyr::filter(n_obs >= 3 & UCS_var > 0 & CS_var > 0)

# Now calculate the correlation between UCS and CS for the filtered data
occasion_correlations <- df %>%
  dplyr::semi_join(occasion_filtered, by = c("user_id", "day", "time_window")) %>%
  dplyr::group_by(user_id, day, time_window) %>%
  dplyr::summarise(correlation = cor(UCS, CS, use = "complete.obs"), .groups = 'drop')

# View the cleaned correlation data
head(occasion_correlations)

# Plot the distribution of correlations after filtering
ggplot(occasion_correlations, aes(x = correlation)) +
  geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of UCS ~ CS Correlations After Filtering", x = "Correlation", y = "Frequency")

# Calculate the proportion of negative correlations
mean(occasion_correlations$correlation < 0)
# [1] 0.7590361


# Regression-Based Approach -----------------------------------------------


# Filter participants with at least 10 data points and remove rows with missing values
cleaned_data <- df %>%
  group_by(user_id) %>%
  dplyr::filter(n() >= 10) %>%
  ungroup() %>%
  dplyr::filter(!is.na(CS) & !is.na(UCS) & !is.na(neg_aff_Moment) & !is.na(context_Moment))


# Define the function to perform regression and handle errors
regression_by_participant <- function(participant_data) {
  tryCatch({
    # Perform the regression for each participant
    lm(UCS ~ CS * neg_aff_Moment * context_Moment, data = participant_data)
  }, error = function(e) {
    message("Error for participant: ", unique(participant_data$user_id))
    return(NA)
  })
}

# Apply the regression function to each participant's data
individual_regression_results <- cleaned_data %>%
  group_by(user_id) %>%
  nest() %>%
  mutate(regression_results = map(data, regression_by_participant))


# Function to extract coefficients (CS and interaction term), handling NA models
extract_multiple_coefficients <- function(model) {
  # Check if the model is NA
  if (is.null(model) || inherits(model, "logical") && is.na(model)) {
    return(list(cs_coef = NA, interaction_coef = NA))  # Return NA for failed models
  }
  
  # Extract coefficients from valid models
  coef_summary <- summary(model)$coefficients
  cs_coef <- if ("CS" %in% rownames(coef_summary)) coef_summary["CS", "Estimate"] else NA
  interaction_coef <- if ("CS:neg_aff_Moment" %in% rownames(coef_summary)) coef_summary["CS:neg_aff_Moment", "Estimate"] else NA
  
  return(list(cs_coef = cs_coef, interaction_coef = interaction_coef))
}

# Apply the coefficient extraction function to each model
individual_regression_results <- individual_regression_results %>%
  mutate(coefs = map(regression_results, extract_multiple_coefficients))

# Separate the coefficients into individual columns for easier analysis
individual_regression_results <- individual_regression_results %>%
  mutate(cs_coefficient = map_dbl(coefs, "cs_coef"),
         interaction_coefficient = map_dbl(coefs, "interaction_coef"))

# View the extracted coefficients
print(individual_regression_results %>% 
        dplyr::select(user_id, cs_coefficient, interaction_coefficient))

# Calculate the proportion of participants with a negative UCS/CS relationship
negative_slope_proportion <- individual_regression_results %>%
  summarize(proportion_negative_cs_slope = mean(cs_coefficient < 0, na.rm = TRUE))

# Print the result
mean(negative_slope_proportion$proportion_negative_cs_slope)

# Calculate the proportion of participants with a significant interaction effect
significant_interaction_proportion <- individual_regression_results %>%
  summarize(proportion_significant_interaction = mean(abs(interaction_coefficient) > 0, na.rm = TRUE))

# Print the result
mean(significant_interaction_proportion$proportion_significant_interaction, na.rm = T)


# Proportion of participants where the interaction term is negative
negative_interaction_proportion <- individual_regression_results %>%
  summarize(proportion_negative_interaction = mean(interaction_coefficient < 0, na.rm = TRUE))

mean(negative_interaction_proportion$proportion_negative_interaction, na.rm = T)


# Autoregressive model ----------------------------------------------------

# 
# # Ensure that the data is sorted by user_id, day, and time_window
# df <- df %>%
#   arrange(user_id, day, time_window)
# 
# # Create lagged variables for UCS and CS (lag by 1 occasion)
# df <- df %>%
#   group_by(user_id) %>%  # Group by participant
#   mutate(
#     lagged_UCS = lag(UCS, 1),
#     lagged_CS = lag(CS, 1),
#     lagged_neg_aff_Moment = lag(neg_aff_Moment, 1),
#     lagged_context_Moment = lag(context_Moment, 1)
#   ) %>%
#   ungroup()  # Ungroup for further analysis
# 
# 
# # Define the function to perform the lagged regression for each participant
# lagged_regression_by_participant <- function(participant_data) {
#   tryCatch({
#     # Perform the lagged regression for each participant
#     lm(UCS ~ lagged_UCS + lagged_CS * neg_aff_Moment * context_Moment, data = participant_data)
#   }, error = function(e) {
#     message("Error for participant: ", unique(participant_data$user_id))
#     return(NA)
#   })
# }
# 
# # Apply the regression function to each participant's data
# lagged_regression_results <- df %>%
#   group_by(user_id) %>%
#   nest() %>%
#   mutate(regression_results = map(data, lagged_regression_by_participant))
# 
# # Print the regression results for the first few participants
# print(lagged_regression_results %>% 
#         slice(1:5) %>% 
#         dplyr::select(user_id, regression_results))
# 
# # Function to extract coefficients for lagged UCS, lagged CS, and interactions
# extract_lagged_coefficients <- function(model) {
#   if (is.null(model) || inherits(model, "logical") && is.na(model)) {
#     return(list(lagged_UCS_coef = NA, lagged_CS_coef = NA, interaction_coef = NA))  # Handle failed models
#   }
#   
#   coef_summary <- summary(model)$coefficients
#   lagged_UCS_coef <- if ("lagged_UCS" %in% rownames(coef_summary)) coef_summary["lagged_UCS", "Estimate"] else NA
#   lagged_CS_coef <- if ("lagged_CS" %in% rownames(coef_summary)) coef_summary["lagged_CS", "Estimate"] else NA
#   interaction_coef <- if ("lagged_CS:neg_aff_Moment" %in% rownames(coef_summary)) coef_summary["lagged_CS:neg_aff_Moment", "Estimate"] else NA
#   
#   return(list(lagged_UCS_coef = lagged_UCS_coef, lagged_CS_coef = lagged_CS_coef, interaction_coef = interaction_coef))
# }
# 
# # Apply the extraction function to each regression model
# lagged_regression_results <- lagged_regression_results %>%
#   mutate(coefs = map(regression_results, extract_lagged_coefficients))
# 
# # Separate the coefficients into individual columns
# lagged_regression_results <- lagged_regression_results %>%
#   mutate(lagged_UCS_coefficient = map_dbl(coefs, "lagged_UCS_coef"),
#          lagged_CS_coefficient = map_dbl(coefs, "lagged_CS_coef"),
#          interaction_coefficient = map_dbl(coefs, "interaction_coef"))
# 
# # View the extracted coefficients
# print(lagged_regression_results %>%
#         dplyr::select(user_id, lagged_UCS_coefficient, lagged_CS_coefficient, interaction_coefficient) %>%
#         head())
# 
# # Lower the threshold to capture smaller effects
# meaningful_results <- lagged_regression_results %>%
#   dplyr::filter(abs(lagged_CS_coefficient) > 0.001 | abs(interaction_coefficient) > 0.001)
# 
# # View the filtered results
# print(meaningful_results %>%
#         dplyr::select(user_id, lagged_UCS_coefficient, lagged_CS_coefficient, interaction_coefficient) %>%
#         head())
# 
# # Summarize the coefficients to get a better sense of their magnitude
# summary_stats <- lagged_regression_results %>%
#   dplyr::summarize(
#     min_lagged_CS = min(lagged_CS_coefficient, na.rm = TRUE),
#     max_lagged_CS = max(lagged_CS_coefficient, na.rm = TRUE),
#     mean_lagged_CS = mean(lagged_CS_coefficient, na.rm = TRUE),
#     median_lagged_CS = median(lagged_CS_coefficient, na.rm = TRUE),
#     sd_lagged_CS = sd(lagged_CS_coefficient, na.rm = TRUE),
#     
#     min_interaction = min(interaction_coefficient, na.rm = TRUE),
#     max_interaction = max(interaction_coefficient, na.rm = TRUE),
#     mean_interaction = mean(interaction_coefficient, na.rm = TRUE),
#     median_interaction = median(interaction_coefficient, na.rm = TRUE),
#     sd_interaction = sd(interaction_coefficient, na.rm = TRUE)
#   )
# 
# print(summary_stats)
# 
# # Check for missing values in the coefficients
# missing_values_summary <- lagged_regression_results %>%
#   dplyr::summarize(
#     missing_lagged_CS = sum(is.na(lagged_CS_coefficient)),
#     missing_interaction = sum(is.na(interaction_coefficient)),
#     total_participants = n()
#   )
# 
# summary(missing_values_summary)
# 
# # Filter out participants with missing interaction coefficients
# filtered_results <- lagged_regression_results %>%
#   dplyr::filter(!is.na(interaction_coefficient))
# 
# # Recalculate summary statistics after filtering
# summary_stats_filtered <- filtered_results %>%
#   dplyr::summarize(
#     min_lagged_CS = min(lagged_CS_coefficient, na.rm = TRUE),
#     max_lagged_CS = max(lagged_CS_coefficient, na.rm = TRUE),
#     mean_lagged_CS = mean(lagged_CS_coefficient, na.rm = TRUE),
#     median_lagged_CS = median(lagged_CS_coefficient, na.rm = TRUE),
#     sd_lagged_CS = sd(lagged_CS_coefficient, na.rm = TRUE),
#     
#     min_interaction = min(interaction_coefficient, na.rm = TRUE),
#     max_interaction = max(interaction_coefficient, na.rm = TRUE),
#     mean_interaction = mean(interaction_coefficient, na.rm = TRUE),
#     median_interaction = median(interaction_coefficient, na.rm = TRUE),
#     sd_interaction = sd(interaction_coefficient, na.rm = TRUE)
#   )
# 
# print(summary_stats_filtered)
# 
# # Histogram for lagged CS coefficients
# ggplot(filtered_results, aes(x = lagged_CS_coefficient)) +
#   geom_histogram(binwidth = 0.0001, fill = "blue", color = "black") +
#   labs(title = "Distribution of Lagged CS Coefficients", x = "Lagged CS Coefficient", y = "Frequency")
# 
# # Histogram for interaction coefficients
# ggplot(filtered_results, aes(x = interaction_coefficient)) +
#   geom_histogram(binwidth = 0.0001, fill = "green", color = "black") +
#   labs(title = "Distribution of Interaction Coefficients", x = "Interaction Coefficient", y = "Frequency")
# 
# 
# # Define the function to perform regression without lagged variables for each participant
# regression_by_participant <- function(participant_data) {
#   tryCatch({
#     # Perform the regression using only current UCS, CS, and contextual variables
#     lm(UCS ~ CS * neg_aff_Moment * context_Moment, data = participant_data)
#   }, error = function(e) {
#     message("Error for participant: ", unique(participant_data$user_id))
#     return(NA)
#   })
# }
# 
# # Apply the regression function to each participant's data
# regression_results_no_lag <- df %>%
#   group_by(user_id) %>%
#   nest() %>%
#   mutate(regression_results = map(data, regression_by_participant))
# 
# # Extract coefficients for CS and the interactions
# extract_coefficients <- function(model) {
#   if (is.null(model) || inherits(model, "logical") && is.na(model)) {
#     return(list(cs_coef = NA, interaction_coef = NA))  # Handle failed models
#   }
#   
#   coef_summary <- summary(model)$coefficients
#   cs_coef <- if ("CS" %in% rownames(coef_summary)) coef_summary["CS", "Estimate"] else NA
#   interaction_coef <- if ("CS:neg_aff_Moment" %in% rownames(coef_summary)) coef_summary["CS:neg_aff_Moment", "Estimate"] else NA
#   
#   return(list(cs_coef = cs_coef, interaction_coef = interaction_coef))
# }
# 
# # Apply the extraction function to each regression model
# regression_results_no_lag <- regression_results_no_lag %>%
#   mutate(coefs = map(regression_results, extract_coefficients))
# 
# # Separate the coefficients into individual columns for further analysis
# regression_results_no_lag <- regression_results_no_lag %>%
#   mutate(cs_coefficient = map_dbl(coefs, "cs_coef"),
#          interaction_coefficient = map_dbl(coefs, "interaction_coef"))
# 
# # View the extracted coefficients
# print(regression_results_no_lag %>%
#         dplyr::select(user_id, cs_coefficient, interaction_coefficient) %>%
#         head())





# Define the function to perform regression without lagged variables for each participant
regression_by_participant <- function(participant_data) {
  tryCatch({
    # Perform the regression using only current UCS, CS, and contextual variables
    lm(UCS ~ CS * neg_aff_Moment * context_Moment, data = participant_data)
  }, error = function(e) {
    message("Error for participant: ", unique(participant_data$user_id))
    return(NA)
  })
}

# Apply the regression function to each participant's data
regression_results_no_lag <- df %>%
  group_by(user_id) %>%
  nest() %>%
  mutate(regression_results = map(data, regression_by_participant))

# Extract coefficients for CS and the interactions
extract_coefficients <- function(model) {
  if (is.null(model) || inherits(model, "logical") && is.na(model)) {
    return(list(cs_coef = NA, interaction_coef = NA))  # Handle failed models
  }
  
  coef_summary <- summary(model)$coefficients
  cs_coef <- if ("CS" %in% rownames(coef_summary)) coef_summary["CS", "Estimate"] else NA
  interaction_coef <- if ("CS:neg_aff_Moment" %in% rownames(coef_summary)) coef_summary["CS:neg_aff_Moment", "Estimate"] else NA
  
  return(list(cs_coef = cs_coef, interaction_coef = interaction_coef))
}

# Apply the extraction function to each regression model
regression_results_no_lag <- regression_results_no_lag %>%
  mutate(coefs = map(regression_results, extract_coefficients))

# Separate the coefficients into individual columns for further analysis
regression_results_no_lag <- regression_results_no_lag %>%
  mutate(cs_coefficient = map_dbl(coefs, "cs_coef"),
         interaction_coefficient = map_dbl(coefs, "interaction_coef"))

# View the extracted coefficients
print(regression_results_no_lag %>%
        dplyr::select(user_id, cs_coefficient, interaction_coefficient) %>%
        head())

# Plot the distribution of CS coefficients
ggplot(regression_results_no_lag, aes(x = cs_coefficient)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Distribution of CS Coefficients", x = "CS Coefficient", y = "Frequency")

# Plot the distribution of interaction coefficients (CS * neg_aff_Moment)
ggplot(regression_results_no_lag, aes(x = interaction_coefficient)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black") +
  labs(title = "Distribution of Interaction Coefficients", x = "Interaction Coefficient", y = "Frequency")

# Filter for participants with meaningful interaction coefficients
meaningful_results <- regression_results_no_lag %>%
  dplyr::filter(abs(interaction_coefficient) > 0.01)

# View the results
print(meaningful_results %>%
        dplyr::select(user_id, cs_coefficient, interaction_coefficient) %>%
        head())

# Calculate summary statistics for CS and interaction coefficients
summary_stats <- meaningful_results %>%
  dplyr::summarize(
    mean_cs = mean(cs_coefficient, na.rm = TRUE),
    median_cs = median(cs_coefficient, na.rm = TRUE),
    sd_cs = sd(cs_coefficient, na.rm = TRUE),
    min_cs = min(cs_coefficient, na.rm = TRUE),
    max_cs = max(cs_coefficient, na.rm = TRUE),
    
    mean_interaction = mean(interaction_coefficient, na.rm = TRUE),
    median_interaction = median(interaction_coefficient, na.rm = TRUE),
    sd_interaction = sd(interaction_coefficient, na.rm = TRUE),
    min_interaction = min(interaction_coefficient, na.rm = TRUE),
    max_interaction = max(interaction_coefficient, na.rm = TRUE)
  )

mean(meaningful_results$cs_coefficient < 0)

mean(meaningful_results$interaction_coefficient < 0)


# Calculate the within-person correlation between CS and UCS
within_person_correlation <- df %>%
  group_by(user_id) %>%
  summarize(correlation = cor(CS, UCS, use = "complete.obs"))

# View the correlations
print(within_person_correlation)


# Cross validation --------------------------------------------------------

# Import and clean data
df <- get_data_2(reverse_coding_ucs = 0)

length(unique(df$user_id))
# [1] 495

df |> 
  dplyr::select(starts_with("scs_")) |> 
  cor() |> 
  round(2)

# Create a function to calculate the sum of columns starting with a specific pattern
sum_columns <- function(df, pattern) {
  df %>%
    dplyr::select(starts_with(pattern)) %>%
    rowSums(na.rm = TRUE)  # Sum across rows, ignoring NA values
}

# Apply the function to create the UCS and CS columns
df <- df %>%
  mutate(
    UCS = sum_columns(., "scs_neg_"),
    CS = sum_columns(., "scs_pos_")
  )

# Create a new column by combining day and time_window
df <- df %>%
  mutate(occasion = paste0(day, "_", time_window))

# Split the data into two datasets based on the 'study' variable
study1_data <- df %>% dplyr::filter(study == 1)
study2_data <- df %>% dplyr::filter(study == 2)

# Define the regression function
regression_by_participant <- function(participant_data) {
  tryCatch({
    # Perform the regression using current UCS, CS, and contextual variables
    lm(UCS ~ CS * neg_aff_Moment * context_Moment, data = participant_data)
  }, error = function(e) {
    message("Error for participant: ", unique(participant_data$user_id))
    return(NA)
  })
}

# Function to extract coefficients
extract_coefficients <- function(model) {
  if (is.null(model) || inherits(model, "logical") && is.na(model)) {
    return(list(cs_coef = NA, interaction_coef = NA))  # Handle failed models
  }
  
  coef_summary <- summary(model)$coefficients
  cs_coef <- if ("CS" %in% rownames(coef_summary)) coef_summary["CS", "Estimate"] else NA
  interaction_coef <- if ("CS:neg_aff_Moment" %in% rownames(coef_summary)) coef_summary["CS:neg_aff_Moment", "Estimate"] else NA
  
  return(list(cs_coef = cs_coef, interaction_coef = interaction_coef))
}

# Apply the regression to each participant's data for Study 1
regression_results_study1 <- study1_data %>%
  group_by(user_id) %>%
  nest() %>%
  mutate(regression_results = map(data, regression_by_participant),
         coefs = map(regression_results, extract_coefficients),
         cs_coefficient = map_dbl(coefs, "cs_coef"),
         interaction_coefficient = map_dbl(coefs, "interaction_coef"))

# Apply the regression to each participant's data for Study 2
regression_results_study2 <- study2_data %>%
  group_by(user_id) %>%
  nest() %>%
  mutate(regression_results = map(data, regression_by_participant),
         coefs = map(regression_results, extract_coefficients),
         cs_coefficient = map_dbl(coefs, "cs_coef"),
         interaction_coefficient = map_dbl(coefs, "interaction_coef"))

# Calculate the proportion of participants with negative UCS/CS slope in Study 1
negative_cs_proportion_study1 <- mean(regression_results_study1$cs_coefficient < 0, na.rm = TRUE)
cat("Proportion of negative UCS/CS slopes in Study 1:", negative_cs_proportion_study1, "\n")

# Calculate the proportion of participants with negative UCS/CS slope in Study 2
negative_cs_proportion_study2 <- mean(regression_results_study2$cs_coefficient < 0, na.rm = TRUE)
cat("Proportion of negative UCS/CS slopes in Study 2:", negative_cs_proportion_study2, "\n")

# Compare Interaction Coefficients in Study 1 and Study 2
interaction_comparison <- regression_results_study1 %>%
  summarize(mean_interaction_study1 = mean(interaction_coefficient, na.rm = TRUE)) %>%
  bind_cols(regression_results_study2 %>%
              summarize(mean_interaction_study2 = mean(interaction_coefficient, na.rm = TRUE)))

print(interaction_comparison)

# Visualize the distribution of UCS/CS coefficients in both studies
ggplot(regression_results_study1, aes(x = cs_coefficient)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "CS Coefficients - Study 1", x = "CS Coefficient", y = "Frequency")

ggplot(regression_results_study2, aes(x = cs_coefficient)) +
  geom_histogram(binwidth = 0.1, fill = "red", color = "black") +
  labs(title = "CS Coefficients - Study 2", x = "CS Coefficient", y = "Frequency")

# Plot Interaction Coefficients for both studies
ggplot(regression_results_study1, aes(x = interaction_coefficient)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black") +
  labs(title = "Interaction Coefficients - Study 1", x = "Interaction Coefficient", y = "Frequency")

ggplot(regression_results_study2, aes(x = interaction_coefficient)) +
  geom_histogram(binwidth = 0.1, fill = "purple", color = "black") +
  labs(title = "Interaction Coefficients - Study 2", x = "Interaction Coefficient", y = "Frequency")

# Cross-Validation: Train on Study 1, Test on Study 2
# (This assumes you've fitted the model on Study 1 and want to predict Study 2)
study1_model <- lm(UCS ~ CS * neg_aff_Moment * context_Moment, data = study1_data)
study2_predictions <- predict(study1_model, newdata = study2_data)

# Evaluate performance on Study 2
study2_performance <- cor(study2_predictions, study2_data$UCS)
cat("Cross-Validation: Correlation between predicted UCS and actual UCS in Study 2:", study2_performance, "\n")

# Similarly, you can reverse the roles (Train on Study 2, Test on Study 1)
study2_model <- lm(UCS ~ CS * neg_aff_Moment * context_Moment, data = study2_data)
study1_predictions <- predict(study2_model, newdata = study1_data)

# Evaluate performance on Study 1
study1_performance <- cor(study1_predictions, study1_data$UCS)
cat("Cross-Validation: Correlation between predicted UCS and actual UCS in Study 1:", study1_performance, "\n")


# ARIMAX -------------------


# Filter out participants where the predictors have very low variance
df_filtered <- df_filtered %>%
  group_by(user_id) %>%
  dplyr::filter(sd(CS, na.rm = TRUE) > 1e-6 & 
                  sd(neg_aff_Moment, na.rm = TRUE) > 1e-6 & 
                  sd(context_Moment, na.rm = TRUE) > 1e-6) %>%
  ungroup()


# Define the ARIMAX model for each participant
arimax_by_participant <- function(participant_data) {
  tryCatch({
    # Fit ARIMAX model with UCS as the response and CS, neg_aff_Moment, context_Moment as predictors
    arima_model <- auto.arima(participant_data$UCS, 
                              xreg = as.matrix(participant_data %>%
                                                 dplyr::select(CS, neg_aff_Moment, context_Moment)),
                              allowdrift = FALSE)
    
    # Extract the coefficient for CS (the UCS-CS relationship)
    cs_coef <- arima_model$coef["CS"]
    
    return(cs_coef)
  }, error = function(e) {
    message("Error for participant: ", unique(participant_data$user_id), " - ", e$message)
    return(NA)
  })
}


# Apply the ARIMAX model to each participant's data
arimax_results <- df_filtered %>%
  group_by(user_id) %>%
  nest() %>%
  mutate(cs_coefficient = map_dbl(data, arimax_by_participant))


# View the ARIMAX results
print(arimax_results %>%
        dplyr::select(user_id, cs_coefficient))

# Summary of CS coefficients
summary(arimax_results$cs_coefficient)

# Check how many participants have a valid CS coefficient
sum(!is.na(arimax_results$cs_coefficient))

# Proportion of negative UCS/CS slopes
negative_cs_proportion <- mean(arimax_results$cs_coefficient < 0, na.rm = TRUE)
cat("Proportion of negative UCS/CS slopes:", negative_cs_proportion, "\n")


# Cluster analysis --------------------------------------------------------

# Ensure you have valid cs_coefficient data (remove rows where cs_coefficient is NA)
valid_pam_data <- arimax_results %>%
  dplyr::filter(!is.na(cs_coefficient)) %>%  # Remove participants with missing cs_coefficient
  dplyr::select(user_id, cs_coefficient)  # Keep only user_id and cs_coefficient

# Perform PAM clustering with 2 clusters
pam_fit <- pam(valid_pam_data$cs_coefficient, k = 2)

# Add cluster labels to the valid PAM data
# This works because pam_fit$clustering should be the same length as valid_pam_data
valid_pam_data$cluster <- as.factor(pam_fit$clustering)

# View the data with cluster labels
print(valid_pam_data)

# Visualize the cluster distributions
ggplot(valid_pam_data, aes(x = cs_coefficient, fill = cluster)) +
  geom_histogram(binwidth = 0.1, color = "black") +
  facet_wrap(~ cluster, scales = "free") +
  labs(title = "Cluster Analysis of UCS/CS Slopes", x = "CS Coefficient", y = "Count")

# Summarize the clusters
cluster_summary <- valid_pam_data %>%
  group_by(cluster) %>%
  summarise(mean_cs_slope = mean(cs_coefficient),
            sd_cs_slope = sd(cs_coefficient),
            n = n())

print(cluster_summary)



############# Bayes ----------------------------------------------------


# Step 1: Create lagged UCS for autoregressive component
df_filtered <- df_filtered %>%
  group_by(user_id) %>%
  mutate(
    lag_UCS = lag(UCS, 1),       # Lagged UCS
    diff_UCS = UCS - lag_UCS     # Differencing UCS for stationarity if needed
  ) %>%
  ungroup()

# Step 2: Filter out rows with missing lagged UCS
df_filtered <- df_filtered %>%
  dplyr::filter(!is.na(lag_UCS))

# Step 3: Filter out subjects with too few observations (e.g., less than 5) and insufficient variance in predictors
df_filtered <- df_filtered %>%
  group_by(user_id) %>%
  dplyr::filter(
    n() >= 5,                                         # Keep participants with at least 5 observations
    sd(CS, na.rm = TRUE) > 1e-6,                      # Keep participants with enough variance in CS
    sd(neg_aff_Moment, na.rm = TRUE) > 1e-6,          # Keep participants with enough variance in neg_aff_Moment
    sd(context_Moment, na.rm = TRUE) > 1e-6           # Keep participants with enough variance in context_Moment
  ) %>%
  ungroup()

# Step 4: Standardize predictors and UCS
df_filtered <- df_filtered %>%
  mutate(
    CS_scaled = scale(CS),
    neg_aff_Moment_scaled = scale(neg_aff_Moment),
    context_Moment_scaled = scale(context_Moment),
    UCS_scaled = scale(UCS),
    lag_UCS_scaled = scale(lag_UCS)
  )

df_filtered <- df_filtered %>%
  group_by(user_id) %>%
  mutate(
    UCS_scaled = scale(UCS),
    CS_scaled = scale(CS),
    neg_aff_Moment_scaled = scale(neg_aff_Moment),
    context_Moment_scaled = scale(context_Moment)
  ) %>%
  ungroup()

# Step 5: Prepare the data for Stan
stan_data <- list(
  N = nrow(df_filtered),  # Number of observations
  K = 3,  # Number of exogenous predictors: CS, neg_aff_Moment, context_Moment
  X = as.matrix(df_filtered[, c("CS", "neg_aff_Moment", "context_Moment")]),  # Exogenous predictors
  y = as.vector(df_filtered$UCS),  # Ensure y is a 1D vector
  lag_y = as.vector(df_filtered$lag_UCS)  # Lagged UCS for AR component, ensure it's also a vector
)


# Print a summary to ensure correctness
str(stan_data)


stan_file <- here::here(
  "02_idiographic_test", 
  "stan",
  "arimax_model.stan"
)

mod <- cmdstan_model(stan_file)

fit <- mod$variational(
  data = stan_data,
  seed = 42,
  init = function() list(
    beta = rep(0.01, stan_data$K),
    sigma = 0.01,
    alpha = 0.01,  # Initialize alpha (intercept)
    phi = 0.01     # Initialize phi (autoregressive term)
  )
)

fit <- mod$sample(
  data = stan_data,
  seed = 42,
  chains = 4,
  parallel_chains = 4,
  init = function() list(
    beta_raw = rep(0.01, stan_data$K),
    sigma = 0.01,
    alpha_raw = 0.01,  # Initialize alpha_raw
    phi_raw = 0.01     # Initialize phi_raw
  )
)



# Or, if you want summary statistics for all parameters
summary <- fit$summary()

# Print summary statistics for all parameters
print(summary)

# Extract draws for all parameters
draws <- fit$draws()

# Extract individual-level parameters
alpha_indiv <- draws[,"alpha"]
phi_indiv <- draws[,"phi"]

# You can also isolate beta if you want to analyze predictors
beta_indiv <- draws[,"beta"]

# For example, to calculate correlations between CS and UCS
# across individual participants or across occasions:
correlations <- sapply(1:n_participants, function(i) cor(CS[i, ], UCS[i, ]))

# Analyze the sign and strength of the correlations
summary(correlations)




library(posterior)

# Convert the draws to a matrix format
draws_matrix <- as_draws_matrix(draws)

# Check available parameter names
parameter_names <- colnames(draws_matrix)
print(parameter_names)

# Extract alpha, beta, and phi parameters
alpha_indiv <- draws_matrix[, "alpha"]
phi_indiv <- draws_matrix[, "phi"]
beta_indiv <- draws_matrix[, grep("beta\\[", parameter_names)]

# Inspect the first few rows of alpha_indiv
head(alpha_indiv)


# Summary of beta_indiv
summary(beta_indiv)

# Visualize the distribution of beta for each individual (if needed)
hist(beta_indiv[, 1], main = "Distribution of Beta[3]",
     xlab = "Beta[1]", ylab = "Frequency")



# Load necessary library
library(bayesplot)

# Extract beta coefficients
beta_1 <- draws_matrix[, "beta[1]"]  # Assuming this corresponds to negative affect
beta_2 <- draws_matrix[, "beta[2]"]  # Assuming this corresponds to context evaluation

# Summary statistics for beta[1] (negative affect)
summary_beta_1 <- summary(beta_1)
print(summary_beta_1)

# Plot the posterior distribution for beta[1] (negative affect)
mcmc_hist(beta_1, main = "Posterior of beta[1] (Negative Affect)")

# Check if the 95% credible interval is entirely positive
quantile(beta_1, probs = c(0.025, 0.975))

# Summary statistics for beta[2] (context evaluation)
summary_beta_2 <- summary(beta_2)
print(summary_beta_2)

# Plot the posterior distribution for beta[2] (context evaluation)
mcmc_hist(beta_2, main = "Posterior of beta[2] (Context Evaluation)")

# Check if the 95% credible interval is entirely negative
quantile(beta_2, probs = c(0.025, 0.975))


# Extract phi from the posterior
phi <- draws_matrix[, "phi"]

# Summary statistics for phi
summary_phi <- summary(phi)
print(summary_phi)

# Plot the posterior distribution of phi
mcmc_hist(phi, main = "Posterior of phi (Autoregressive Effect)")

# Check the 95% credible interval for phi
quantile(phi, probs = c(0.025, 0.975))

# Interpret whether phi is significantly positive (stability over time) or not


K <- 3
# Summary of all beta coefficients
for (i in 1:K) {
  beta_indiv <- draws_matrix[, paste0("beta[", i, "]")]
  
  cat("Summary of beta[", i, "]\n")
  print(summary(beta_indiv))
  
  # Plot the posterior for each beta
  mcmc_hist(beta_indiv, main = paste0("Posterior of beta[", i, "]"))
  
  # Check the 95% credible interval
  ci <- quantile(beta_indiv, probs = c(0.025, 0.975))
  cat("95% Credible Interval for beta[", i, "]: ", ci, "\n\n")
}


#-----------------------------------------

cor(df_filtered$UCS, df_filtered$CS)

#' UCS ~ student_t(nu, alpha +  gamma_CS * CS + 
#'  gamma_neg_aff * neg_aff_Moment +  gamma_context * context_Moment + 
#'   gamma_interaction * CS .* neg_aff_Moment +  // Interaction term
#'   phi * lag_CS_same_day, 
#'   sigma);
stan_file <- here::here(
  "02_idiographic_test", 
  "stan",
  "arimax_individual_interaction_model.stan"
)

mod <- cmdstan_model(stan_file)

data <- df_filtered
  
# Assuming you have the data for all participants
participants <- unique(data$user_id)


# Select the first 10 participants
subset_participants <- participants # participants[1:30]

for (participant in subset_participants) {
  # Subset data for the current participant
  participant_data <- data[data$user_id == participant, ]
  
  # Ensure lag_CS is computed within each day for CS
  participant_data <- participant_data %>%
    group_by(day) %>%
    mutate(lag_CS_same_day = lag(CS, default = NA))
  
  # Filter out rows where there is no valid lag (i.e., for the first row of each day)
  participant_data <- participant_data %>%
    filter(!is.na(lag_CS_same_day)) %>%
    filter(!is.na(neg_aff_Moment) & !is.na(context_Moment))  # Ensure no missing values in covariates
  
  # Create the Stan data list
  stan_data <- list(
    N = nrow(participant_data),
    CS = participant_data$CS,   # Predictor CS
    UCS = participant_data$UCS,   # Outcome variable UCS
    neg_aff_Moment = participant_data$neg_aff_Moment,  # Additional covariate 1
    context_Moment = participant_data$context_Moment,  # Additional covariate 2
    lag_CS_same_day = participant_data$lag_CS_same_day  # Lagged CS, only within the same day
  )
  
  # Check if there's enough data to fit the model for this participant
  if (nrow(participant_data) < 2) {
    cat("Skipping participant:", participant, "- not enough data\n")
    next
  }
  
  # Fit the Stan model
  fit <- mod$sample(
    data = stan_data,
    seed = 42,
    chains = 4,
    parallel_chains = 4,
    adapt_delta = 0.999,  # Tuning the sampler
    max_treedepth = 15
  )
  
  # Extract the draws
  draws_matrix <- fit$draws(format = "matrix")
  
  # Save the results for later analysis
  saveRDS(
    draws_matrix, 
    here::here(
      "02_idiographic_test", "rds", paste0("participant_", participant, "_draws.rds")
    )
  )
}


# beta_CS -----------------------------------------------------------------

# Initialize counters for the summary statistics
negative_assoc_count <- 0
positive_assoc_count <- 0
zero_assoc_count <- 0

for (participant in subset_participants) {
  # Load saved draws
  draws_matrix <- readRDS(
    here::here(
      "02_idiographic_test", "rds", paste0("participant_", participant, "_draws.rds")
    )
  )
  
  # Extract the slope coefficient for CS (gamma)
  beta_CS <- draws_matrix[, "gamma_CS"]
  
  # Calculate 50% (median) of the posterior distribution of gamma
  mean_beta_CS <- quantile(beta_CS, probs = c(0.5))
  
  # Check if the mean (or median) estimate of gamma is positive, negative, or zero
  if (mean_beta_CS > 0) {
    positive_assoc_count <- positive_assoc_count + 1
  } else if (mean_beta_CS < 0) {
    negative_assoc_count <- negative_assoc_count + 1
  } else {
    zero_assoc_count <- zero_assoc_count + 1
  }
}

# Print results
cat("Number of participants with negative association:", negative_assoc_count, "\n")
cat("Number of participants with positive association:", positive_assoc_count, "\n")
cat("Number of participants with zero association:", zero_assoc_count, "\n")


# beta_CS Credibility Intervals -------------------------------------------

# Initialize counters for the summary statistics
negative_assoc_count <- 0
positive_assoc_count <- 0
zero_assoc_count <- 0

for (participant in subset_participants) {
  # Load saved draws
  draws_matrix <- readRDS(
    here::here(
      "02_idiographic_test", "rds", paste0("participant_", participant, "_draws.rds")
    )
  )
  
  # Extract the slope coefficient for CS (gamma_CS)
  beta_CS <- draws_matrix[, "gamma_CS"]
  
  # Calculate the 89% credible interval for gamma_CS
  ci_beta_CS <- quantile(beta_CS, probs = c(0.055, 0.945))  # 89% CI
  
  # Check if the entire 89% credible interval is positive, negative, or includes zero
  if (ci_beta_CS[1] > 0) {
    positive_assoc_count <- positive_assoc_count + 1
  } else if (ci_beta_CS[2] < 0) {
    negative_assoc_count <- negative_assoc_count + 1
  } else {
    zero_assoc_count <- zero_assoc_count + 1
  }
}

# Print results
cat("Number of participants with negative association:", negative_assoc_count, "\n")
cat("Number of participants with positive association:", positive_assoc_count, "\n")
cat("Number of participants with zero association:", zero_assoc_count, "\n")


# beta_CS Negative Proportion ---------------------------------------------

# Create a vector to store the proportion of positive samples for each participant
negative_proportions <- c()

# Loop through the participants again to store the proportions
for (participant in subset_participants) {
  
  # Load saved draws for the participant
  draws_matrix <- readRDS(
    here::here(
      "02_idiographic_test", "rds", paste0("participant_", participant, "_draws.rds")
    )
  )
  
  # Extract the posterior samples for the slope of CS (gamma_CS)
  beta_CS <- draws_matrix[, "gamma_CS"]
  
  # Calculate the proportion of positive samples
  prop_negative <- mean(beta_CS < 0)
  
  # Store the proportion
  negative_proportions <- c(negative_proportions, prop_negative)
}

# Plot a histogram of the positive proportions
hist(negative_proportions, main = "Distribution of Negative Slopes Across Participants", 
     xlab = "Proportion of Positive Slopes", ylab = "Number of Participants")

mean(negative_proportions)
# [1] 0.7614572

median(negative_proportions)
# [1] 0.82


# Create data frame for family = binomial
n = length(negative_proportions)

beta_cs_df <- tibble(
  p = (negative_proportions * (n - 1) + 0.5) / n,
  id = 1:n
)

library(brms)

# Fit a hierarchical beta regression model
# fit_gamma_cs <- brm(
#   bf(p ~ 1 + (1 | id), family = "beta"),
#   data = beta_cs_df,  
#   prior = c(
#     prior(normal(0, 5), class = "Intercept"),  
#     prior(student_t(3, 0, 1), class = "sd")  
#   ),
#   iter = 8000, warmup = 2000, chains = 4, cores = 4,
#   backend = "cmdstanr",
#   control = list(adapt_delta = 0.999, max_treedepth = 20)  # Further increasing adapt_delta and max_treedepth
# )
# 
# pp_check(fit_gamma_cs)
# summary(fit_gamma_cs, prob = 0.89)

# The Beta family produces divergent transitions. I will use Binomial().

# Create data frame for family = binomial
beta_cs_df <- beta_cs_df %>%
  mutate(
    n_total = 4000,  # number of posterior draws for each participant
    n_negative = negative_proportions * n_total  
  )
head(beta_cs_df)
# p    id n_total n_negative
# <dbl> <int>   <dbl>      <dbl>
# 1 0.797     1    4000       3192
# 2 0.901     2    4000       3608
# 3 0.852     3    4000       3410
# 4 0.812     4    4000       3251
# 5 0.946     5    4000       3787
# 6 0.865     6    4000       3465

beta_cs_df$n_negative <- as.integer(beta_cs_df$n_negative)

fit_gamma_cs_binom <- brm(
  bf(n_negative | trials(n_total) ~ 1 + (1 | id), family = "binomial"),
  data = beta_cs_df,
  prior = c(prior(normal(0, 0.5), class = Intercept),
            prior(normal(0, 0.5), class = sd)
            ), 
  iter = 100000, warmup = 2000, chains = 4, cores = 4,
  backend = "cmdstanr",
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)

fit_gamma_cs_binom |> 
  pp_check()

ggsave(
  here::here(
    "02_idiographic_test", "plots", "ppcheck_idiographic_gamma_cs.png"
  )
)

summary(fit_gamma_cs_binom, prob = 0.89)
# Family: binomial 
# Links: mu = logit 
# Formula: n_negative | trials(n_total) ~ 1 + (1 | id) 
# Data: beta_cs_df (Number of observations: 485) 
# Draws: 4 chains, each with iter = 1e+05; warmup = 2000; thin = 1;
# total post-warmup draws = 392000
# 
# Multilevel Hyperparameters:
#   ~id (Number of levels: 485) 
# Estimate Est.Error l-89% CI u-89% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     1.25      0.04     1.19     1.32 1.00     2588     5142
# 
# Regression Coefficients:
#   Estimate Est.Error l-89% CI u-89% CI Rhat Bulk_ESS Tail_ESS
# Intercept     1.45      0.06     1.36     1.54 1.01      748     1196
# 
# Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

b_logit <- c(1.45, 1.36, 1.54)
proportion <- 1 / (1 + exp(-b_logit))
round(proportion, 3)
# [1] 0.810 0.796 0.823

#' To assess the heterogeneity of the effects in your case, you can compute 
#' and interpret the variance of the random intercepts in your model, 
#' specifically the standard deviation of the intercepts (sd(Intercept)). 
#' This value reflects how much individual participants differ from the overall 
#' average proportion (i.e., the heterogeneity across participants).
#' 
#' Step-by-step:
#' Variance of the random intercepts: The sd(Intercept) in the model represents 
#' the variability of the individual participant-level effects. This provides a 
#' measure of heterogeneity across participants.
#' From your output: sd(Intercept) = 1.22 (logit scale) 
#' Interpretation on the logit scale: The variability on the logit scale 
#' indicates how much the individual effects deviate from the group-level mean. 
#' To interpret heterogeneity in the same probability (proportion) scale as your 
#' aggregate effect, you can transform the logit-based variability to the 
#' proportion scale using the following steps.
#' 
#' Converting logit SD to the proportion scale: While transforming individual 
#' means is straightforward (as you did for the intercept), the variability 
#' (i.e., the standard deviation) requires a different approach. You can 
#' simulate this transformation to get a sense of the heterogeneity in the 
#' proportion space.
#' Here’s an approach to simulate the distribution and assess heterogeneity in 
#' the proportion scale:
#' Simulating the Variability in Proportion Space: You can simulate the random 
#' effects to understand how much the intercepts vary across individuals. For 
#' this, we can simulate from a normal distribution with mean = 1.46 (the 
#' group-level intercept) and standard deviation = 1.22 (the estimated 
#' sd(Intercept)), and then convert these values back to proportions.

# Simulating 1000 individual intercepts on the logit scale
set.seed(42)
sim_intercepts <- rnorm(10000, mean = 1.45, sd = 1.25)

# Convert from logit to proportion scale
sim_proportions <- 1 / (1 + exp(-sim_intercepts))

# Summary of simulated proportions
summary(sim_proportions)

# Plotting the distribution of the proportions to visualize heterogeneity
hist(sim_proportions, breaks = 30, main = "Simulated Individual Proportions", xlab = "Proportion", col = "lightblue")

# Calculate standard deviation of the simulated proportions
sd(sim_proportions)
# [1] 0.1964286

sd(negative_proportions)
# [1] 0.1919265

#' FOR THE PAPER:
#' The aggregate effect of the idiographic analysis was a mean proportion of 
#' 0.812 (89% CI [0.797, 0.823]), with a standard deviation of 0.192, 
#' reflecting considerable heterogeneity in the individual-level effects.


# slope for neg_aff_Moment -----------------------------------------------------

# Create a vector to store the proportion of positive samples for each participant
post_means_neg_aff <- c()

# Loop through the participants again to store the proportions
for (participant in subset_participants) {
  
  # Load saved draws for the participant
  draws_matrix <- readRDS(
    here::here(
      "02_idiographic_test", "rds", paste0("participant_", participant, "_draws.rds")
    )
  )
  
  # Extract the posterior samples for the slope of CS (gamma_CS)
  beta_neg_aff <- draws_matrix[, "gamma_neg_aff"]
  
  # Calculate the proportion of positive samples
  avg_beta_neg_aff <- mean(beta_neg_aff)
  
  # Store the proportion
  post_means_neg_aff <- c(post_means_neg_aff, avg_beta_neg_aff)
}

# Plot a histogram of the positive proportions
hist(post_means_neg_aff, main = "Distribution of Negative Affect Slopes Across Participants", 
     xlab = "Posterior Mean of Negative Affect Slopes", ylab = "Number of Participants")

mean(post_means_neg_aff)
# [1] 0.4144562

# brm() analysis

gamma_neg_aff_df <- tibble(
  mean_gamma_neg_aff = post_means_neg_aff,
  id = 1:length(post_means_neg_aff)
)

fit_gamma_neg_aff <- brm(
  bf(mean_gamma_neg_aff ~ 1 + (1 | id)),
  family = student(),
  data = gamma_neg_aff_df,  
  prior = c(
    prior(normal(0, 0.25), class = "Intercept"),  
    prior(normal(0, 0.25), class = "sd")         
  ),
  iter = 15000, warmup = 5000, chains = 4, cores = 4,  
  backend = "cmdstanr",
  control = list(adapt_delta = 0.9999, max_treedepth = 20)  
)

fit_gamma_neg_aff |> 
  pp_check()

ggsave(
  here::here(
    "02_idiographic_test", "plots", "ppcheck_idiographic_gamma_neg_aff.png"
  )
)

summary(fit_gamma_neg_aff, prob = 0.89)
# Family: student 
# Links: mu = identity; sigma = identity; nu = identity 
# Formula: mean_gamma_neg_aff ~ 1 + (1 | id) 
# Data: gamma_neg_aff_df (Number of observations: 485) 
# Draws: 4 chains, each with iter = 15000; warmup = 5000; thin = 1;
# total post-warmup draws = 40000
# 
# Multilevel Hyperparameters:
#   ~id (Number of levels: 485) 
# Estimate Est.Error l-89% CI u-89% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.19      0.13     0.02     0.44 1.02      289      103
# 
# Regression Coefficients:
#   Estimate Est.Error l-89% CI u-89% CI Rhat Bulk_ESS Tail_ESS
# Intercept     0.38      0.02     0.34     0.42 1.00    22811    27075
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-89% CI u-89% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.44      0.08     0.26     0.52 1.02      265       90
# nu       19.94     12.00     7.03    42.50 1.00    11345     5575
# 
# Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).


# slope for context_Moment -----------------------------------------------------

# Create a vector to store the proportion of positive samples for each participant
post_means_context <- c()

# Loop through the participants again to store the proportions
for (participant in subset_participants) {
  
  # Load saved draws for the participant
  draws_matrix <- readRDS(
    here::here(
      "02_idiographic_test", "rds", paste0("participant_", participant, "_draws.rds")
    )
  )
  
  # Extract the posterior samples for the slope of CS (gamma_CS)
  beta_context <- draws_matrix[, "gamma_context"]
  
  # Calculate the posterior mean
  avg_beta_context <- mean(beta_context)
  
  # Store the posterior means
  post_means_context <- c(post_means_context, avg_beta_context)
}

# Plot a histogram of the positive proportions
hist(post_means_context, main = "Distribution of Context Slopes Across Participants", 
     xlab = "Posterior Mean of Context Slopes", ylab = "Number of Participants")

mean(post_means_context)

# brm() analysis

gamma_context_df <- tibble(
  mean_gamma_context = post_means_context,
  id = 1:length(post_means_context)
)

fit_gamma_context <- brm(
  bf(mean_gamma_context ~ 1 + (1 | id)),
  family = student(),
  data = gamma_context_df,  
  prior = c(
    prior(normal(0, 0.5), class = "Intercept"),
    prior(normal(0, 2), class = "sd")  # Random effects for participants
  ),
  iter = 10000, warmup = 2000, chains = 4, cores = 4,
  backend = "cmdstanr",
  control = list(adapt_delta = 0.999, max_treedepth = 20)
)

fit_gamma_context |> 
  pp_check()

ggsave(
  here::here(
    "02_idiographic_test", "plots", "ppcheck_idiographic_gamma_context.png"
  )
)


summary(fit_gamma_context, prob =.89)
# Family: student 
# Links: mu = identity; sigma = identity; nu = identity 
# Formula: mean_gamma_context ~ 1 + (1 | id) 
# Data: gamma_context_df (Number of observations: 485) 
# Draws: 4 chains, each with iter = 10000; warmup = 2000; thin = 1;
# total post-warmup draws = 32000
# 
# Multilevel Hyperparameters:
#   ~id (Number of levels: 485) 
# Estimate Est.Error l-89% CI u-89% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.17      0.11     0.02     0.35 1.02      224      237
# 
# Regression Coefficients:
#   Estimate Est.Error l-89% CI u-89% CI Rhat Bulk_ESS Tail_ESS
# Intercept    -0.04      0.02    -0.07    -0.01 1.00    22842    21437
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-89% CI u-89% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.32      0.07     0.19     0.39 1.02      234      215
# nu       17.75     11.28     5.76    38.62 1.00     2614     1142
# 
# Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

# slope for Interaction Coefficient ---------------------------------------

# Create a vector to store the proportion of positive samples for each participant
post_means_interaction <- c()

# Loop through the participants again to store the proportions
for (participant in subset_participants) {
  
  # Load saved draws for the participant
  draws_matrix <- readRDS(
    here::here(
      "02_idiographic_test", "rds", paste0("participant_", participant, "_draws.rds")
    )
  )
  
  # Extract the posterior samples for the slope of CS (gamma_CS)
  beta_neg_aff <- draws_matrix[, "gamma_interaction"]
  
  # Calculate the proportion of positive samples
  avg_beta_interaction <- mean(beta_neg_aff)
  
  # Store the proportion
  post_means_interaction <- c(post_means_interaction, avg_beta_interaction)
}

# Plot a histogram of the positive proportions
hist(post_means_interaction, main = "Distribution of Interaction Coefficient Across Participants", 
     xlab = "Posterior Mean of Interaction Coefficient", ylab = "Number of Participants")

mean(post_means_interaction)
# [1]  0.004095943

# brm() analysis

gamma_interaction_df <- tibble(
  mean_gamma_interaction = post_means_interaction,
  id = 1:length(post_means_interaction)
)

fit_gamma_interaction <- brm(
  bf(mean_gamma_interaction ~ 1 + (1 | id)),
  family = student(),
  data = gamma_interaction_df,  
  prior = c(
    prior(normal(0, 0.25), class = "Intercept"),  
    prior(normal(0, 0.25), class = "sd")         
  ),
  iter = 15000, warmup = 5000, chains = 4, cores = 4,  
  backend = "cmdstanr",
  control = list(adapt_delta = 0.9999, max_treedepth = 20)  
)

fit_gamma_interaction |> 
  pp_check() + xlim(-1, 1)

ggsave(
  here::here(
    "02_idiographic_test", "plots", "ppcheck_idiographic_gamma_interaction.png"
  )
)

fit_gamma_interaction |> 
  summary(prob = 0.89)
# Family: student 
# Links: mu = identity; sigma = identity; nu = identity 
# Formula: mean_gamma_interaction ~ 1 + (1 | id) 
# Data: gamma_interaction_df (Number of observations: 485) 
# Draws: 4 chains, each with iter = 15000; warmup = 5000; thin = 1;
# total post-warmup draws = 40000
# 
# Multilevel Hyperparameters:
#   ~id (Number of levels: 485) 
# Estimate Est.Error l-89% CI u-89% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.03      0.02     0.00     0.06 1.00     5356     7221
# 
# Regression Coefficients:
#   Estimate Est.Error l-89% CI u-89% CI Rhat Bulk_ESS Tail_ESS
# Intercept    -0.01      0.01    -0.03     0.00 1.00    78153    30316
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-89% CI u-89% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.13      0.01     0.12     0.15 1.00    12881    11000
# nu        2.21      0.31     1.76     2.74 1.00    26071    22144
# 
# Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

