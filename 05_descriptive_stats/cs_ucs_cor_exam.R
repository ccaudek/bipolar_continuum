# Overview ----------------------------------------------------------------
# Associated project: EMA Self-Compassion Mindfulness paper
# Script purpose: Compute within-person CS/UCS correlation in three
#   time periods: pre-exam, post-exam, no-exam.
#
# Written by: Corrado Caudek (corrado.caudek@unifi.it)
# Version: 2025-01-15
# Last update:
# Status: In progress
# Notes:


# Load necessary libraries ------------------------------------------------

if (!requireNamespace("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, brms, here, rio, performance
)

# Study 1 - Import data ---------------------------------------------------

alldata1 <- rio::import(
  here::here(
    "data", "piel_data", "public_study_1_data.csv"
  )
)

# Standardization of SC and USC
alldata1$SC <- scale(
  alldata1$scs_pos_7 + alldata1$scs_pos_3 +
    alldata1$scs_pos_1 + alldata1$scs_pos_6
) |> as.numeric()

alldata1$USC <- scale(
  alldata1$scs_neg_8 + alldata1$scs_neg_5 +
    alldata1$scs_neg_2 + alldata1$scs_neg_4
) |> as.numeric()


# Outliers detection ------------------------------------------------------

d1 <- alldata1 |>
  dplyr::select(SC, USC, user_id, day, time_window) |>
  drop_na()

outliers <- check_outliers(d1, method = "mcd", verbose = FALSE)
outliers

d_clean <- d1[-which(outliers), ]
# The analysis after removing outliers produces the same coclusion as the
# analysis of the complete data set. Therefore, the complete data set will
# be used.


# Joint Model for All Periods ---------------------------------------------

# Fit the multilevel model
f_joint <- brm(
  data = d1,
  family = student,
  bf(
    mvbind(SC, USC) ~ 1 + # Include intercepts
      (1 | user_id) +
      (1 | user_id:day) +
      (1 | user_id:day:time_window)
  ) +
    set_rescor(TRUE),
  iter = 8000,
  warmup = 2000,
  chains = 4,
  cores = 8,
  seed = 210191,
  # control = list(adapt_delta = 0.95),
  algorithm = "meanfield",
  backend = "cmdstanr"
)

# Model Diagnostics
print(summary(f_joint)) # Check convergence (R-hat should be close to 1)
pp_check(f_joint, resp = "SC") + xlim(-5, 5) # Posterior predictive checks
pp_check(f_joint, resp = "USC") + xlim(-5, 5) # Posterior predictive checks

# Function to compute a single SC-USC correlation with 89% credible intervals
extract_overall_correlation <- function(model) {
  # Extract posterior draws of residual correlations
  draws <- as_draws_df(model)
  rescor_cols <- grep("^rescor__SC__USC", names(draws), value = TRUE)

  if (length(rescor_cols) == 0) {
    stop("Residual correlations not found in the model. Ensure 'set_rescor(TRUE)' was used.")
  }

  # Extract posterior samples for the SC-USC correlation
  rescor_samples <- draws[[rescor_cols]]

  if (is.null(rescor_samples)) {
    stop("No residual correlation samples found in the model.")
  }

  # Calculate median and 89% credible intervals
  overall_correlation <- median(rescor_samples, na.rm = TRUE)
  ci_lower <- quantile(rescor_samples, 0.055, na.rm = TRUE) # 89% lower bound
  ci_upper <- quantile(rescor_samples, 0.945, na.rm = TRUE) # 89% upper bound

  # Return results in a clean data frame
  results <- data.frame(
    correlation = overall_correlation,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )

  return(results)
}

# Extract and print results
results <- extract_overall_correlation(f_joint)

# Print formatted results
cat("\nOverall within-person SC-USC correlation:\n")
print(knitr::kable(results, digits = 3))

# Correlation plot
p1 <- ggplot(results$correlations, aes(x = period, y = correlation)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(
    title = "Within-person SC-USC correlations across exam periods",
    y = "Correlation",
    x = "Exam period"
  )

# Differences plot
p2 <- ggplot(results$differences, aes(x = comparison, y = difference)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Differences in SC-USC correlations between exam periods",
    y = "Correlation difference",
    x = "Comparison"
  )

# Print both plots
print(p1)
print(p2)



# Study 2 - Import data ---------------------------------------------------

alldata <- rio::import(
  here::here(
    "data", "mpath_data", "public_study_2_data.csv"
  )
)

table(alldata$exam_day)
# no_exam    post     pre
#    6221     284     274

# Standardization of SC and USC
alldata$SC <- scale(
  alldata$scs_pos_7 + alldata$scs_pos_3 +
    alldata$scs_pos_1 + alldata$scs_pos_6
) |> as.numeric()

alldata$USC <- scale(
  alldata$scs_neg_8 + alldata$scs_neg_5 +
    alldata$scs_neg_2 + alldata$scs_neg_4
) |> as.numeric()

# Ensure exam_day is a factor
alldata$exam_day <- factor(alldata$exam_day)


# Outliers detection ------------------------------------------------------

d <- alldata |>
  dplyr::select(SC, USC, user_id, exam_day, day, time_window)

outliers <- check_outliers(d, method = "mcd", verbose = FALSE)
outliers

d_clean <- d[-which(outliers), ]
# The analysis after removing outliers produces the same coclusion as the
# analysis of the complete data set. Therefore, the complete data set will
# be used.


# Joint Model for All Periods ---------------------------------------------

# Fit the multilevel model
f_joint <- brm(
  data = alldata,
  family = student,
  bf(
    mvbind(SC, USC) ~ 0 + exam_day +
      (1 + exam_day | user_id) +
      (1 | user_id:day) +
      (1 | user_id:day:time_window),
    sigma ~ 0 + exam_day
  ) +
    set_rescor(TRUE),
  iter = 8000,
  warmup = 2000,
  chains = 4,
  cores = 8,
  seed = 210191,
  algorithm = "meanfield",
  # control = list(adapt_delta = 0.95),  # Increase adaptation delta for better convergence
  backend = "cmdstanr"
)

# Model Diagnostics
print(summary(f_joint)) # Check convergence (R-hat should be close to 1)
pp_check(f_joint, resp = "SC") + xlim(-5, 5) # Posterior predictive checks
pp_check(f_joint, resp = "USC") + xlim(-5, 5) # Posterior predictive checks


# Function to extract period-specific correlations
extract_period_correlations <- function(model) {
  # Get posterior draws using the newer method
  draws <- as_draws_df(model)

  # Get the exam day levels
  exam_days <- levels(model$data$exam_day)

  # Extract the residual correlation parameter
  rescor_cols <- grep("^rescor__SC__USC", names(draws), value = TRUE)
  rescor <- draws[[rescor_cols]]

  if (is.null(rescor)) {
    stop("Could not find residual correlations in the model. Make sure set_rescor(TRUE) was used in the model.")
  }

  # Initialize results dataframes
  cors <- data.frame(
    period = exam_days,
    correlation = NA,
    ci_lower = NA,
    ci_upper = NA
  )

  # Get fitted values for each exam day
  fitted_vals <- fitted(model, summary = FALSE)
  n_samples <- dim(fitted_vals)[1]

  # Calculate correlations for each exam day
  period_cors <- matrix(NA, nrow = n_samples, ncol = length(exam_days))

  for (i in seq_along(exam_days)) {
    day_data <- model$data$exam_day == exam_days[i]
    if (sum(day_data) > 0) { # Check if we have data for this exam day
      sc_vals <- fitted_vals[, day_data, 1] # SC values
      usc_vals <- fitted_vals[, day_data, 2] # USC values

      # Calculate correlation for each posterior sample
      period_cors[, i] <- sapply(1:n_samples, function(s) {
        cor(sc_vals[s, ], usc_vals[s, ])
      })
    }
  }

  # Update cors dataframe with period-specific correlations
  for (i in seq_along(exam_days)) {
    if (!all(is.na(period_cors[, i]))) {
      cors$correlation[i] <- median(period_cors[, i], na.rm = TRUE)
      cors$ci_lower[i] <- quantile(period_cors[, i], 0.025, na.rm = TRUE)
      cors$ci_upper[i] <- quantile(period_cors[, i], 0.975, na.rm = TRUE)
    }
  }

  # Calculate differences
  comparisons <- combn(exam_days, 2, simplify = FALSE)
  diffs <- data.frame(
    comparison = sapply(comparisons, function(x) paste(x[1], "vs", x[2])),
    difference = NA,
    ci_lower = NA,
    ci_upper = NA
  )

  for (i in seq_along(comparisons)) {
    pair <- comparisons[[i]]
    idx1 <- which(exam_days == pair[1])
    idx2 <- which(exam_days == pair[2])

    diff_samples <- period_cors[, idx1] - period_cors[, idx2]
    diffs$difference[i] <- median(diff_samples, na.rm = TRUE)
    diffs$ci_lower[i] <- quantile(diff_samples, 0.025, na.rm = TRUE)
    diffs$ci_upper[i] <- quantile(diff_samples, 0.975, na.rm = TRUE)
  }

  return(list(correlations = cors, differences = diffs))
}

# Extract and print results
results <- extract_period_correlations(f_joint)

# Print formatted results
cat("\nWithin-person correlations by exam period:\n")
print(knitr::kable(results$correlations, digits = 3))
# |period  | correlation| ci_lower| ci_upper|
# |:-------|-----------:|--------:|--------:|
# |no_exam |      -0.688|   -0.704|   -0.671|
# |post    |      -0.682|   -0.716|   -0.649|
# |pre     |      -0.668|   -0.701|   -0.635|

cat("\nDifferences between periods:\n")
print(knitr::kable(results$differences, digits = 3))
# |comparison      | difference| ci_lower| ci_upper|
# |:---------------|----------:|--------:|--------:|
# |no_exam vs post |     -0.006|   -0.037|    0.027|
# |no_exam vs pre  |     -0.019|   -0.052|    0.012|
# |post vs pre     |     -0.014|   -0.056|    0.029|


# Correlation plot
p1 <- ggplot(results$correlations, aes(x = period, y = correlation)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(
    title = "Within-person SC-USC correlations across exam periods",
    y = "Correlation",
    x = "Exam period"
  )

# Differences plot
p2 <- ggplot(results$differences, aes(x = comparison, y = difference)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Differences in SC-USC correlations between exam periods",
    y = "Correlation difference",
    x = "Comparison"
  )

# Print both plots
print(p1)
print(p2)



cor(alldata[alldata$exam_day == "pre", ]$SC, alldata[alldata$exam_day == "pre", ]$USC)
cor(alldata[alldata$exam_day == "post", ]$SC, alldata[alldata$exam_day == "post", ]$USC)
cor(alldata[alldata$exam_day == "no_exam", ]$SC, alldata[alldata$exam_day == "no_exam", ]$USC)










# Extract posterior predictions
posterior_predictions <- posterior_epred(f_joint, newdata = alldata, re_formula = NULL)

# Check the structure of posterior_predictions
dim(posterior_predictions)

# Convert 3D array to long format
# Convert the 3D array into a data frame
posterior_long <- as.data.frame.table(posterior_predictions, responseName = "predicted_value") %>%
  rename(
    iteration = Var1,
    observation = Var2,
    response = Var3
  ) %>%
  mutate(
    iteration = as.integer(iteration),
    observation = as.integer(observation),
    response = ifelse(response == 1, "SC", "USC") # Map response index to names
  )

# Add an index column to alldata
alldata <- alldata %>%
  mutate(index = row_number())

# Join posterior predictions with the original data
long_data <- posterior_long %>%
  left_join(alldata, by = c("observation" = "index"))


# Calculate correlations for each group
occasion_correlations <- long_data %>%
  group_by(user_id, day, time_window, iteration) %>%
  summarize(
    # Filter SC and USC predictions
    correlation = {
      sc_values <- predicted_value[response == "SC"]
      usc_values <- predicted_value[response == "USC"]
      if (length(sc_values) > 1 && length(usc_values) > 1) {
        cor(sc_values, usc_values)
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  )

# Summarize correlations across iterations
correlation_summary <- occasion_correlations %>%
  group_by(user_id, day, time_window) %>%
  summarize(
    median_correlation = median(correlation, na.rm = TRUE),
    ci_lower = quantile(correlation, 0.055, na.rm = TRUE),
    ci_upper = quantile(correlation, 0.945, na.rm = TRUE),
    .groups = "drop"
  )

# View the correlation summary
print(correlation_summary)



# eof -----------------












# Analysis for exam_day == "pre" ------------------------------------------

# This is the moment with the higest emotional activation.
# According to H3, the polarization between the CS and UCS components should be
# the greatest. According to H3, we expect the largest within-person CS/UCS
# correlation

only_exam_day <- alldata |>
  dplyr::filter(exam_day == "pre")

only_exam_day$SC <- scale(
  only_exam_day$scs_pos_7 + only_exam_day$scs_pos_3 +
    only_exam_day$scs_pos_1 + only_exam_day$scs_pos_6
) |> as.numeric()

only_exam_day$USC <- scale(
  only_exam_day$scs_neg_8 + only_exam_day$scs_neg_5 +
    only_exam_day$scs_neg_2 + only_exam_day$scs_neg_4
) |> as.numeric()

length(unique(only_exam_day$user_id))

cor(only_exam_day$SC, only_exam_day$USC)

hist(only_exam_day$SC)

d <- only_exam_day |>
  dplyr::select(SC, USC, user_id, day)

outliers <- check_outliers(d, method = "mcd", verbose = FALSE)
outliers

d_clean <- d[-which(outliers), ]

f1 <-
  brm(
    data = d_clean,
    family = student,
    bf(mvbind(SC, USC) ~ 1 + (1 | user_id) + (1 | user_id:day)) + set_rescor(TRUE),
    iter = 8000,
    warmup = 2000,
    chains = 4,
    cores = 4,
    seed = 210191,
    backend = "cmdstanr",
    adapt_delta = 0.99
  )

summary(f1)
# Residual Correlations:
#                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# rescor(SC,USC)    -0.77      0.05    -0.85    -0.67 1.00     4628     6825

pp_check(f1)

# Function to extract correlations
extract_correlations <- function(model) {
  # Extract posterior samples using updated syntax
  posts <- as_draws_df(model)

  # Extract residual (within-timewindow) correlation
  within_corr <- median(posts$rescor__SC__USC)
  within_ci <- quantile(posts$rescor__SC__USC, probs = c(0.055, 0.945))

  # Extract random effects correlations
  # For user level
  user_var_usc <- posts$sd_user_id__USC_Intercept^2
  user_var_sc <- posts$sd_user_id__SC_Intercept^2

  # For day:user_id level
  day_var_usc <- posts$`sd_user_id:day__USC_Intercept`^2
  day_var_sc <- posts$`sd_user_id:day__SC_Intercept`^2

  # Calculate ICC for each variable
  total_var_usc <- user_var_usc + day_var_usc + posts$sigma_USC^2
  total_var_sc <- user_var_sc + day_var_sc + posts$sigma_SC^2

  icc_user_usc <- user_var_usc / total_var_usc
  icc_user_sc <- user_var_sc / total_var_sc
  icc_day_usc <- day_var_usc / total_var_usc
  icc_day_sc <- day_var_sc / total_var_sc

  # Calculate median ICCs
  results <- list(
    within = list(
      correlation = within_corr,
      ci_lower = within_ci[1],
      ci_upper = within_ci[2]
    ),
    icc = list(
      user = list(
        USC = median(icc_user_usc),
        SC = median(icc_user_sc),
        USC_ci = quantile(icc_user_usc, probs = c(0.055, 0.945)),
        SC_ci = quantile(icc_user_sc, probs = c(0.055, 0.945))
      ),
      day = list(
        USC = median(icc_day_usc),
        SC = median(icc_day_sc),
        USC_ci = quantile(icc_day_usc, probs = c(0.055, 0.945)),
        SC_ci = quantile(icc_day_sc, probs = c(0.055, 0.945))
      )
    )
  )

  return(results)
}

# Extract results
results <- extract_correlations(f1)

# Create summary tables
correlation_table <- data.frame(
  Level = "Within-person",
  Correlation = results$within$correlation,
  CI_lower = results$within$ci_lower,
  CI_upper = results$within$ci_upper
)

icc_table <- data.frame(
  Level = c("Between-users", "Between-days"),
  Variable = rep(c("USC", "SC"), each = 2),
  ICC = c(
    results$icc$user$USC,
    results$icc$user$SC,
    results$icc$day$USC,
    results$icc$day$SC
  ),
  CI_lower = c(
    results$icc$user$USC_ci[1],
    results$icc$user$SC_ci[1],
    results$icc$day$USC_ci[1],
    results$icc$day$SC_ci[1]
  ),
  CI_upper = c(
    results$icc$user$USC_ci[2],
    results$icc$user$SC_ci[2],
    results$icc$day$USC_ci[2],
    results$icc$day$SC_ci[2]
  )
)

# Print formatted results
cat("\nCorrelation between USC and SC:\n")
print(knitr::kable(correlation_table, digits = 3))
# |     |Level         | Correlation| CI_lower| CI_upper|
# |:----|:-------------|-----------:|--------:|--------:|
# |5.5% |Within-person |      -0.776|   -0.836|   -0.691|

cat("\nIntraclass Correlation Coefficients (ICC):\n")
print(knitr::kable(icc_table, digits = 3))
# |Level         |Variable |   ICC| CI_lower| CI_upper|
# |:-------------|:--------|-----:|--------:|--------:|
# |Between-users |USC      | 0.263|    0.114|    0.452|
# |Between-days  |USC      | 0.281|    0.136|    0.467|
# |Between-users |SC       | 0.015|    0.000|    0.096|
# |Between-days  |SC       | 0.014|    0.000|    0.089|


# No exam day -------------------------------------------------------------

no_exam_day <- alldata |>
  dplyr::filter(exam_day == "no_exam")

no_exam_day$SC <- scale(
  no_exam_day$scs_pos_7 + no_exam_day$scs_pos_3 +
    no_exam_day$scs_pos_1 + no_exam_day$scs_pos_6
) |> as.numeric()

no_exam_day$USC <- scale(
  no_exam_day$scs_neg_8 + no_exam_day$scs_neg_5 +
    no_exam_day$scs_neg_2 + no_exam_day$scs_neg_4
) |> as.numeric()


d1_no_exam <- no_exam_day |>
  dplyr::select(SC, USC, user_id, day, time_window)

outliers <- check_outliers(d1_no_exam, method = "mcd", verbose = FALSE)
outliers

d1_no_exam_clean <- d1_no_exam[-which(outliers), ]

f2 <-
  brm(
    data = d1_no_exam_clean,
    family = student,
    bf(mvbind(SC, USC) ~ 1 +
      (1 | user_id) +
      (1 | user_id:day) +
      (1 | user_id:day:time_window)) +
      set_rescor(TRUE),
    iter = 4000,
    warmup = 2000,
    chains = 4,
    cores = 8,
    seed = 210191,
    backend = "cmdstanr"
  )

summary(f2)
# Residual Correlations:
#                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# rescor(SC,USC)    -0.43      0.02    -0.46    -0.39 1.00     2744     3511

pp_check(f2, "SC")




# Updated function to extract correlations
extract_correlations <- function(model) {
  # Extract posterior samples using updated syntax
  posts <- as_draws_df(model)

  # Extract residual (within-timewindow) correlation
  within_corr <- median(posts$rescor__SC__USC)
  within_ci <- quantile(posts$rescor__SC__USC, probs = c(0.055, 0.945))

  # Extract random effects variances
  # For user level
  user_var_usc <- posts$sd_user_id__USC_Intercept^2
  user_var_sc <- posts$sd_user_id__SC_Intercept^2

  # For day:user_id level
  day_var_usc <- posts$`sd_user_id:day__USC_Intercept`^2
  day_var_sc <- posts$`sd_user_id:day__SC_Intercept`^2

  # For time_window:day:user_id level
  time_var_usc <- posts$`sd_user_id:day:time_window__USC_Intercept`^2
  time_var_sc <- posts$`sd_user_id:day:time_window__SC_Intercept`^2

  # Calculate total variance for each variable
  total_var_usc <- user_var_usc + day_var_usc + time_var_usc + posts$sigma_USC^2
  total_var_sc <- user_var_sc + day_var_sc + time_var_sc + posts$sigma_SC^2

  # Calculate ICCs for each level
  icc_user_usc <- user_var_usc / total_var_usc
  icc_user_sc <- user_var_sc / total_var_sc
  icc_day_usc <- day_var_usc / total_var_usc
  icc_day_sc <- day_var_sc / total_var_sc
  icc_time_usc <- time_var_usc / total_var_usc
  icc_time_sc <- time_var_sc / total_var_sc

  # Calculate median ICCs
  results <- list(
    within = list(
      correlation = within_corr,
      ci_lower = within_ci[1],
      ci_upper = within_ci[2]
    ),
    icc = list(
      user = list(
        USC = median(icc_user_usc),
        SC = median(icc_user_sc),
        USC_ci = quantile(icc_user_usc, probs = c(0.055, 0.945)),
        SC_ci = quantile(icc_user_sc, probs = c(0.055, 0.945))
      ),
      day = list(
        USC = median(icc_day_usc),
        SC = median(icc_day_sc),
        USC_ci = quantile(icc_day_usc, probs = c(0.055, 0.945)),
        SC_ci = quantile(icc_day_sc, probs = c(0.055, 0.945))
      ),
      time_window = list(
        USC = median(icc_time_usc),
        SC = median(icc_time_sc),
        USC_ci = quantile(icc_time_usc, probs = c(0.055, 0.945)),
        SC_ci = quantile(icc_time_sc, probs = c(0.055, 0.945))
      )
    )
  )

  return(results)
}

# Extract results
results <- extract_correlations(f2)

# Print formatted results
cat("\nCorrelation between USC and SC:\n")
print(knitr::kable(correlation_table, digits = 3))
# |     |Level         | Correlation| CI_lower| CI_upper|
# |:----|:-------------|-----------:|--------:|--------:|
# |5.5% |Within-person |      -0.776|   -0.836|   -0.691|

icc_table <- data.frame(
  Level = c("Between-users", "Between-days", "Between-time_windows"),
  Variable = rep(c("USC", "SC"), each = 3),
  ICC = c(
    results$icc$user$USC,
    results$icc$user$SC,
    results$icc$day$USC,
    results$icc$day$SC,
    results$icc$time_window$USC,
    results$icc$time_window$SC
  ),
  CI_lower = c(
    results$icc$user$USC_ci[1],
    results$icc$user$SC_ci[1],
    results$icc$day$USC_ci[1],
    results$icc$day$SC_ci[1],
    results$icc$time_window$USC_ci[1],
    results$icc$time_window$SC_ci[1]
  ),
  CI_upper = c(
    results$icc$user$USC_ci[2],
    results$icc$user$SC_ci[2],
    results$icc$day$USC_ci[2],
    results$icc$day$SC_ci[2],
    results$icc$time_window$USC_ci[2],
    results$icc$time_window$SC_ci[2]
  )
)

cat("\nIntraclass Correlation Coefficients (ICC):\n")
print(knitr::kable(icc_table, digits = 3))
# |Level                |Variable |   ICC| CI_lower| CI_upper|
# |:--------------------|:--------|-----:|--------:|--------:|
# |Between-users        |USC      | 0.722|    0.682|    0.759|
# |Between-days         |USC      | 0.707|    0.665|    0.745|
# |Between-time_windows |USC      | 0.169|    0.144|    0.196|
# |Between-users        |SC       | 0.165|    0.141|    0.192|
# |Between-days         |SC       | 0.001|    0.000|    0.005|
# |Between-time_windows |SC       | 0.000|    0.000|    0.001|


# Analysis for exam_day == "post" ------------------------------------------

exam_day_post <- alldata |>
  dplyr::filter(exam_day == "post")

exam_day_post$SC <- scale(
  exam_day_post$scs_pos_7 + exam_day_post$scs_pos_3 +
    exam_day_post$scs_pos_1 + exam_day_post$scs_pos_6
) |> as.numeric()

exam_day_post$USC <- scale(
  exam_day_post$scs_neg_8 + exam_day_post$scs_neg_5 +
    exam_day_post$scs_neg_2 + exam_day_post$scs_neg_4
) |> as.numeric()

length(unique(exam_day_post$user_id))

cor(exam_day_post$SC, exam_day_post$USC)

hist(exam_day_post$SC)
hist(exam_day_post$USC)

d_post <- exam_day_post |>
  dplyr::select(SC, USC, user_id, day)

outliers <- check_outliers(d_post, method = "mcd", verbose = FALSE)
outliers

d_post_clean <- d_post[-which(outliers), ]

f3 <-
  brm(
    data = d_post_clean,
    family = student,
    bf(mvbind(SC, USC) ~ 1 + (1 | user_id) + (1 | user_id:day)) + set_rescor(TRUE),
    iter = 8000,
    warmup = 2000,
    chains = 4,
    cores = 8,
    seed = 210191,
    # algorithm = "meanfield",
    backend = "cmdstanr"
  )

summary(f3)
# Residual Correlations:
#                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# rescor(SC,USC)    -0.77      0.05    -0.85    -0.67 1.00     3451     5685

pp_check(f3, resp = "SC") + xlim(-5, 5)
pp_check(f3, resp = "USC") + xlim(-5, 5)


# Function to extract correlations
extract_correlations <- function(model) {
  # Extract posterior samples using updated syntax
  posts <- as_draws_df(model)

  # Extract residual (within-timewindow) correlation
  within_corr <- median(posts$rescor__SC__USC)
  within_ci <- quantile(posts$rescor__SC__USC, probs = c(0.055, 0.945))

  # Extract random effects correlations
  # For user level
  user_var_usc <- posts$sd_user_id__USC_Intercept^2
  user_var_sc <- posts$sd_user_id__SC_Intercept^2

  # For day:user_id level
  day_var_usc <- posts$`sd_user_id:day__USC_Intercept`^2
  day_var_sc <- posts$`sd_user_id:day__SC_Intercept`^2

  # Calculate ICC for each variable
  total_var_usc <- user_var_usc + day_var_usc + posts$sigma_USC^2
  total_var_sc <- user_var_sc + day_var_sc + posts$sigma_SC^2

  icc_user_usc <- user_var_usc / total_var_usc
  icc_user_sc <- user_var_sc / total_var_sc
  icc_day_usc <- day_var_usc / total_var_usc
  icc_day_sc <- day_var_sc / total_var_sc

  # Calculate median ICCs
  results <- list(
    within = list(
      correlation = within_corr,
      ci_lower = within_ci[1],
      ci_upper = within_ci[2]
    ),
    icc = list(
      user = list(
        USC = median(icc_user_usc),
        SC = median(icc_user_sc),
        USC_ci = quantile(icc_user_usc, probs = c(0.055, 0.945)),
        SC_ci = quantile(icc_user_sc, probs = c(0.055, 0.945))
      ),
      day = list(
        USC = median(icc_day_usc),
        SC = median(icc_day_sc),
        USC_ci = quantile(icc_day_usc, probs = c(0.055, 0.945)),
        SC_ci = quantile(icc_day_sc, probs = c(0.055, 0.945))
      )
    )
  )

  return(results)
}

# Extract results
results <- extract_correlations(f3)

# Create summary tables
correlation_table <- data.frame(
  Level = "Within-person",
  Correlation = results$within$correlation,
  CI_lower = results$within$ci_lower,
  CI_upper = results$within$ci_upper
)

icc_table <- data.frame(
  Level = c("Between-users", "Between-days"),
  Variable = rep(c("USC", "SC"), each = 2),
  ICC = c(
    results$icc$user$USC,
    results$icc$user$SC,
    results$icc$day$USC,
    results$icc$day$SC
  ),
  CI_lower = c(
    results$icc$user$USC_ci[1],
    results$icc$user$SC_ci[1],
    results$icc$day$USC_ci[1],
    results$icc$day$SC_ci[1]
  ),
  CI_upper = c(
    results$icc$user$USC_ci[2],
    results$icc$user$SC_ci[2],
    results$icc$day$USC_ci[2],
    results$icc$day$SC_ci[2]
  )
)

# Print formatted results
cat("\nCorrelation between USC and SC:\n")
print(knitr::kable(correlation_table, digits = 3))
# |     |Level         | Correlation| CI_lower| CI_upper|
# |:----|:-------------|-----------:|--------:|--------:|
# |5.5% |Within-person |      -0.777|   -0.837|   -0.693|

cat("\nIntraclass Correlation Coefficients (ICC):\n")
print(knitr::kable(icc_table, digits = 3))
# |Level         |Variable |   ICC| CI_lower| CI_upper|
# |:-------------|:--------|-----:|--------:|--------:|
# |Between-users |USC      | 0.262|    0.117|    0.449|
# |Between-days  |USC      | 0.277|    0.130|    0.462|
# |Between-users |SC       | 0.016|    0.000|    0.096|
# |Between-days  |SC       | 0.013|    0.000|    0.088|
