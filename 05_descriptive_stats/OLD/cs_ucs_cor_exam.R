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

# Import data -------------------------------------------------------------

alldata <- readRDS(
  here::here(
    "data", "mpath_data", "ema_data_3.RDS"
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


# Joint Model for All Periods ---------------------------------------------

# Fit the multilevel model
f_joint <- brm(
  data = alldata,
  family = student,
  bf(
    mvbind(SC, USC) ~ 0 + exam_day +
      (0 + exam_day | user_id) +
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
  backend = "cmdstanr"
)

# Model Diagnostics
print(summary(f_joint)) # Check convergence (R-hat should be close to 1)
pp_check(f_joint, resp = "SC") + xlim(-5, 5) # Posterior predictive checks
pp_check(f_joint, resp = "USC") + xlim(-5, 5) # Posterior predictive checks


# Function to extract period-specific correlations
extract_period_correlations <- function(model) {
  # Get posterior draws
  draws <- as_draws_df(model)
  # Extract residual correlations
  rescor <- draws$rescor__SC__USC

  # Extract residual SDs for each period and variable
  sigma_sc_pre <- draws$`sigma_SC_exam_daypre`
  sigma_usc_pre <- draws$`sigma_USC_exam_daypre`
  sigma_sc_post <- draws$`sigma_SC_exam_daypost`
  sigma_usc_post <- draws$`sigma_USC_exam_daypost`
  sigma_sc_no <- draws$`sigma_SC_exam_dayno`
  sigma_usc_no <- draws$`sigma_USC_exam_dayno`

  # Calculate period-specific correlations
  cors <- data.frame(
    period = c("pre-exam", "post-exam", "no-exam"),
    correlation = c(
      median(rescor[1:length(rescor) / 3]),
      median(rescor[(length(rescor) / 3 + 1):(2 * length(rescor) / 3)]),
      median(rescor[(2 * length(rescor) / 3 + 1):length(rescor)])
    )
  )

  # Add 95% CIs
  cors$ci_lower <- c(
    quantile(rescor[1:length(rescor) / 3], 0.025),
    quantile(rescor[(length(rescor) / 3 + 1):(2 * length(rescor) / 3)], 0.025),
    quantile(rescor[(2 * length(rescor) / 3 + 1):length(rescor)], 0.025)
  )

  cors$ci_upper <- c(
    quantile(rescor[1:length(rescor) / 3], 0.975),
    quantile(rescor[(length(rescor) / 3 + 1):(2 * length(rescor) / 3)], 0.975),
    quantile(rescor[(2 * length(rescor) / 3 + 1):length(rescor)], 0.975)
  )

  # Add difference tests
  diffs <- data.frame(
    comparison = c("pre vs post", "pre vs no", "post vs no"),
    difference = c(
      median(rescor[1:length(rescor) / 3] -
        rescor[(length(rescor) / 3 + 1):(2 * length(rescor) / 3)]),
      median(rescor[1:length(rescor) / 3] -
        rescor[(2 * length(rescor) / 3 + 1):length(rescor)]),
      median(rescor[(length(rescor) / 3 + 1):(2 * length(rescor) / 3)] -
        rescor[(2 * length(rescor) / 3 + 1):length(rescor)])
    )
  )

  # Add 95% CIs for differences
  diffs$ci_lower <- c(
    quantile(rescor[1:length(rescor) / 3] -
      rescor[(length(rescor) / 3 + 1):(2 * length(rescor) / 3)], 0.025),
    quantile(rescor[1:length(rescor) / 3] -
      rescor[(2 * length(rescor) / 3 + 1):length(rescor)], 0.025),
    quantile(rescor[(length(rescor) / 3 + 1):(2 * length(rescor) / 3)] -
      rescor[(2 * length(rescor) / 3 + 1):length(rescor)], 0.025)
  )

  diffs$ci_upper <- c(
    quantile(rescor[1:length(rescor) / 3] -
      rescor[(length(rescor) / 3 + 1):(2 * length(rescor) / 3)], 0.975),
    quantile(rescor[1:length(rescor) / 3] -
      rescor[(2 * length(rescor) / 3 + 1):length(rescor)], 0.975),
    quantile(rescor[(length(rescor) / 3 + 1):(2 * length(rescor) / 3)] -
      rescor[(2 * length(rescor) / 3 + 1):length(rescor)], 0.975)
  )

  return(list(correlations = cors, differences = diffs))
}

# Extract and print results
results <- extract_period_correlations(f_joint)

# Print formatted results
cat("\nWithin-person correlations by exam period:\n")
print(knitr::kable(results$correlations, digits = 3))
# |period    | correlation| ci_lower| ci_upper|
# |:---------|-----------:|--------:|--------:|
# |pre-exam  |      -0.522|   -0.545|   -0.502|
# |post-exam |      -0.522|   -0.543|   -0.500|
# |no-exam   |      -0.521|   -0.544|   -0.499|

cat("\nDifferences between periods:\n")
print(knitr::kable(results$differences, digits = 3))
# |comparison  | difference| ci_lower| ci_upper|
# |:-----------|----------:|--------:|--------:|
# |pre vs post |      0.000|   -0.031|    0.029|
# |pre vs no   |     -0.001|   -0.033|    0.030|
# |post vs no  |     -0.001|   -0.033|    0.033|

# Create a visualization

ggplot(results$correlations, aes(x = period, y = correlation)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  theme_minimal() +
  labs(
    title = "Within-person SC-USC correlations across exam periods",
    y = "Correlation",
    x = "Exam period"
  )


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
