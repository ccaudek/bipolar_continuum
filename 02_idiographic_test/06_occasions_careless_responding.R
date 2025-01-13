# Overview ----------------------------------------------------------------
# Associated project: Mindfulness paper on EMA self-compassion
# Script purpose: Check careless responding on the occasion level. Separate
# analyses for each study.
#
# Written by: Corrado Caudek (corrado.caudek@unifi.it)
# Version: 2024-12-20
# Last update: Thu Jan  2 09:31:26 2025
# Status: In progress
# Notes:


# Prelims -----------------------------------------------------------------

if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  here, tictoc, rio, tidyverse, cmdstanr, posterior, bayesplot,
  careless, brms, purrr, psych
)

# Set seed
set.seed(42)

theme_set(bayesplot::theme_default(base_size = 13, base_family = "sans"))
color_scheme_set("brightblue") # bayesplot

# Load helper functions
source(here::here("R", "importing_cleaning_data.R"))


# Study 1 -----------------------------------------------------------------

# Import raw data of study 1
study_1_df <- rio::import(here::here("data", "study_1_data.csv")) |>
  dplyr::select(
    "user_id", "day", "time_window", "scs_pos_1",
    "scs_neg_2", "scs_pos_3", "scs_neg_4", "scs_neg_5",
    "scs_pos_6", "scs_pos_7", "scs_neg_8",
    "neg_aff_Moment", "neg_aff_Day", "neg_aff_Person",
    "context_Moment", "context_Day", "context_Person"
  )

# Import raw data of study 2
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

# Reverse-coding for items dec_2, dec_3, and dec_4, and scale between -3 and +3
foo <- study_2_temp %>%
  mutate(
    dec_2 = 0 - dec_2, # Reverse-coding for dec_2
    dec_3 = 0 - dec_3, # Reverse-coding for dec_4
    dec_4 = 0 - dec_4 # Reverse-coding for dec_4
  )

foo1 <- foo |>
  dplyr::select(starts_with("dec_"))

cor(foo1) |> round(2)

# Factor analysis to compute the factor scores
fa_result <- fa(foo1[, c("dec_1", "dec_2", "dec_3", "dec_4")], nfactors = 1, fm = "minres")

# Extract factor scores (1-factor solution)
foo1$dec <- fa_result$scores[, 1]

# Add factor scores to complete data frame
study_2_temp$dec <- foo1$dec

# Centering decentering
study_2_temp <- center3L(
  dataname = study_2_temp,
  varname = dec,
  idname = user_id,
  dayname = day
)

study_2_df <- study_2_temp |>
  dplyr::select(
    "user_id", "day", "time_window", "scs_pos_1",
    "scs_neg_2", "scs_pos_3", "scs_neg_4", "scs_neg_5",
    "scs_pos_6", "scs_pos_7", "scs_neg_8",
    "neg_aff_Moment", "neg_aff_Day", "neg_aff_Person",
    "context_Moment", "context_Day", "context_Person",
    "dec_Moment", "dec_Day", "dec_Person"
  )

df <- study_2_df # only study 2
# df <- study_1_df # only study 1


FLAG <- 1 # if Study 1
# FLAG <- 2 # if Study 2


# Remove invalid values of time_window ------------------------------------

# Group by user_id and day, and count unique time_window values
time_window_check <- df %>%
  group_by(user_id, day) %>%
  summarize(
    unique_time_windows = n_distinct(time_window), # Count unique time windows
    invalid_time_windows = any(!time_window %in% 1:5), # Check for invalid values
    .groups = "drop"
  )

# Identify problematic rows
problems <- time_window_check %>%
  dplyr::filter(unique_time_windows > 5 | invalid_time_windows)

# Display the problematic rows
print(problems)

# Count the total number of subjects and days with issues
total_problems <- nrow(problems)
cat("Total problematic subject-days:", total_problems, "\n")

df_clean <- df %>%
  dplyr::filter(time_window %in% 1:5)

# Group by user_id and day, and count unique time_window values
time_window_check <- df_clean %>%
  group_by(user_id, day) %>%
  summarize(
    unique_time_windows = n_distinct(time_window), # Count unique time windows
    invalid_time_windows = any(!time_window %in% 1:5), # Check for invalid values
    .groups = "drop"
  )

# Identify problematic rows
problems <- time_window_check %>%
  filter(unique_time_windows > 5 | invalid_time_windows)

# Display the problematic rows
print(problems)

# Count the total number of subjects and days with issues
total_problems <- nrow(problems)
cat("Total problematic subject-days:", total_problems, "\n")


# Remove duplicates -------------------------------------------------------

first_non_na <- function(x) {
  x[!is.na(x)][1]
}

df_clean <- df_clean %>%
  group_by(user_id, day, time_window) %>%
  summarize(across(everything(), first_non_na), .groups = "drop")


# Data wrangling ----------------------------------------------------------

# Scale negative affect, context, and dec
df_clean$neg_aff_Moment <- scale(df_clean$neg_aff_Moment) |> as.numeric()
df_clean$neg_aff_Day <- scale(df_clean$neg_aff_Day) |> as.numeric()
df_clean$neg_aff_Person <- scale(df_clean$neg_aff_Person) |> as.numeric()
df_clean$context_Moment <- scale(df_clean$context_Moment) |> as.numeric()
df_clean$context_Day <- scale(df_clean$context_Day) |> as.numeric()
df_clean$context_Person <- scale(df_clean$context_Person) |> as.numeric()

if (FLAG == 2) {
  df_clean$dec_Moment <- scale(df_clean$dec_Moment) |> as.numeric()
  df_clean$dec_Day <- scale(df_clean$dec_Day) |> as.numeric()
  df_clean$dec_Person <- scale(df_clean$dec_Person) |> as.numeric()
}

# Renaming the "scs_pos_" and "scs_neg_" columns in the desired sequence
colnames(df_clean)[colnames(df_clean) == "scs_pos_1"] <- "scs_pos_1"
colnames(df_clean)[colnames(df_clean) == "scs_pos_3"] <- "scs_pos_2"
colnames(df_clean)[colnames(df_clean) == "scs_pos_6"] <- "scs_pos_3"
colnames(df_clean)[colnames(df_clean) == "scs_pos_7"] <- "scs_pos_4"

colnames(df_clean)[colnames(df_clean) == "scs_neg_2"] <- "scs_neg_1"
colnames(df_clean)[colnames(df_clean) == "scs_neg_4"] <- "scs_neg_2"
colnames(df_clean)[colnames(df_clean) == "scs_neg_5"] <- "scs_neg_3"
colnames(df_clean)[colnames(df_clean) == "scs_neg_8"] <- "scs_neg_4"

unique_ids <- unique(df_clean$user_id)
length(unique_ids)
# [1] 326
# [1] 169


# Compute indices for careless responding ---------------------------------

# Remove three values with NAs on time_window.
df_clean <- df_clean[!is.na(df_clean$time_window), ]

# Define the columns containing the scale items
scs_cols <- c(
  "scs_pos_1", "scs_neg_1", "scs_pos_2", "scs_neg_2",
  "scs_neg_3", "scs_pos_3", "scs_pos_4", "scs_neg_4"
)

# Adjusted fences using robust methods -----------------------------------

# Robust function to calculate adjusted fences using bootstrapping
calculate_adjusted_fences <- function(values, n_boot = 500, multiplier = 1.5) {
  # Check if there are enough non-NA values
  if (sum(!is.na(values)) < 5) { # Minimum threshold for robust quantile computation
    return(list(lower = NA, upper = NA))
  }

  # Bootstrapping with explicit handling of errors and structure
  boot_quantiles <- tryCatch(
    {
      boot_samples <- replicate(n_boot, sample(values, length(values), replace = TRUE), simplify = FALSE)
      do.call(rbind, lapply(boot_samples, function(sample) {
        Q1 <- quantile(sample, 0.25, na.rm = TRUE)
        Q3 <- quantile(sample, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        data.frame(lower = Q1 - multiplier * IQR, upper = Q3 + multiplier * IQR)
      }))
    },
    error = function(e) {
      NULL # Return NULL if an error occurs
    }
  )

  # Check if boot_quantiles is valid and has rows
  if (is.null(boot_quantiles) || nrow(boot_quantiles) == 0) {
    return(list(lower = NA, upper = NA))
  }

  # Calculate mean bounds
  lower_bound <- mean(boot_quantiles$lower, na.rm = TRUE)
  upper_bound <- mean(boot_quantiles$upper, na.rm = TRUE)
  list(lower = lower_bound, upper = upper_bound)
}


# Longstring Index per occasion -------------------------------------------

# Compute the Longstring Index per occasion
df_longstring <- df_clean %>%
  rowwise() %>%
  mutate(
    longstring_val = longstring(matrix(c_across(all_of(scs_cols)), nrow = 1))
  ) %>%
  ungroup()

# Compute quartiles and flag occasions with robust fences
flagged_occasions_longstring <- df_longstring %>%
  group_by(user_id) %>%
  summarize(
    upper_fence = calculate_adjusted_fences(longstring_val)$upper # Only calculate the upper fence
  ) %>%
  right_join(df_longstring, by = "user_id") %>%
  mutate(
    careless_flag = longstring_val > upper_fence # Flag only large values
  ) %>%
  ungroup()

# Compute the total number of occasions and the proportion of flagged
# occasions per participant
summary_proportions <- flagged_occasions_longstring %>%
  group_by(user_id) %>%
  summarize(
    total_occasions = n(), # Total occasions for each participant
    flagged_occasions = sum(careless_flag, na.rm = TRUE), # Total flagged occasions
    proportion_flagged = flagged_occasions / total_occasions
    # Proportion of flagged occasions
  ) %>%
  arrange(desc(proportion_flagged))

# Plot the histogram of proportions
hist(
  summary_proportions$proportion_flagged,
  main = "Proportion of Flagged Occasions",
  xlab = "Proportion Flagged",
  ylab = "Frequency",
  col = "lightblue"
)

# Count the number of participants with no flagged occasions
no_flagged_participants <- sum(summary_proportions$flagged_occasions == 0)
no_flagged_participants
# [1] 101
# [1] 51

# Proportion of participants with no outliers in any occasion:
no_flagged_participants / length(unique(df_clean$user_id))
# [1] 0.309816
# [1] 0.3017751


# Compute IRV index per occasion ------------------------------------------

# Note: Smaller number of items → higher likelihood of detecting outliers.
# Compute IRV index per occasion
df_irv <- df_clean %>%
  rowwise() %>%
  mutate(
    irv_val = irv(as.data.frame(t(c_across(all_of(scs_cols)))), na.rm = TRUE, split = FALSE, num.split = 3)
  ) %>%
  ungroup()

# Compute robust fences and flag occasions with low IRV values
flagged_occasions_irv <- df_irv %>%
  group_by(user_id) %>%
  summarize(
    lower_fence = calculate_adjusted_fences(irv_val)$lower, # Robust lower fence
    upper_fence = calculate_adjusted_fences(irv_val)$upper # Robust upper fence (optional, if needed)
  ) %>%
  right_join(df_irv, by = "user_id") %>%
  mutate(
    careless_flag = irv_val < lower_fence # Flag values below the robust lower fence
  ) %>%
  ungroup()

# Compute the total number of occasions and the proportion of flagged occasions per participant
summary_proportions <- flagged_occasions_irv %>%
  group_by(user_id) %>%
  summarize(
    total_occasions = n(), # Total occasions for each participant
    flagged_occasions = sum(careless_flag, na.rm = TRUE), # Total flagged occasions
    proportion_flagged = flagged_occasions / total_occasions # Proportion of flagged occasions
  ) %>%
  arrange(desc(proportion_flagged))

# Plot the histogram of proportions
hist(
  summary_proportions$proportion_flagged,
  main = "Proportion of Flagged Occasions (Low IRV)",
  xlab = "Proportion Flagged",
  ylab = "Frequency",
  col = "lightblue"
)

# Count the number of participants with no flagged occasions
no_flagged_participants <- sum(summary_proportions$flagged_occasions == 0)
no_flagged_participants
# [1] 212
# [1] 124

# Proportion of participants with no outliers in any occasion:
no_flagged_participants / length(unique(df_clean$user_id))
# [1] 0.6503067
# [1] 0.7337278


# Even-odd by occasion ----------------------------------------------------

# Define even and odd items
even_items <- c("scs_pos_1", "scs_pos_2", "scs_pos_3", "scs_pos_4")
odd_items <- c("scs_neg_1", "scs_neg_2", "scs_neg_3", "scs_neg_4")

# Compute the even-odd index per occasion
df_evenodd <- df_clean %>%
  rowwise() %>%
  mutate(
    even_mean = mean(c_across(all_of(even_items)), na.rm = TRUE), # Mean of even items
    odd_mean = mean(c_across(all_of(odd_items)), na.rm = TRUE), # Mean of odd items
    evenodd_val = 1 - abs(even_mean - odd_mean) / (even_mean + odd_mean) # Consistency metric
  ) %>%
  ungroup()

# Clean the data by removing infinite or missing values
df_evenodd_clean <- df_evenodd %>%
  mutate(evenodd_val = ifelse(is.infinite(evenodd_val), NA, evenodd_val)) %>%
  drop_na(evenodd_val)

# Compute robust fences and flag occasions with extreme even-odd values
flagged_occasions_eo <- df_evenodd_clean %>%
  group_by(user_id) %>%
  summarize(
    lower_fence = calculate_adjusted_fences(evenodd_val)$lower, # Robust lower fence
    upper_fence = calculate_adjusted_fences(evenodd_val)$upper # Robust upper fence
  ) %>%
  right_join(df_evenodd_clean, by = "user_id") %>%
  mutate(
    careless_flag = evenodd_val < lower_fence | evenodd_val > upper_fence # Flag both extremes
  ) %>%
  ungroup()

# Compute the total number of occasions and the proportion of flagged occasions per participant
summary_proportions <- flagged_occasions_eo %>%
  group_by(user_id) %>%
  summarize(
    total_occasions = n(), # Total occasions for each participant
    flagged_occasions = sum(careless_flag, na.rm = TRUE), # Total flagged occasions
    proportion_flagged = flagged_occasions / total_occasions # Proportion of flagged occasions
  ) %>%
  arrange(desc(proportion_flagged))

# Plot the histogram of proportions
hist(
  summary_proportions$proportion_flagged,
  main = "Proportion of Flagged Occasions (Low Even-Odd Consistency)",
  xlab = "Proportion Flagged",
  ylab = "Frequency",
  col = "lightblue"
)

# Count the number of participants with no flagged occasions
no_flagged_participants <- sum(summary_proportions$flagged_occasions == 0)
no_flagged_participants
# [1] 45
# [1] 29

# Proportion of participants with no outliers in any occasion:
no_flagged_participants / length(unique(df_clean$user_id))
# [1] 0.1380368
# [1] 0.1715976


# Mahalanobis distance ----------------------------------------------------

# Filter out rows with all NA values in SCS columns
df_mahad_clean <- df_clean %>%
  filter(rowSums(is.na(select(., all_of(scs_cols)))) < length(scs_cols)) # Keep rows with at least one non-NA value

# Compute Mahalanobis distances for SCS columns
df_mahad <- df_mahad_clean %>%
  mutate(
    mahad_val = mahad(as.matrix(select(., all_of(scs_cols)))) # Compute Mahalanobis distance
  )

# Compute robust fences and flag occasions with high Mahalanobis distances
flagged_occasions_mahad <- df_mahad %>%
  group_by(user_id) %>%
  summarize(
    upper_fence = calculate_adjusted_fences(mahad_val)$upper # Robust upper fence
  ) %>%
  right_join(df_mahad, by = "user_id") %>%
  mutate(
    careless_flag = mahad_val > upper_fence # Flag values above the robust upper fence
  ) %>%
  ungroup()

# Compute the total number of occasions and the proportion of flagged occasions
# per participant
summary_proportions <- flagged_occasions_mahad %>%
  group_by(user_id) %>%
  summarize(
    total_occasions = n(), # Total occasions for each participant
    flagged_occasions = sum(careless_flag, na.rm = TRUE), # Total flagged occasions
    proportion_flagged = flagged_occasions / total_occasions # Proportion of flagged occasions
  ) %>%
  arrange(desc(proportion_flagged))

# Plot the histogram of proportions
hist(
  summary_proportions$proportion_flagged,
  main = "Proportion of Flagged Occasions (High Mahalanobis Distance)",
  xlab = "Proportion Flagged",
  ylab = "Frequency",
  col = "lightblue"
)

# Count the number of participants with no flagged occasions
no_flagged_participants <- sum(summary_proportions$flagged_occasions == 0)
no_flagged_participants
# [1] 85
# [1] 49

# Proportion of participants with no outliers in any occasion:
no_flagged_participants / length(unique(df_clean$user_id))
# [1] 0.2607362
# [1] 0.2899408


# Merge -------------------------------------------------------------------

# Merge flagged data from all CR indices by user_id and occasion
integrated_flags <- df_clean %>%
  dplyr::select(user_id, day, time_window) %>% # Keep only user_id, day, and time_window
  dplyr::distinct() %>% # Ensure no duplicate rows
  left_join(
    flagged_occasions_longstring %>%
      select(user_id, day, time_window, careless_flag) %>%
      rename(longstring_flag = careless_flag),
    by = c("user_id", "day", "time_window")
  ) %>% # Add longstring flag
  left_join(
    flagged_occasions_irv %>%
      select(user_id, day, time_window, careless_flag) %>%
      rename(irv_flag = careless_flag),
    by = c("user_id", "day", "time_window")
  ) %>% # Add IRV flag
  left_join(
    flagged_occasions_eo %>%
      select(user_id, day, time_window, careless_flag) %>%
      rename(evenodd_flag = careless_flag),
    by = c("user_id", "day", "time_window")
  ) %>% # Add even-odd flag
  left_join(
    flagged_occasions_mahad %>%
      select(user_id, day, time_window, careless_flag) %>%
      rename(mahad_flag = careless_flag),
    by = c("user_id", "day", "time_window")
  ) # Add Mahalanobis flag

# Replace NA with FALSE for flags (indicating no flag for missing cases)
integrated_flags <- integrated_flags %>%
  mutate(
    longstring_flag = ifelse(is.na(longstring_flag), FALSE, longstring_flag),
    irv_flag = ifelse(is.na(irv_flag), FALSE, irv_flag),
    evenodd_flag = ifelse(is.na(evenodd_flag), FALSE, evenodd_flag),
    mahad_flag = ifelse(is.na(mahad_flag), FALSE, mahad_flag)
  )

# Add a summary column indicating the total number of flags per occasion
integrated_flags <- integrated_flags %>%
  mutate(total_flags = longstring_flag + irv_flag + evenodd_flag + mahad_flag)

# View the integrated data
head(integrated_flags)

# Summary of occasions with at least one flag
flag_summary <- integrated_flags %>%
  summarize(
    total_occasions = n(),
    occasions_with_flags = sum(total_flags > 0),
    proportion_with_flags = occasions_with_flags / total_occasions
  )

# Print the summary
print(flag_summary)
#     total_occasions occasions_with_flags proportion_with_flags
#   1           11932                 2340                 0.196

#     total_occasions occasions_with_flags proportion_with_flags
#   1            6630                 1374                 0.207

flag_distribution <- integrated_flags %>%
  group_by(total_flags) %>%
  summarize(count = n(), proportion = count / nrow(integrated_flags))

print(flag_distribution)

# Study 1
#   total_flags count proportion
# 1           0  9592  0.804
# 2           1  2113  0.177
# 3           2   211  0.0177
# 4           3    15  0.00126
# 5           4     1  0.0000838

# Study 2
#     total_flags count proportion
# 1           0  5256   0.793
# 2           1  1256   0.189
# 3           2   112   0.0169
# 4           3     5   0.000754
# 5           4     1   0.000151

#' The output from your flag_distribution shows a detailed breakdown of
#' how many occasions are flagged for 0, 1, 2, or 3 CR indices.
#' Here's what it means:

# Summary of Results
# Total Flags = 0:
# 9592 occasions (80.4%) are not flagged for any CR index.
# This indicates the majority of the dataset does not show signs of careless responding.

# Total Flags = 1:
# 2113 occasions (17.7%) are flagged for exactly one CR index.
# This suggests some mild evidence of careless responding, isolated to a single measure.

# Total Flags = 2:
# 211 occasions (1.77%) are flagged for two CR indices.
# This indicates stronger evidence of careless responding.

# Total Flags = 3:
# 1 occasions (0.126%) are flagged for three CR indices.
# These occasions likely reflect significant issues.

# Total Flags = 4:
# 15 occasions (0.00838%) are flagged for three CR indices.
# These occasions very likely reflect significant issues.

# Interpretation:
# Occasions with 1 flag may not warrant exclusion unless they show consistent
# patterns.
# Occasions with 2 or more flags are stronger candidates for exclusion,
# especially if concentrated within certain subjects.
# Subjects with a high proportion of flagged occasions may be identified for
# closer review or exclusion from analysis.

# Study 1 ---

# Filter out occasions with Total Flags >= 2
filtered_data <- df_clean %>%
  left_join(integrated_flags %>% select(user_id, day, time_window, total_flags),
    by = c("user_id", "day", "time_window")
  ) %>%
  dplyr::filter(total_flags < 2 | is.na(total_flags)) %>% # Keep occasions with <2 flags or missing total_flags
  dplyr::select(-total_flags) # Remove the total_flags column if no longer needed

nrow(filtered_data) / nrow(df_clean)
# [1] 0.9809755

rio::export(
  filtered_data,
  here::here(
    "04_stan", "study_1", "without_occasion_cr.csv"
  )
)

# Study 2 ---

# Filter out occasions with Total Flags >= 2
filtered_data <- df_clean %>%
  left_join(integrated_flags %>% select(user_id, day, time_window, total_flags),
    by = c("user_id", "day", "time_window")
  ) %>%
  dplyr::filter(total_flags < 2 | is.na(total_flags)) %>% # Keep occasions with <2 flags or missing total_flags
  dplyr::select(-total_flags) # Remove the total_flags column if no longer needed

nrow(filtered_data) / nrow(df_clean)
# [1] 0.9828054

# Remove subject with more than two flags on the overall CR analysis
filtered2_data <- filtered_data |>
  dplyr::filter(user_id != "iWiTcM0XQi")

rio::export(
  filtered2_data,
  here::here(
    "04_stan", "study_2", "without_occasion_cr.csv"
  )
)





# Calculate the median proportion of flagged occasions (> 1 CR indices) per participant
median_proportion <- integrated_flags %>%
  group_by(user_id) %>%
  summarize(
    flagged_proportion = mean(total_flags > 1, na.rm = TRUE) # Proportion of flagged occasions
  ) %>%
  summarize(
    mean_flagged_proportion = mean(flagged_proportion, na.rm = TRUE) # Mean proportion
  )

# View the result
print(median_proportion)

# Study 1
# 1                  0.0197





# eof ---
