# Overview ----------------------------------------------------------------
# Associated project: Mindfulness paper on EMA self-compassion
# Script purpose: Check careless responding for data quality. Separate 
# analysis for each study.
#
# Written by: Corrado Caudek (corrado.caudek@unifi.it)
# Version: 2024-12-20
# Last update: Thu Jan  2 08:56:17 2025
# Status: Final
# Notes: 


# Prelims -----------------------------------------------------------------

if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  here, tictoc, rio, tidyverse, cmdstanr, posterior, bayesplot, 
  careless, brms, papaja, magrittr, purrr
)

# Set seed
set.seed(42)

theme_set(bayesplot::theme_default(base_size = 13, base_family = "sans"))
color_scheme_set("brightblue") # bayesplot

# Load helper functions
source(here::here("R", "importing_cleaning_data.R"))
source(here::here("R", "shared_overall_careless_resp_indices.R"))


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

study_2_df <- study_2_temp |>
  dplyr::select(
    "user_id", "day", "time_window", "scs_pos_1",
    "scs_neg_2", "scs_pos_3", "scs_neg_4", "scs_neg_5",
    "scs_pos_6", "scs_pos_7", "scs_neg_8",
    "neg_aff_Moment", "neg_aff_Day", "neg_aff_Person",
    "context_Moment", "context_Day", "context_Person"
  )

# Combine the data of the two studies

# df <- bind_rows(study_1_df, study_2_df)
df <- study_1_df # only study 1


# Remove invalid values of time_window ------------------------------------ 

# Group by user_id and day, and count unique time_window values
time_window_check <- df %>%
  group_by(user_id, day) %>%
  summarize(
    unique_time_windows = n_distinct(time_window),  # Count unique time windows
    invalid_time_windows = any(!time_window %in% 1:5),  # Check for invalid values
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
    unique_time_windows = n_distinct(time_window),  # Count unique time windows
    invalid_time_windows = any(!time_window %in% 1:5),  # Check for invalid values
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

# Change the name of the data.frame
df <- df_clean

# Scale negative affect and context
df$neg_aff_Moment <- scale(df$neg_aff_Moment) |> as.numeric()
df$neg_aff_Day <- scale(df$neg_aff_Day) |> as.numeric()
df$neg_aff_Person <- scale(df$neg_aff_Person) |> as.numeric()
df$context_Moment <- scale(df$context_Moment) |> as.numeric()
df$context_Day <- scale(df$context_Day) |> as.numeric()
df$context_Person <- scale(df$context_Person) |> as.numeric()

# Renaming the "scs_pos_" and "scs_neg_" columns in the desired sequence
colnames(df)[colnames(df) == "scs_pos_1"] <- "scs_pos_1"
colnames(df)[colnames(df) == "scs_pos_3"] <- "scs_pos_2"
colnames(df)[colnames(df) == "scs_pos_6"] <- "scs_pos_3"
colnames(df)[colnames(df) == "scs_pos_7"] <- "scs_pos_4"

colnames(df)[colnames(df) == "scs_neg_2"] <- "scs_neg_1"
colnames(df)[colnames(df) == "scs_neg_4"] <- "scs_neg_2"
colnames(df)[colnames(df) == "scs_neg_5"] <- "scs_neg_3"
colnames(df)[colnames(df) == "scs_neg_8"] <- "scs_neg_4"

unique_ids <- unique(df$user_id)
length(unique_ids)
# [1] 326

# Compute number of shared CR indices ---

shared_cr_indices(df)
# $shared_by_2
# [1] 2
# 
# $shared_by_3
# [1] 1
# 
# $shared_by_4
# [1] 0

# Conclusion
# There are only 3 participants with extreme values on two or more CR indices.


# Study 2 -----------------------------------------------------------------

df <- study_2_df # only study 2
length(unique(df$user_id))
# [1] 169


# Remove invalid values of time_window ------------------------------------ 

# Group by user_id and day, and count unique time_window values
time_window_check <- df %>%
  group_by(user_id, day) %>%
  summarize(
    unique_time_windows = n_distinct(time_window),  # Count unique time windows
    invalid_time_windows = any(!time_window %in% 1:5),  # Check for invalid values
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
    unique_time_windows = n_distinct(time_window),  # Count unique time windows
    invalid_time_windows = any(!time_window %in% 1:5),  # Check for invalid values
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

# Change the name of the data.frame
df <- df_clean


# Scale negative affect and context
df$neg_aff_Moment <- scale(df$neg_aff_Moment) |> as.numeric()
df$neg_aff_Day <- scale(df$neg_aff_Day) |> as.numeric()
df$neg_aff_Person <- scale(df$neg_aff_Person) |> as.numeric()
df$context_Moment <- scale(df$context_Moment) |> as.numeric()
df$context_Day <- scale(df$context_Day) |> as.numeric()
df$context_Person <- scale(df$context_Person) |> as.numeric()

# Renaming the "scs_pos_" and "scs_neg_" columns in the desired sequence
colnames(df)[colnames(df) == "scs_pos_1"] <- "scs_pos_1"
colnames(df)[colnames(df) == "scs_pos_3"] <- "scs_pos_2"
colnames(df)[colnames(df) == "scs_pos_6"] <- "scs_pos_3"
colnames(df)[colnames(df) == "scs_pos_7"] <- "scs_pos_4"

colnames(df)[colnames(df) == "scs_neg_2"] <- "scs_neg_1"
colnames(df)[colnames(df) == "scs_neg_4"] <- "scs_neg_2"
colnames(df)[colnames(df) == "scs_neg_5"] <- "scs_neg_3"
colnames(df)[colnames(df) == "scs_neg_8"] <- "scs_neg_4"

unique_ids <- unique(df$user_id)
length(unique_ids)
# 169

# Compute number of shared CR indices ---

shared_cr_indices(df)
# $shared_by_2
# [1] 3
# 
# $shared_by_3
# [1] 1
# 
# $shared_by_4
# [1] 0


# eof ---




  