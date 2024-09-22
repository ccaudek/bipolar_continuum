
suppressPackageStartupMessages({
  library(here)
  library(rio)
  library(tidyverse)
  library(lavaan)
  library("MplusAutomation")
})


study_1_df <- rio::import(here::here("data", "study_1_data.csv")) |>
  dplyr::select(
    "user_id", "day", "time_window", "scs_pos_1",
    "scs_neg_2", "scs_pos_3", "scs_neg_4", "scs_neg_5",
    "scs_pos_6", "scs_pos_7", "scs_neg_8"
  )

study_2_df <- rio::import(here::here("data", "study_2_data.csv")) |>
  dplyr::select(
    "user_id", "day", "time_window", "scs_pos_1",
    "scs_neg_2", "scs_pos_3", "scs_neg_4", "scs_neg_5",
    "scs_pos_6", "scs_pos_7", "scs_neg_8"
  )

df <- bind_rows(study_1_df, study_2_df)
length(unique(df$user_id))

# Identify the columns that contain "scs_neg_" in their names
neg_items <- grep("scs_neg_", colnames(df), value = TRUE)

# Invert the values for all "scs_neg_" columns (assuming the maximum score is 3)
df[neg_items] <- 3 - df[neg_items]

cor(df[, 4:11]) |> round(2)

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

df$scs_neg_1 <- df$scs_neg_1 - 3
df$scs_neg_2 <- df$scs_neg_2 - 3
df$scs_neg_3 <- df$scs_neg_3 - 3
df$scs_neg_4 <- df$scs_neg_4 - 3

cor(df[, 4:11])


# Consider only the first occasion in the first day -----------------------

df_first_day_first_measurement <- df %>%
  group_by(user_id) %>%                 # Group by user_id
  filter(day == min(day)) %>%           # Select the first day for each user_id
  filter(time_window == min(time_window)) %>%   # Select the first measurement for that day
  ungroup() |>                             # Ungroup the data for further operations
  distinct(user_id, .keep_all = TRUE)    # Keep only the first occurrence of each user_id

df_first_day_first_measurement <- df %>%
  group_by(user_id) %>%                 # Group by user_id
  filter(day == 1) %>%           # Select the first day for each user_id
  filter(time_window == 3) %>%   # Select the first measurement for that day
  ungroup() |>                             # Ungroup the data for further operations
  distinct(user_id, .keep_all = TRUE)    # Keep only the first occurrence of each user_id


length(unique(df_first_day_first_measurement$user_id))
# 494


# One-factor model
model_1 <- '
  SC =~ scs_pos_1 + scs_pos_2 + scs_pos_3 + scs_pos_4 + 
     scs_neg_1 + scs_neg_2 + scs_neg_3 + scs_neg_4
'

# Two factor model
model_2 <- '
  CS =~ scs_pos_1 + scs_pos_2 + scs_pos_3 + scs_pos_4 
  UCS =~ scs_neg_1 + scs_neg_2 + scs_neg_3 + scs_neg_4

  CS ~~ UCS
'


fit_1 <- cfa(
  model_1, 
  data = df_first_day_first_measurement
)

fit_2 <- cfa(
  model_2, 
  data = df_first_day_first_measurement
)

summary(fit_2, fit.measures = TRUE, standardized = TRUE)

anova(fit_1, fit_2)


# All data ----------------------------------------------------------------


# One-factor model
model_onefactor <- '
  # Within level (Level 1)
  SelfCompassion_w =~ scs_pos_1 + scs_pos_2 + scs_pos_3 + scs_pos_4 + 
     scs_neg_1 + scs_neg_2 + scs_neg_3 + scs_neg_4
  
  # Between level (Level 2)
  SelfCompassion_b =~ scs_pos_1 + scs_pos_2 + scs_pos_3 + scs_pos_4 + 
     scs_neg_1 + scs_neg_2 + scs_neg_3 + scs_neg_4
'

fit_onefactor <- cfa(
  model_onefactor, 
  data = df, 
  cluster = "user_id", 
  estimator = "MLR"
)

summary(fit_onefactor, fit.measures = TRUE, standardized = TRUE)


# Generate data for Mplus -------------------------------------------------

# Converts an R data.frame into a tab-delimited file (without header) to be 
# used in an Mplus input file. 
MplusAutomation::prepareMplusData(
  df,
  file = here::here(
    "data", "mplus_data", "neff.dat"
  )
)

# TITLE: Your title goes here
# DATA: FILE = "/Users/corrado/Documents/bipolar_continuum/data/mplus_data/neff.dat";
# VARIABLE:
#   NAMES = user_id day time_window scs_pos_1 scs_neg_2 scs_pos_3 scs_neg_4 scs_neg_5
# scs_pos_6 scs_pos_7 scs_neg_8;
# MISSING=.;

MplusAutomation::runModels(
  here::here("scripts", "mplus_models", "one_factor_2.inp"), showOutput = TRUE
  )

MplusAutomation::runModels(
  here::here("scripts", "mplus_models", "two_factor_2.inp"), showOutput = TRUE
)
# LRT = 274.61, df = 3.


df_no_variation <- df %>%
  group_by(user_id, day) %>%            # Group by user_id and day
  summarize(var_scs_pos_1 = var(scs_pos_1, na.rm = TRUE)) %>%  # Calculate variance for scs_pos_1 within each day
  filter(var_scs_pos_1 == 0 | is.na(var_scs_pos_1))  # Filter rows where variance is zero or NA (if all values are NA)

# View the result
print(df_no_variation)


# Include Negative Affect -------------------------------------------------


study_1_df <- rio::import(here::here("data", "study_1_data.csv")) |>
  dplyr::select(
    "user_id", "day", "time_window", "scs_pos_1",
    "scs_neg_2", "scs_pos_3", "scs_neg_4", "scs_neg_5",
    "scs_pos_6", "scs_pos_7", "scs_neg_8",
    "neg_aff_Moment", "neg_aff_Day", "neg_aff_Person"
  )

study_2_df <- rio::import(here::here("data", "study_2_data.csv")) |>
  dplyr::select(
    "user_id", "day", "time_window", "scs_pos_1",
    "scs_neg_2", "scs_pos_3", "scs_neg_4", "scs_neg_5",
    "scs_pos_6", "scs_pos_7", "scs_neg_8",
    "neg_aff_Moment", "neg_aff_Day", "neg_aff_Person"
  )

df <- bind_rows(study_1_df)
length(unique(df$user_id))

# Identify the columns that contain "scs_neg_" in their names
neg_items <- grep("scs_neg_", colnames(df), value = TRUE)

# Invert the values for all "scs_neg_" columns (assuming the maximum score is 3)
df[neg_items] <- 3 - df[neg_items]

cor(df[, 4:11]) |> round(2)

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

df$scs_neg_1 <- df$scs_neg_1 - 3
df$scs_neg_2 <- df$scs_neg_2 - 3
df$scs_neg_3 <- df$scs_neg_3 - 3
df$scs_neg_4 <- df$scs_neg_4 - 3


# Converts an R data.frame into a tab-delimited file (without header) to be 
# used in an Mplus input file. 
MplusAutomation::prepareMplusData(
  df,
  file = here::here(
    "data", "mplus_data", "neff_2.dat"
  )
)


MplusAutomation::runModels(
  here::here("scripts", "mplus_models", "one_factor_3.inp"), 
  showOutput = TRUE
)


MplusAutomation::runModels(
  here::here("scripts", "mplus_models", "two_factor_3.inp"), 
  showOutput = TRUE
)


