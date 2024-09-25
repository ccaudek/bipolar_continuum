# Bipolar continuum project


# Setup -------------------------------------------------------------------

# Load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, rio, tidyverse, lavaan, MplusAutomation)

# Set seed
set.seed(42)

# Load helper functions
source(here::here("R", "importing_cleaning_data.R"))


# Import and clean data ---------------------------------------------------

df <- get_data()
length(unique(df$user_id))
# [1] 495

# Clean data
df <- data_cleaning(df)
# Check that all correlations are positive after coding reversal of UCS
cor(df[, 4:11]) |> round(2)


# Data dictionary ---------------------------------------------------------

# TODO


# Consider only the first occasion in the first day -----------------------

df_first_day_first_measurement <- df %>%
  group_by(user_id) %>%                 # Group by user_id
  filter(day == min(day)) %>%           # Select the first day for each user_id
  filter(time_window == min(time_window)) %>%   # Select the first measurement for that day
  ungroup() |>                           # Ungroup the data for further operations
  distinct(user_id, .keep_all = TRUE)    # Keep only the first occurrence of each user_id

df_first_day_first_measurement <- df %>%
  group_by(user_id) %>%          # Group by user_id
  filter(day == 1) %>%           # Select the first day for each user_id
  filter(time_window == 3) %>%   # Select the first measurement for that day
  ungroup() |>                   # Ungroup the data for further operations
  distinct(user_id, .keep_all = TRUE)  # Keep only the first occurrence of each user_id


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


study_2_temp <- rio::import(here::here("data", "study_2_data.csv"))
# Add negative affect scaled by occasion, day, person
study_2_temp <- center3L(
  dataname = study_2_temp,
  varname = neg_aff, 
  idname = user_id, 
  dayname = day
)

study_2_df <- study_2_temp |>
  dplyr::select(
    "user_id", "day", "time_window", "scs_pos_1",
    "scs_neg_2", "scs_pos_3", "scs_neg_4", "scs_neg_5",
    "scs_pos_6", "scs_pos_7", "scs_neg_8",
    "neg_aff_Moment", "neg_aff_Day", "neg_aff_Person"
  )

# Combine the two studies
df <- bind_rows(study_1_df, study_2_df)
length(unique(df$user_id))
# [1] 495

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

hist(df$neg_aff_Person)
hist(df$neg_aff_Day)
hist(df$neg_aff_Moment)

# Assuming df is your data frame

# Function to vincentize a variable (cap at 5th and 95th percentiles)
vincentize <- function(x) {
  lower <- quantile(x, 0.01, na.rm = TRUE)  # 5th percentile
  upper <- quantile(x, 0.99, na.rm = TRUE)  # 95th percentile
  x <- ifelse(x < lower, lower, x)  # Cap at the lower bound
  x <- ifelse(x > upper, upper, x)  # Cap at the upper bound
  return(x)
}

# Apply vincentizing to the relevant columns
df$neg_aff_Moment <- vincentize(df$neg_aff_Moment)
df$neg_aff_Day <- vincentize(df$neg_aff_Day)
df$neg_aff_Person <- vincentize(df$neg_aff_Person)

# Scale negative affect components
df$neg_aff_Moment <- scale(df$neg_aff_Moment) |> as.numeric()
df$neg_aff_Day <- scale(df$neg_aff_Day) |> as.numeric()
df$neg_aff_Person <- scale(df$neg_aff_Person) |> as.numeric()

hist(df$neg_aff_Person)
hist(df$neg_aff_Day)
hist(df$neg_aff_Moment)


# Generate a tab-delimited file -------------------------------------------

# Converts an R data.frame into a tab-delimited file (without header) to be 
# used in an Mplus input file. 
MplusAutomation::prepareMplusData(
  df,
  file = here::here(
    "data", "mplus_data", "neff_2.dat"
  )
)


# MplusAutomation::runModels(
#   here::here("scripts", "mplus_models", "one_factor_3.inp"), 
#   showOutput = TRUE
# )
# 
# 
# MplusAutomation::runModels(
#   here::here("scripts", "mplus_models", "two_factor_3.inp"), 
#   showOutput = TRUE
# )


# Models comparison -------------------------------------------------------

MplusAutomation::runModels(
  here::here("01_dimensionality_test", "mplus_models", "one_factor_3.inp"),
  showOutput = TRUE
)

MplusAutomation::runModels(
  here::here("01_dimensionality_test", "mplus_models", "two_factor_4.inp"),
  showOutput = TRUE
)

MplusAutomation::runModels(
  here::here("01_dimensionality_test", "mplus_models", "bifactor_model.inp"),
  showOutput = TRUE
)

# Define paths to the .out files
model_files <- c(
  here("01_dimensionality_test", "mplus_models", "one_factor_3.out"),
  here("01_dimensionality_test", "mplus_models", "two_factor_3.out"),
  here("01_dimensionality_test", "mplus_models", "bifactor_model.out")
)

# Initialize an empty list to store model summaries
model_summaries_list <- list()

# Loop over each model file and read the summaries
for (model_file in model_files) {
  model_summaries_list[[model_file]] <- readModels(model_file, what="summaries")$summaries
}

# Convert the list into a data frame using bind_rows to handle differing columns
model_summaries <- bind_rows(model_summaries_list)

# Check the resulting data frame
print(model_summaries)


# Extract key indices for comparison
aic_values <- model_summaries$AIC
bic_values <- model_summaries$BIC
rmsea_values <- model_summaries$RMSEA_Estimate
cfi_values <- model_summaries$CFI
tli_values <- model_summaries$TLI

LL_values <- model_summaries$LL  # Uncorrected Log-likelihood values
LL_correction_factors <- model_summaries$LLCorrectionFactor  # Correction factors
df_values <- model_summaries$Parameters  # Degrees of freedom (number of parameters)
# Apply the correction to the log-likelihood values
corrected_LL_values <- LL_values / LL_correction_factors

RMSEA_Estimate <- model_summaries$RMSEA_Estimate
SRMR.Within <- model_summaries$SRMR.Within
SRMR.Between <- model_summaries$SRMR.Between
ChiSqM_Value <- model_summaries$ChiSqM_Value
ChiSqM_DF <- model_summaries$ChiSqM_DF


# Combine results into a data frame for easy comparison
comparison_table <- data.frame(
  Model = c("One-Factor", "Two-Factor", "Bifactor"),
  AIC = aic_values,
  BIC = bic_values,
  RMSEA = rmsea_values,
  CFI = cfi_values,
  TLI = tli_values,
  LL = corrected_LL_values,
  df = df_values,
  RSMSEA = RMSEA_Estimate, 
  SRMR_Within = SRMR.Within,
  SRMR_Between = SRMR.Between,
  ChiSqM_Value = ChiSqM_Value,
  ChiSqM_DF = ChiSqM_DF
)

# Display the comparison table
print(comparison_table)


# Log-likelihoods and number of parameters from model summaries
LL_one_factor <- comparison_table[1, 7]  # Log-likelihood of One-Factor model
df_one_factor <- comparison_table[1, 8]          # Number of parameters in One-Factor model

LL_two_factor <- comparison_table[2, 7] # Log-likelihood of Two-Factor model
df_two_factor <- comparison_table[2, 8]         # Number of parameters in Two-Factor model

LL_bifactor <- comparison_table[3, 7]    # Log-likelihood of Bifactor model
df_bifactor <- comparison_table[3, 8]           # Number of parameters in Bifactor model

# LRT between One-Factor and Two-Factor models
LL_diff_1_vs_2 <- 2 * (LL_two_factor - LL_one_factor)  # Calculate chi-square statistic
df_diff_1_vs_2 <- df_two_factor - df_one_factor        # Difference in degrees of freedom
p_value_1_vs_2 <- pchisq(LL_diff_1_vs_2, df_diff_1_vs_2, lower.tail = FALSE)  # Calculate p-value

cat("LRT between One-Factor and Two-Factor models:\n")
cat("Chi-square statistic:", LL_diff_1_vs_2, "\n")
cat("Degrees of freedom difference:", df_diff_1_vs_2, "\n")
cat("p-value:", p_value_1_vs_2, "\n\n")

# LRT between Two-Factor and Bifactor models
LL_diff_2_vs_bifactor <- 2 * (LL_bifactor - LL_two_factor)  # Calculate chi-square statistic
df_diff_2_vs_bifactor <- df_bifactor - df_two_factor        # Difference in degrees of freedom
p_value_2_vs_bifactor <- pchisq(LL_diff_2_vs_bifactor, df_diff_2_vs_bifactor, lower.tail = FALSE)  # Calculate p-value

cat("LRT between Two-Factor and Bifactor models:\n")
cat("Chi-square statistic:", LL_diff_2_vs_bifactor, "\n")
cat("Degrees of freedom difference:", df_diff_2_vs_bifactor, "\n")
cat("p-value:", p_value_2_vs_bifactor, "\n")


