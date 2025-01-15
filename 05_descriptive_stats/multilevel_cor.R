# Compute the multilevel correlations between each of the 8 components
# of the state self-compassion scale and the average value of CS and UCS,
# by considering the clustering nature of the data.


# Setup -------------------------------------------------------------------

# Load packages
if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  here, tictoc, rio, tidyverse, cmdstanr, posterior,
  bayesplot, lme4, brms
)

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
# df <- bind_rows(study_1_df, study_2_df)
df <- study_1_df

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
if (0) {
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



# Prepare the data
# Compute the Compassionate Self (CS) and Uncompassionate Self (UCS) components

# First create a function to calculate means excluding specific items
calculate_means_excluding_item <- function(df, item, pos_items, neg_items) {
  if (item %in% pos_items) {
    # For positive items, exclude from CS
    CS <- rowMeans(select(df, all_of(pos_items[pos_items != item])), na.rm = TRUE)
    UCS <- rowMeans(select(df, all_of(neg_items)), na.rm = TRUE)
  } else {
    # For negative items, exclude from UCS
    CS <- rowMeans(select(df, all_of(pos_items)), na.rm = TRUE)
    UCS <- rowMeans(select(df, all_of(neg_items[neg_items != item])), na.rm = TRUE)
  }
  return(list(CS = CS, UCS = UCS))
}

# Define the positive and negative items
pos_items <- c("scs_pos_1", "scs_pos_2", "scs_pos_3", "scs_pos_4")
neg_items <- c("scs_neg_1", "scs_neg_2", "scs_neg_3", "scs_neg_4")
scs_subscales <- c(pos_items, neg_items)

# Initialize results list
results <- list()

# Loop through each SCS subscale
for (subscale in scs_subscales) {
  # Calculate means excluding the current item
  means <- calculate_means_excluding_item(df, subscale, pos_items, neg_items)

  # Create temporary dataframe with updated scores
  temp_df <- df
  temp_df$CS <- scale(means$CS)[, 1] # Standardize scores
  temp_df$UCS <- scale(means$UCS)[, 1]
  temp_df[[subscale]] <- scale(df[[subscale]])[, 1] # Standardize current item

  # Determine which score to use based on item type
  target_score <- if (subscale %in% pos_items) "CS" else "UCS"

  # Fit lmer model
  model <- lmer(
    paste(target_score, "~", subscale, "+ (1 | user_id/day/time_window)"),
    data = temp_df
  )

  # Extract fixed effect coefficient - now always positive regardless of item type
  between_corr <- (fixef(model)[[2]])

  # Extract residuals
  residuals <- resid(model)

  # Calculate within correlation
  within_corr <- cor(temp_df[[subscale]], residuals + temp_df[[target_score]])

  # Store results
  results[[subscale]] <- list(
    between_correlation = between_corr,
    within_correlation = within_corr
  )
}


# Print results
print(results)




# $scs_pos_1
# $scs_pos_1$between_correlation
# [1] 0.9418825
#
# $scs_pos_1$within_correlation
# [1] 0.9103079
#
#
# $scs_pos_2
# $scs_pos_2$between_correlation
# [1] 0.8824519
#
# $scs_pos_2$within_correlation
# [1] 0.8130814
#
#
# $scs_pos_3
# $scs_pos_3$between_correlation
# [1] 0.9234244
#
# $scs_pos_3$within_correlation
# [1] 0.8354339
#
#
# $scs_pos_4
# $scs_pos_4$between_correlation
# [1] 0.9334885
#
# $scs_pos_4$within_correlation
# [1] 0.8351323
#
#
# $scs_neg_1
# $scs_neg_1$between_correlation
# [1] 0.8797395
#
# $scs_neg_1$within_correlation
# [1] 0.8090338
#
#
# $scs_neg_2
# $scs_neg_2$between_correlation
# [1] 0.8533959
#
# $scs_neg_2$within_correlation
# [1] 0.7918063
#
#
# $scs_neg_3
# $scs_neg_3$between_correlation
# [1] 0.8936485
#
# $scs_neg_3$within_correlation
# [1] 0.8101387
#
#
# $scs_neg_4
# $scs_neg_4$between_correlation
# [1] 0.9176115
#
# $scs_neg_4$within_correlation
# [1] 0.9160366
