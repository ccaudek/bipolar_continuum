# Overview ----------------------------------------------------------------
# Associated project: Mindfulness paper on EMA self-compassion
# Script purpose: can careless responding explain the positive 
#   association between CS and UCS that has been found for some
#   participants?
#
# Written by: Corrado Caudek (corrado.caudek@unifi.it)
# Version: 2024-12-20
# Last update: 
# Status: In progress
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
df <- study_2_df
length(unique(df$user_id))
# [1] 326
# [1] 169

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


# Careless responding --------------

df_longstring <- df %>%
  rowwise() %>%
  mutate(
    longstring_val = longstring(matrix(c_across(all_of(scs_cols)), nrow = 1))
  ) %>%
  ungroup()

foo <- df_longstring |> 
  group_by(user_id) |> 
  summarize(
    lgstr = mean(longstring_val)
  )

hist(foo$lgstr)

q95_longstring <- quantile(foo$lgstr, .95)
longstring_bad <- foo[foo$lgstr > q95_longstring, ]$user_id


# IRV --------------

df_irv <- df |> 
  rowwise() |> 
  mutate(
    irv_val = irv(matrix(c_across(all_of(scs_cols)), nrow = 1))
  ) |> 
  ungroup()

foo2 <- df_irv |> 
  group_by(user_id) |> 
  summarize(
    sdirv = mean(irv_val)
  )
  
foo2$sdirv |> hist()

q95_irv <- quantile(foo2$sdirv, .95)
irv_bad <- foo2[foo2$sdirv > q95_irv, ]$user_id


# Evn-odd -------

# Define even and odd items
even_items <- c("scs_pos_1", "scs_pos_2", "scs_pos_3", "scs_pos_4")
odd_items <- c("scs_neg_1", "scs_neg_2", "scs_neg_3", "scs_neg_4")

df_evenodd <- df %>%
  group_by(user_id, day, time_window) %>%
  mutate(
    even_mean = rowMeans(across(all_of(even_items)), na.rm = TRUE),
    odd_mean = rowMeans(across(all_of(odd_items)), na.rm = TRUE),
    evenodd_val = 1 - abs(even_mean - odd_mean) / (even_mean + odd_mean)  # Consistency metric
  ) %>%
  ungroup()

# Select columns for multilevel analysis
df_evenodd <- df_evenodd |> 
  dplyr::select(
    user_id, day, time_window, evenodd_val
  )

df_evenodd_clean <- df_evenodd %>%
  mutate(evenodd_val = ifelse(is.infinite(evenodd_val), NA, evenodd_val)) %>%
  drop_na(evenodd_val)

foo3 <- df_evenodd_clean |> 
  group_by(user_id) |> 
  summarize(
    eo = mean(evenodd_val)
  )

foo3$eo |> hist()


q95_eo <- quantile(foo3$eo, .95)
even_odd_bad <- foo3[foo3$eo > q95_eo, ]$user_id


# Mahad ---------------


# Define the columns for Mahalanobis calculation
scs_cols <- c("scs_pos_1", "scs_neg_1", "scs_pos_2", "scs_neg_2", 
              "scs_neg_3", "scs_pos_3", "scs_pos_4", "scs_neg_4")

# Subset the data for the SCS columns
scs_matrix <- as.matrix(df[, scs_cols])

# Calculate Mahalanobis distances
df$mahad_val <- mahad(scs_matrix)


foo4 <- df |> 
  group_by(user_id) |> 
  summarize(
    maha = mean(mahad_val)
  )

foo4$maha |> hist()

q95_maha <- quantile(foo4$maha, .95)
maha_bad <- foo4[foo4$maha > q95_maha, ]$user_id


#---------------------

# Convert to lists for easier manipulation
vectors <- list(mahad_bad, longstring_bad, irv_bad, even_odd_bad)

# Function to find shared elements across combinations
shared_counts <- map(2:4, ~ {
  combos <- combn(vectors, .x, simplify = FALSE)
  shared <- map(combos, ~ Reduce(intersect, .x)) %>% unlist() %>% unique()
  length(shared)
}) %>% set_names(paste0("shared_by_", 2:4))

# Output the results
shared_counts
# $shared_by_2
# [1] 3
# 
# $shared_by_3
# [1] 0
# 
# $shared_by_4
# [1] 0

#' 
  