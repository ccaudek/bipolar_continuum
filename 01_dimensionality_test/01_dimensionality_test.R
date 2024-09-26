# Bipolar continuum project


# Setup -------------------------------------------------------------------

# Load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  here, tictoc, rio, tidyverse, lavaan, MplusAutomation, tibble, knitr,
  semPlot
)

# Set seed
set.seed(42)

# Load helper functions
source(here::here("R", "importing_cleaning_data.R"))


# Import and clean data ---------------------------------------------------

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

# Models comparison -------------------------------------------------------

MplusAutomation::runModels(
  here::here("01_dimensionality_test", "mplus_models", "one_factor_3.inp"),
  showOutput = TRUE
)

MplusAutomation::runModels(
  here::here("01_dimensionality_test", "mplus_models", "two_factor_3.inp"),
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


# Extract log-likelihoods and number of parameters
LL_one_factor <- model_summaries$LL[1]
LL_two_factor <- model_summaries$LL[2]
LL_bifactor <- model_summaries$LL[3]

nparam_one_factor <- model_summaries$Parameters[1]
nparam_two_factor <- model_summaries$Parameters[2]
nparam_bifactor <- model_summaries$Parameters[3]

# Perform LRT for one-factor vs two-factor
LRT_one_vs_two <- 2 * (LL_two_factor - LL_one_factor)
df_one_vs_two <- nparam_two_factor - nparam_one_factor
p_value_one_vs_two <- pchisq(LRT_one_vs_two, df=df_one_vs_two, lower.tail=FALSE)

# Print the result for one-factor vs two-factor
cat("LRT for One-Factor vs Two-Factor Model:\n")
cat("LRT statistic:", LRT_one_vs_two, "\n")
cat("Degrees of freedom:", df_one_vs_two, "\n")
cat("p-value:", p_value_one_vs_two, "\n\n")

# Perform LRT for two-factor vs bifactor
LRT_two_vs_bifactor <- 2 * (LL_bifactor - LL_two_factor)
df_two_vs_bifactor <- nparam_bifactor - nparam_two_factor
p_value_two_vs_bifactor <- pchisq(LRT_two_vs_bifactor, df=df_two_vs_bifactor, lower.tail=FALSE)

# Print the result for two-factor vs bifactor
cat("LRT for Two-Factor vs Bifactor Model:\n")
cat("LRT statistic:", LRT_two_vs_bifactor, "\n")
cat("Degrees of freedom:", df_two_vs_bifactor, "\n")
cat("p-value:", p_value_two_vs_bifactor, "\n")



# Create table ------------------------------------------------------------

# Define paths to the .out files
model_files <- c(
  here("01_dimensionality_test", "mplus_models", "one_factor_3.out"),
  here("01_dimensionality_test", "mplus_models", "two_factor_3.out"),
  here("01_dimensionality_test", "mplus_models", "bifactor_model.out")
)

# Initialize an empty list to store standardized parameter estimates
standardized_list <- list()

# Loop over each model file and extract the stdyx.standardized parameters
for (model_file in model_files) {
  model_output <- readModels(model_file)
  
  if (!is.null(model_output$parameters$stdyx.standardized)) {
    standardized_list[[model_file]] <- model_output$parameters$stdyx.standardized
  } else {
    warning(paste("No standardized parameters found for", model_file))
  }
}

# Optionally, name the list items for easier identification
names(standardized_list) <- c("One-Factor", "Two-Factor", "Bifactor")

# Inspect the standardized_list to ensure the parameters are stored
str(standardized_list)

# Combine all standardized parameters into a single data frame
standardized_df <- bind_rows(
  tibble(Model = "One-Factor", standardized_list[[1]]),
  tibble(Model = "Two-Factor", standardized_list[[2]]),
  tibble(Model = "Bifactor", standardized_list[[3]])
)

# Select only key columns such as parameter header, parameter, estimate, and standard error
standardized_df <- standardized_df %>%
  select(Model, paramHeader, param, est, se) %>%
  arrange(Model, paramHeader, param)

# View the resulting combined data frame
print(standardized_df)

# Create a table with key parameters for reporting
kable(standardized_df, format = "markdown", 
      col.names = c("Model", "Parameter Type", "Parameter", "Estimate", "SE"),
      caption = "Standardized Parameter Estimates for One-Factor, Two-Factor, and Bifactor Models")


# Save as CSV
# write.csv(standardized_df, "standardized_parameters.csv", row.names = FALSE)

# library(flextable)
# library(officer)
# 
# # Create a flextable and save to a Word document
# ft <- flextable(standardized_df)
# doc <- read_docx() %>% 
#   body_add_flextable(ft) %>% 
#   print(target = "standardized_parameters.docx")


# Create a table for the One-Factor model
kable(standardized_list[[1]], format = "markdown", 
      col.names = c("Parameter Type", "Parameter", "Estimate", "SE", "Estimate/SE", "p-value", "Between/Within"),
      caption = "Standardized Parameter Estimates for One-Factor Model")

# Create a table for the Two-Factor model
kable(standardized_list[[2]], format = "markdown", 
      col.names = c("Parameter Type", "Parameter", "Estimate", "SE", "Estimate/SE", "p-value", "Between/Within"),
      caption = "Standardized Parameter Estimates for Two-Factor Model")

# Create a table for the Bifactor model
kable(standardized_list[[3]], format = "markdown", 
      col.names = c("Parameter Type", "Parameter", "Estimate", "SE", "Estimate/SE", "p-value", "Between/Within"),
      caption = "Standardized Parameter Estimates for Bifactor Model")



# semPlot -----------------------------------------------------------------

# Load the models directly from the Mplus output files
one_factor_model <- semPlotModel(here("01_dimensionality_test", "mplus_models", "one_factor_3.out"))
two_factor_model <- semPlotModel(here("01_dimensionality_test", "mplus_models", "two_factor_3.out"))
bifactor_model <- semPlotModel(here("01_dimensionality_test", "mplus_models", "bifactor_model.out"))

# Now you can plot the path diagrams
# One-Factor Model
semPaths(one_factor_model, 
         what = "est",  # Use estimates for path diagram
         style = "ram",  # RAM (reticular action model) style
         layout = "tree",  # Tree layout
         title = TRUE,  # Display the title
         title.text = "One-Factor Model",  # Title text
         edge.label.cex = 0.8)  # Customize text size for edges


# Two-Factor Model
semPaths(two_factor_model, 
         what = "est", 
         style = "ram", 
         layout = "tree", 
         title = TRUE,
         title.text = "Two-Factor Model", 
         edge.label.cex = 0.8)


# Bifactor Model
semPaths(bifactor_model, 
         what = "est", 
         style = "ram", 
         layout = "tree", 
         title = TRUE,
         title.text = "Bifactor Model", 
         edge.label.cex = 0.8)



