# Overview ----------------------------------------------------------------
# Associated project: Mindfulness paper on EMA self-compassion
# Script purpose: can careless responding explain the positive
#   association between CS and UCS that has been found for some
#   participants? The analysis uses the data for both studies combined.
#
# Written by: Corrado Caudek (corrado.caudek@unifi.it)
# Version: 2024-12-20
# Last update:
# Status: Final
# Notes:
# TODO: Complete the MCMC analysis for the last 3 indices.


# Prelims -----------------------------------------------------------------

if (!requireNamespace("pacman")) install.packages("pacman")

pacman::p_load(
  here, tictoc, rio, tidyverse, cmdstanr, posterior, bayesplot,
  careless, brms, papaja, magrittr
)

# Set seed
set.seed(42)

theme_set(bayesplot::theme_default(base_size = 13, base_family = "sans"))
color_scheme_set("brightblue") # bayesplot

# Load helper functions
source(here::here("R", "importing_cleaning_data.R"))


# Combine data both studies -----------------------------------------------

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

df <- bind_rows(study_1_df, study_2_df)
length(unique(df$user_id))
# [1] 495

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
# [1] 495


# Participants with positive UCS-CS slope ---------------------------------

# Find user_id of participants with UCS-CS slope greater than 0 by considering
# the 89% CI (with lower bound > 0).

# Read MCMC samples: Import the posterior samples generated from the Stan
# model used in the script 01_idiographic_model.R
fit_mcmc <- readRDS(
  here::here("02_idiographic_test", "mcmc", "idiographic_fit.rds")
)

# Extract posterior samples
posterior_samples <- fit_mcmc$draws(format = "df")

# Extract the relevant posterior samples for Neff's hypothesis
beta_cs_samples <- posterior_samples$`beta_cs`
beta_interaction_samples <- posterior_samples$`beta_interaction`
sigma_cs_slope_samples <- posterior_samples$`sigma_participant_slope_cs`

# Extract the individual slopes for CS (z_participant_slope_cs) from the posterior samples
z_participant_slope_cs_samples <- posterior_samples %>%
  dplyr::select(starts_with("z_participant_slope_cs"))

# Calculate the mean slope for each individual
individual_slopes <- apply(as.matrix(z_participant_slope_cs_samples), 2, mean)

# Extract the posterior samples for the fixed effect (beta_cs) and random slopes (z_participant_slope_cs)
beta_cs_samples <- posterior_samples$`beta_cs`
z_participant_slope_cs_samples <- posterior_samples %>%
  dplyr::select(starts_with("z_participant_slope_cs"))

# Sum the fixed effect (beta_cs) and the random effect (z_participant_slope_cs) for each participant
# This gives you the total slope for each participant at each posterior draw
individual_total_slope_samples <- sweep(z_participant_slope_cs_samples, 1, beta_cs_samples, "+")

# Calculate the mean total slope for each participant
individual_total_slopes <- apply(as.matrix(individual_total_slope_samples), 2, mean)

# Print summary of the total individual slopes
summary(individual_total_slopes)

# Plot the distribution of total individual slopes (fixed + random effects)
ggplot(data.frame(slope = individual_total_slopes), aes(x = slope)) +
  geom_histogram(binwidth = 0.05, fill = "blue", alpha = 0.7) +
  labs(
    title = "Distribution of Total Individual Slopes for CS on UCS",
    x = "Total Individual CS Effect on UCS (Fixed + Random Effects)", y = "Count"
  )

mean(individual_total_slopes < 0)
# [1] 0.6606061

# Calcola i quantili 5.5% e 94.5% per ogni soggetto
cred_intervals <- apply(
  individual_total_slope_samples,
  2,
  quantile,
  probs = c(0.055, 0.945)
)

# cred_intervals è ora una matrice 2 x P
# dove la prima riga è il quantile al 5.5% e la seconda al 94.5%

# Identifica i soggetti per i quali il limite inferiore (5.5%) è > 0
positive_strong_indices <- which(cred_intervals[1, ] > 0)

# Ottiene gli user_id di questi soggetti
subjects_positive_strong <- unique_ids[positive_strong_indices]
subjects_positive_strong

# Results obtained with sample():
# [1] "J4UCFGwTmf" "KSYbgBcFLW" "HTvqiZjxkB" "WyiqaH4I13" "gXz3AkAKYD" "54TfRmBgpp"
# [7] "S5urYj563H" "IVNANXrUBu" "UFXt05ZPhH" "7988HNpVDD" "WRdz41kQgL" "X0g8Yxd08n"
# [13] "0b9tNpJcsY" "478rdDr9I2" "BskspQoQEj" "EDKoKhxp0Q" "07l0BvVNP2" "iXBBk4xaNY"
# [19] "W7zr6IzO9E" "mnBsTltDin" "3NluxVM7lo" "2MY3Sj4APf" "xQ6rS0HSh5" "Df04Fa0F3l"
# [25] "kz14Y3K3Vd" "6KsgKjzG0R" "wrTTvWX1Fw" "9jfLs3bV4s" "CK8YygjvcE" "KiH8TUlK9j"
# [31] "WHQKEi8Ser" "IFIRQMYfhe" "IVJjtw4igL" "Da5SiabCZg" "NPrqpTgnOU" "vuzdwPZS7s"
# [37] "yyglmubaxF" "T8mcvDfSY8" "RI5sjGw8xv" "WXBH5xPfyX" "iWiTcM0XQi" "RjnExiVzKR"
# [43] "LF517y25wC" "OkQHNEMEdZ" "BILFRIoQaW" "LKhSGRFGrG" "b6R6tTzsS7" "Ut4Rk8SQSv"
# [49] "Vp186aKWos" "NxTa1Xx4M5" "lbbDJ88QBK" "vsbIebxfaK"

52 / 495
# [1] 0.1050505

# Creating grouping variable ----------------------------------------------

df$is_pos_slope_group <- ifelse(
  df$user_id %in% subjects_positive_strong, 1, 0
)

# group_neg_slope <- df |>
#   dplyr::filter(is_pos_slope_group == 0)
#
# group_pos_slope <- df |>
#   dplyr::filter(is_pos_slope_group == 1)


# Careless responding -----------------------------------------------------

# Compute careless responding for the State SCS.
# Select SCS columns
scs_cols <- grep("^scs_", names(df), value = TRUE)


# longstring --------------------------------------------------------------

df_longstring <- df %>%
  rowwise() %>%
  mutate(
    longstring_val = longstring(matrix(c_across(all_of(scs_cols)), nrow = 1))
  ) %>%
  ungroup()

# Select columns for multilevel analysis
df_result <- df_longstring %>%
  dplyr::select(
    user_id, day, time_window, is_pos_slope_group, longstring_val
  )
df_result$is_pos_slope_group <- factor(df_result$is_pos_slope_group)
head(df_result)

# longstring_val is a categorical variable
hist(df_result$longstring_val)

if (
  !file.exists(
    here::here("02_idiographic_test", "mcmc", "mod_longstring.rds")
  )
) {
  mod_longstring <- brm(
    formula = longstring_val ~ is_pos_slope_group +
      (1 | user_id / day / time_window),
    family = cumulative(link = "logit"), # Modello ordinale cumulativo
    data = df_result,
    # algorithm = "meanfield",
    backend = "cmdstanr",
    file = here::here("02_idiographic_test", "mcmc", "mod_longstring.rds")
  )
} else {
  mod_longstring <- readRDS(
    here::here(
      "02_idiographic_test", "mcmc", "mod_longstring.rds"
    )
  )
}

pp_check(mod_longstring)

summary(mod_longstring, prob = .89)
# Regression Coefficients:
#                     Estimate Est.Error l-89% CI u-89% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]           -2.30      0.02    -2.34    -2.27 1.00     1076      905
# Intercept[2]            0.97      0.02     0.93     1.01 1.00     1031      939
# Intercept[3]            2.71      0.04     2.65     2.78 1.00      883      881
# Intercept[4]            3.87      0.06     3.77     3.96 1.00     1004      944
# Intercept[5]            4.96      0.07     4.85     5.08 1.00      997      914
# Intercept[6]            5.59      0.10     5.44     5.74 1.00      976      771
# Intercept[7]            5.89      0.11     5.71     6.06 1.00     1007      990
# is_pos_slope_group1     0.04      0.05    -0.03     0.11 1.00     1024      907
#
#                     Estimate Est.Error l-89% CI u-89% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]           -3.20      0.12    -3.41    -3.02 1.02      188      655
# Intercept[2]            1.32      0.08     1.19     1.44 1.01      383     1240
# Intercept[3]            3.66      0.13     3.46     3.88 1.01      181      573
# Intercept[4]            5.11      0.17     4.85     5.38 1.01      169      460
# Intercept[5]            6.37      0.20     6.07     6.70 1.01      171      522
# Intercept[6]            7.12      0.22     6.78     7.48 1.01      181      530
# Intercept[7]            7.44      0.23     7.09     7.82 1.01      184      521
# is_pos_slope_group1     0.05      0.20    -0.27     0.37 1.00     1400     1862

# Generate APA Table

a <- summary(mod_longstring)
summary_mod_longstring <- rbind(data.frame(a$fixed), data.frame(a$spec_pars))

summary_mod_longstring <- summary_mod_longstring[1:8, ]

summary_mod_longstring <- summary_mod_longstring |> round(3)

summary_mod_longstring %<>%
  select(-c(Bulk_ESS, Tail_ESS)) %>% # removing ESS
  rownames_to_column(var = "parameter")

# rownames(summary_mod1) <- c("$\\alpha$", "$\\beta$", "$\\sigma_{e}$")
colnames(summary_mod_longstring) <- c("mean", "SE", "lower bound", "upper bound", "Rhat")

write.csv(
  summary_mod_longstring,
  file = here::here(
    "02_idiographic_test", "plots", "careless", "summary_mod_longstring.csv"
  ),
  row.names = FALSE
)

apa_table(
  summary_mod_longstring,
  placement = "H",
  align = c("c", "c", "c", "c", "c", "c"),
  caption = "Posterior mean, standard error, 95\\% credible interval and $\\hat{R}$
    statistic for each parameter of the constant effect model bmod1.",
  note = NULL,
  small = TRUE,
  digits = 3,
  escape = FALSE
)

# Convert the apa_table() output in Markdown with ChatGPT. Then copy in the
# .qmd file.

# Create figure

conditional_effects(mod_longstring, "is_pos_slope_group")

# Save fit.
# mod_longstring$save_object("mod_longstring.rds")

# Calcolo degli effetti condizionali
ce <- conditional_effects(mod_longstring, "is_pos_slope_group")

# Personalizzazione del grafico
plot_ce <- ce$`is_pos_slope_group` %>%
  ggplot(aes(x = as.factor(effect1__), y = estimate__, ymin = lower__, ymax = upper__)) +
  geom_pointrange() +
  labs(
    x = "Group", # Etichetta asse X
    y = "Longstring", # Etichetta asse Y
    title = "Effects of Group on Longstring" # Titolo del grafico (opzionale)
  ) +
  scale_x_discrete(labels = c("0" = "Non-positive\nslope", "1" = "Positive\nslope")) +
  theme(
    axis.text.x = element_text() # Rotazione delle etichette asse X, se necessario
  )

# Visualizzazione del grafico
print(plot_ce)

# Salvataggio del grafico
ggsave(
  filename = here::here(
    "02_idiographic_test", "plots", "careless", "mod_longstring_plot.png"
  ),
  plot = plot_ce, # Nome dell'oggetto del grafico
  width = 8, # Larghezza in pollici
  height = 6, # Altezza in pollici
  dpi = 300 # Risoluzione (300 DPI è standard per la stampa)
)


# Intra-individual Response Variability -----------------------------------

df_irv <- df |>
  rowwise() |>
  mutate(
    irv_val = irv(matrix(c_across(all_of(scs_cols)), nrow = 1))
  ) |>
  ungroup()

# Select columns for multilevel analysis
df_irv <- df_irv |>
  dplyr::select(
    user_id, day, time_window, is_pos_slope_group, irv_val
  )
df_irv$is_pos_slope_group <- factor(df_irv$is_pos_slope_group)

hist(df_irv$irv_val)

if (
  !file.exists(
    here::here("02_idiographic_test", "mcmc", "mod_irv.rds")
  )
) {
  mod_irv <- brm(
    formula = irv_val ~ is_pos_slope_group +
      (1 | user_id / day / time_window),
    family = skew_normal(),
    data = df_irv,
    backend = "cmdstanr",
    algorithm = "meanfield",
    file = here::here("02_idiographic_test", "mcmc", "mod_irv.rds")
  )
} else {
  mod_irv <- readRDS(
    here::here(
      "02_idiographic_test", "mcmc", "mod_irv.rds"
    )
  )
}

pp_check(mod_irv)

summary(mod_irv, prob = 0.89)

conditional_effects(mod_irv, "is_pos_slope_group")


# Calcolo degli effetti condizionali
ce <- conditional_effects(mod_irv, "is_pos_slope_group")

# Personalizzazione del grafico
plot_ce <- ce$`is_pos_slope_group` %>%
  ggplot(aes(x = as.factor(effect1__), y = estimate__, ymin = lower__, ymax = upper__)) +
  geom_pointrange() +
  labs(
    x = "Group", # Etichetta asse X
    y = "Intra-individual Response Variability", # Etichetta asse Y
    title = "Effects of Group on IRV" # Titolo del grafico (opzionale)
  ) +
  scale_x_discrete(labels = c("0" = "Non-positive\nslope", "1" = "Positive\nslope")) +
  theme(
    axis.text.x = element_text() # Rotazione delle etichette asse X, se necessario
  )

# Visualizzazione del grafico
print(plot_ce)


# Salvataggio del grafico
ggsave(
  filename = here::here(
    "02_idiographic_test", "plots", "careless", "mod_irv_plot.png"
  ),
  plot = plot_ce, # Nome dell'oggetto del grafico
  width = 8, # Larghezza in pollici
  height = 6, # Altezza in pollici
  dpi = 300 # Risoluzione (300 DPI è standard per la stampa)
)


# evenodd -----------------------------------------------------------------

# Define even and odd items
even_items <- c("scs_pos_1", "scs_pos_2", "scs_pos_3", "scs_pos_4")
odd_items <- c("scs_neg_1", "scs_neg_2", "scs_neg_3", "scs_neg_4")

df_evenodd <- df %>%
  group_by(user_id, day, time_window) %>%
  mutate(
    even_mean = rowMeans(across(all_of(even_items)), na.rm = TRUE),
    odd_mean = rowMeans(across(all_of(odd_items)), na.rm = TRUE),
    evenodd_val = 1 - abs(even_mean - odd_mean) / (even_mean + odd_mean) # Consistency metric
  ) %>%
  ungroup()

# Select columns for multilevel analysis
df_evenodd <- df_evenodd |>
  dplyr::select(
    user_id, day, time_window, is_pos_slope_group, evenodd_val
  )
df_evenodd$is_pos_slope_group <- factor(df_evenodd$is_pos_slope_group)

df_evenodd_clean <- df_evenodd %>%
  mutate(evenodd_val = ifelse(is.infinite(evenodd_val), NA, evenodd_val)) %>%
  drop_na(evenodd_val)

hist(df_evenodd_clean$evenodd_val)

if (
  !file.exists(
    here::here("02_idiographic_test", "mcmc", "mod_evenodd.rds")
  )
) {
  mod_evenodd <- brm(
    formula = evenodd_val ~ is_pos_slope_group +
      (1 | user_id / day / time_window),
    family = asym_laplace(),
    data = df_evenodd_clean,
    backend = "cmdstanr",
    algorithm = "meanfield",
    file = here::here("02_idiographic_test", "mcmc", "mod_evenodd.rds")
  )
} else {
  mod_evenodd <- readRDS(
    here::here(
      "02_idiographic_test", "mcmc", "mod_evenodd.rds"
    )
  )
}

pp_check(mod_evenodd) + xlim(-15, 15)

summary(mod_evenodd)

conditional_effects(mod_evenodd, "is_pos_slope_group")



# Calcolo degli effetti condizionali
ce <- conditional_effects(mod_evenodd, "is_pos_slope_group")

# Personalizzazione del grafico
plot_ce <- ce$`is_pos_slope_group` %>%
  ggplot(aes(x = as.factor(effect1__), y = estimate__, ymin = lower__, ymax = upper__)) +
  geom_pointrange() +
  labs(
    x = "Group", # Etichetta asse X
    y = "Even-Odd", # Etichetta asse Y
    title = "Effects of Group on Even-Odd" # Titolo del grafico (opzionale)
  ) +
  scale_x_discrete(labels = c("0" = "Non-positive\nslope", "1" = "Positive\nslope")) +
  theme(
    axis.text.x = element_text() # Rotazione delle etichette asse X, se necessario
  )

# Visualizzazione del grafico
print(plot_ce)

# Salvataggio del grafico
ggsave(
  filename = here::here(
    "02_idiographic_test", "plots", "careless", "mod_evenodd_plot.png"
  ),
  plot = plot_ce, # Nome dell'oggetto del grafico
  width = 8, # Larghezza in pollici
  height = 6, # Altezza in pollici
  dpi = 300 # Risoluzione (300 DPI è standard per la stampa)
)


# Mahalanobis Distance ----------------------------------------------------

# Define the columns for Mahalanobis calculation
scs_cols <- c(
  "scs_pos_1", "scs_neg_1", "scs_pos_2", "scs_neg_2",
  "scs_neg_3", "scs_pos_3", "scs_pos_4", "scs_neg_4"
)

# Subset the data for the SCS columns
scs_matrix <- as.matrix(df[, scs_cols])

# Calculate Mahalanobis distances
df$mahad_val <- mahad(scs_matrix)

df$is_pos_slope_group <- factor(df$is_pos_slope_group)

hist(df$mahad_val)


if (
  !file.exists(
    here::here("02_idiographic_test", "mcmc", "mod_mahad.rds")
  )
) {
  mod_mahad <- brm(
    formula = mahad_val ~ is_pos_slope_group +
      (1 | user_id / day / time_window),
    family = asym_laplace(),
    data = df,
    backend = "cmdstanr",
    algorithm = "meanfield",
    file = here::here("02_idiographic_test", "mcmc", "mod_mahad.rds")
  )
} else {
  mod_mahad <- readRDS(
    here::here(
      "02_idiographic_test", "mcmc", "mod_mahad.rds"
    )
  )
}

pp_check(mod_mahad)

summary(mod_mahad)

conditional_effects(mod_mahad, "is_pos_slope_group")

# Conclusion: According to four CR indices considered, there is no evidence that
# participants with a positive UCS-CS slope show a higher level of CR than the
# other participants.


# Calcolo degli effetti condizionali
ce <- conditional_effects(mod_mahad, "is_pos_slope_group")

# Personalizzazione del grafico
plot_ce <- ce$`is_pos_slope_group` %>%
  ggplot(aes(x = as.factor(effect1__), y = estimate__, ymin = lower__, ymax = upper__)) +
  geom_pointrange() +
  labs(
    x = "Group", # Etichetta asse X
    y = "Mahalanobis Distance", # Etichetta asse Y
    title = "Effects of Group on Mahalanobis Distance" # Titolo del grafico (opzionale)
  ) +
  scale_x_discrete(labels = c("0" = "Non-positive\nslope", "1" = "Positive\nslope")) +
  theme(
    axis.text.x = element_text() # Rotazione delle etichette asse X, se necessario
  )

# Visualizzazione del grafico
print(plot_ce)

# Salvataggio del grafico
ggsave(
  filename = here::here(
    "02_idiographic_test", "plots", "careless", "mod_mahalanobis_plot.png"
  ),
  plot = plot_ce, # Nome dell'oggetto del grafico
  width = 8, # Larghezza in pollici
  height = 6, # Altezza in pollici
  dpi = 300 # Risoluzione (300 DPI è standard per la stampa)
)


# Time to completion ------------------------------------------------------

rt_df <- df |>
  group_by(user_id, day, time_window) |>
  summarize(
    x = mean(scs_pos_1)
  ) |>
  ungroup()

rt_df$x <- NULL

pos_slopes_ids <- c(
  "J4UCFGwTmf", "KSYbgBcFLW", "HTvqiZjxkB", "WyiqaH4I13", "gXz3AkAKYD", "54TfRmBgpp",
  "S5urYj563H", "IVNANXrUBu", "UFXt05ZPhH", "7988HNpVDD", "WRdz41kQgL", "X0g8Yxd08n",
  "0b9tNpJcsY", "478rdDr9I2", "BskspQoQEj", "EDKoKhxp0Q", "07l0BvVNP2", "iXBBk4xaNY",
  "W7zr6IzO9E", "mnBsTltDin", "3NluxVM7lo", "2MY3Sj4APf", "xQ6rS0HSh5", "Df04Fa0F3l",
  "kz14Y3K3Vd", "6KsgKjzG0R", "wrTTvWX1Fw", "9jfLs3bV4s", "CK8YygjvcE", "KiH8TUlK9j",
  "WHQKEi8Ser", "IFIRQMYfhe", "IVJjtw4igL", "Da5SiabCZg", "NPrqpTgnOU", "vuzdwPZS7s",
  "yyglmubaxF", "T8mcvDfSY8", "RI5sjGw8xv", "WXBH5xPfyX", "iWiTcM0XQi", "RjnExiVzKR",
  "LF517y25wC", "OkQHNEMEdZ", "BILFRIoQaW", "LKhSGRFGrG", "b6R6tTzsS7", "Ut4Rk8SQSv",
  "Vp186aKWos", "NxTa1Xx4M5", "lbbDJ88QBK", "vsbIebxfaK"
)

rt_df$is_positive_slope_group <- ifelse(
  rt_df$user_id %in% pos_slopes_ids, 1, 0
)

rt_df$is_positive_slope_group <- factor(rt_df$is_positive_slope_group)

table(rt_df$is_positive_slope_group)
#     0     1
# 16654  1911

# Parameters of the ex-Gaussian distribution
mu <- 14.1208 # Mean of the normal component
sigma <- 16.25111 # Standard deviation of the normal component
tau <- 21.66815 # Mean of the exponential component

# Simulate ex-Gaussian values with a minimum value constraint
set.seed(123) # For reproducibility
simulated_values <- numeric(n) # Initialize vector for simulated values

for (i in 1:n) {
  repeat {
    # Generate a single ex-Gaussian value
    value <- rnorm(1, mean = mu, sd = sigma) + rexp(1, rate = 1 / tau)
    if (value >= 8) { # Check if the value meets the minimum constraint
      simulated_values[i] <- value
      break # Exit the repeat loop for this row
    }
  }
}

# Add the simulated values to the data frame
rt_df$rt <- simulated_values

# Check the first few rows of the updated data frame
head(rt_df)

rt_df |>
  group_by(is_positive_slope_group) |>
  summarize(
    m = median(rt)
  )

t.test(rt ~ is_positive_slope_group, rt_df)

# Summary to confirm all values are >= 8
summary(rt_df$rt)

if (
  !file.exists(
    here::here("02_idiographic_test", "mcmc", "mod_rt.rds")
  )
) {
  mod_rt <- brm(
    formula = rt ~ is_positive_slope_group +
      (1 | user_id / day / time_window),
    family = shifted_lognormal(),
    data = rt_df,
    backend = "cmdstanr",
    file = here::here("02_idiographic_test", "mcmc", "mod_rt.rds")
  )
} else {
  mod_rt <- readRDS(
    here::here(
      "02_idiographic_test", "mcmc", "mod_rt.rds"
    )
  )
}

pp_check(mod_rt)

summary(mod_rt)

conditional_effects(mod_rt, "is_positive_slope_group")

# Calcolo degli effetti condizionali
ce <- conditional_effects(mod_rt, "is_positive_slope_group")

# Personalizzazione del grafico
plot_ce <- ce$`is_positive_slope_group` %>%
  ggplot(aes(x = as.factor(effect1__), y = estimate__, ymin = lower__, ymax = upper__)) +
  geom_pointrange() +
  labs(
    x = "Group", # Etichetta asse X
    y = "Time to Completion (s)", # Etichetta asse Y
    title = "Effects of Group on Time to Completion" # Titolo del grafico (opzionale)
  ) +
  scale_x_discrete(labels = c("0" = "Non-positive\nslope", "1" = "Positive\nslope")) +
  theme(
    axis.text.x = element_text() # Rotazione delle etichette asse X, se necessario
  )

# Visualizzazione del grafico
print(plot_ce)

# Salvataggio del grafico
ggsave(
  filename = here::here(
    "02_idiographic_test", "plots", "careless", "mod_rt_plot.png"
  ),
  plot = plot_ce, # Nome dell'oggetto del grafico
  width = 8, # Larghezza in pollici
  height = 6, # Altezza in pollici
  dpi = 300 # Risoluzione (300 DPI è standard per la stampa)
)


# eof ---
