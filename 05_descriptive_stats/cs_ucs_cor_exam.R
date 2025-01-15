suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
  library(here)
  library(rio)
  library(performance)
})


alldata <- readRDS(
  here::here(
    "data", "mpath_data", "ema_data_3.RDS"
  )
)

table(alldata$exam_day)
# no_exam    post     pre
#    6221     284     274


# Analysis for exam_day == "pre" ------------------------------------------

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
    bf(mvbind(SC, USC) ~ 1 + (1 | user_id / day)) + set_rescor(TRUE),
    iter = 8000, warmup = 2000, chains = 4, cores = 4,
    seed = 210191,
    # algorithm = "meanfield",
    backend = "cmdstanr"
  )

summary(f1)
# Residual Correlations:
#                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# rescor(SC,USC)    -0.82      0.06    -0.92    -0.70 1.01      500      883


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


d1 <- no_exam_day |>
  dplyr::select(SC, USC, user_id, day, time_window)

outliers <- check_outliers(d1, method = "mcd", verbose = FALSE)
outliers

d1_clean <- d1[-which(outliers), ]

f2 <-
  brm(
    data = d1_clean,
    family = student,
    bf(mvbind(SC, USC) ~ 1 + (1 | user_id / day / time_window)) + set_rescor(TRUE),
    iter = 2000, warmup = 500, chains = 4, cores = 8,
    seed = 210191,
    # algorithm = "meanfield",
    backend = "cmdstanr"
  )

summary(f2)
# Residual Correlations:
#                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# rescor(SC,USC)    -0.41      0.01    -0.43    -0.38 1.00      912     1038


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

d <- exam_day_post |>
  dplyr::select(SC, USC, user_id, day)

outliers <- check_outliers(d, method = "mcd", verbose = FALSE)
outliers

d_clean <- d[-which(outliers), ]

f3 <-
  brm(
    data = d_clean,
    family = student,
    bf(mvbind(SC, USC) ~ 1 + (1 | user_id / day)) + set_rescor(TRUE),
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
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# rescor(SC,USC)    -0.77      0.05    -0.85    -0.67 1.00     3451     5685
