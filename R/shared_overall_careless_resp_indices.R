#' This function identifies participants with the top 5% highest values across
#' four indices of careless responding: Longstring, IRV (Intra-Response Variability),
#' Even-Odd Consistency, and Mahalanobis Distance. It returns the number of participants
#' who fall within the top 5% in at least two, three, or all four indices.
#'
shared_cr_indices <- function(df) {
  # Compute indices for careless responding

  # Remove three values with NAs on time_window
  df <- df[!is.na(df$time_window), ]

  # Define the columns containing the scale items
  scs_cols <- c(
    "scs_pos_1", "scs_neg_1", "scs_pos_2", "scs_neg_2",
    "scs_neg_3", "scs_pos_3", "scs_pos_4", "scs_neg_4"
  )

  # Longstring ---
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

  # IRV ---
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

  # Even-odd ---
  even_items <- c("scs_pos_1", "scs_pos_2", "scs_pos_3", "scs_pos_4")
  odd_items <- c("scs_neg_1", "scs_neg_2", "scs_neg_3", "scs_neg_4")

  df_evenodd <- df %>%
    group_by(user_id) %>%
    mutate(
      even_mean = rowMeans(across(all_of(even_items)), na.rm = TRUE),
      odd_mean = rowMeans(across(all_of(odd_items)), na.rm = TRUE),
      evenodd_val = 1 - abs(even_mean - odd_mean) / (even_mean + odd_mean)
    ) %>%
    ungroup()

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

  # Mahalanobis distance ---
  scs_matrix <- as.matrix(df[, scs_cols])
  df$mahad_val <- mahad(scs_matrix)

  foo4 <- df |>
    group_by(user_id) |>
    summarize(
      maha = mean(mahad_val)
    )

  foo4$maha |> hist()

  q95_maha <- quantile(foo4$maha, .95)
  maha_bad <- foo4[foo4$maha > q95_maha, ]$user_id

  # Shared indices ---
  vectors <- list(maha_bad, longstring_bad, irv_bad, even_odd_bad)

  shared_counts <- map(2:4, ~ {
    combos <- combn(vectors, .x, simplify = FALSE)
    shared <- map(combos, ~ Reduce(intersect, .x)) %>%
      unlist() %>%
      unique()
    length(shared)
  }) %>% set_names(paste0("shared_by_", 2:4))

  # Calculate subjects exceeding thresholds for more than 2 indices
  df_bad_counts <- tibble(
    user_id = unique(c(maha_bad, longstring_bad, irv_bad, even_odd_bad)),
    maha_bad = user_id %in% maha_bad,
    longstring_bad = user_id %in% longstring_bad,
    irv_bad = user_id %in% irv_bad,
    even_odd_bad = user_id %in% even_odd_bad
  ) %>%
    rowwise() %>%
    mutate(
      total_exceed = sum(c_across(c(maha_bad, longstring_bad, irv_bad, even_odd_bad)))
    ) %>%
    ungroup()

  subjects_exceeding_2 <- df_bad_counts %>%
    filter(total_exceed > 2) %>%
    select(user_id)

  # Output results
  list(
    shared_counts = shared_counts,
    subjects_exceeding_2 = subjects_exceeding_2
  )
}




# shared_cr_indices <- function (df) {
#
#   # Compute indices for careless responding
#
#   # Remove three values with NAs on time_window.
#   df <- df[!is.na(df$time_window), ]
#
#   # Define the columns containing the scale items
#   scs_cols <- c(
#     "scs_pos_1", "scs_neg_1", "scs_pos_2", "scs_neg_2",
#     "scs_neg_3", "scs_pos_3", "scs_pos_4", "scs_neg_4"
#   )
#
#   # Longstring ---
#
#   # Indentify participants with upper 5% of the largest values of the
#   # longstring index.
#
#   df_longstring <- df %>%
#     rowwise() %>%
#     mutate(
#       longstring_val = longstring(matrix(c_across(all_of(scs_cols)), nrow = 1))
#     ) %>%
#     ungroup()
#
#   foo <- df_longstring |>
#     group_by(user_id) |>
#     summarize(
#       lgstr = mean(longstring_val)
#     )
#
#   hist(foo$lgstr)
#
#   q95_longstring <- quantile(foo$lgstr, .95)
#   longstring_bad <- foo[foo$lgstr > q95_longstring, ]$user_id
#
#
#   # IRV ---
#
#   # Indentify participants with upper 5% of the largest values of the
#   # IRV index.
#
#   df_irv <- df |>
#     rowwise() |>
#     mutate(
#       irv_val = irv(matrix(c_across(all_of(scs_cols)), nrow = 1))
#     ) |>
#     ungroup()
#
#   foo2 <- df_irv |>
#     group_by(user_id) |>
#     summarize(
#       sdirv = mean(irv_val)
#     )
#
#   foo2$sdirv |> hist()
#
#   q95_irv <- quantile(foo2$sdirv, .95)
#   irv_bad <- foo2[foo2$sdirv > q95_irv, ]$user_id
#
#
#   # Even-odd ---
#
#   # Indentify participants with upper 5% of the largest values of the
#   # even-odd index.
#
#   # Define even and odd items
#   even_items <- c("scs_pos_1", "scs_pos_2", "scs_pos_3", "scs_pos_4")
#   odd_items <- c("scs_neg_1", "scs_neg_2", "scs_neg_3", "scs_neg_4")
#
#   df_evenodd <- df %>%
#     group_by(user_id) %>%
#     mutate(
#       even_mean = rowMeans(across(all_of(even_items)), na.rm = TRUE),
#       odd_mean = rowMeans(across(all_of(odd_items)), na.rm = TRUE),
#       evenodd_val = 1 - abs(even_mean - odd_mean) / (even_mean + odd_mean)  # Consistency metric
#     ) %>%
#     ungroup()
#
#   # Select columns for multilevel analysis
#   df_evenodd <- df_evenodd |>
#     dplyr::select(
#       user_id, day, time_window, evenodd_val
#     )
#
#   df_evenodd_clean <- df_evenodd %>%
#     mutate(evenodd_val = ifelse(is.infinite(evenodd_val), NA, evenodd_val)) %>%
#     drop_na(evenodd_val)
#
#   foo3 <- df_evenodd_clean |>
#     group_by(user_id) |>
#     summarize(
#       eo = mean(evenodd_val)
#     )
#
#   foo3$eo |> hist()
#
#   q95_eo <- quantile(foo3$eo, .95)
#   even_odd_bad <- foo3[foo3$eo > q95_eo, ]$user_id
#
#   # Mahanobis distance ---
#
#   # Indentify participants with upper 5% of the largest values of the
#   # Mahanobis distance.
#
#   # Subset the data for the SCS columns
#   scs_matrix <- as.matrix(df[, scs_cols])
#
#   # Calculate Mahalanobis distances
#   df$mahad_val <- mahad(scs_matrix)
#
#   foo4 <- df |>
#     group_by(user_id) |>
#     summarize(
#       maha = mean(mahad_val)
#     )
#
#   foo4$maha |> hist()
#
#   q95_maha <- quantile(foo4$maha, .95)
#   maha_bad <- foo4[foo4$maha > q95_maha, ]$user_id
#
#   # Calculate number of participants with extreme values on the CR indices in
#   # more than one category.
#
#   # Convert to lists for easier manipulation
#   vectors <- list(maha_bad, longstring_bad, irv_bad, even_odd_bad)
#
#   # Function to find shared elements across combinations
#   shared_counts <- map(2:4, ~ {
#     combos <- combn(vectors, .x, simplify = FALSE)
#     shared <- map(combos, ~ Reduce(intersect, .x)) %>% unlist() %>% unique()
#     length(shared)
#   }) %>% set_names(paste0("shared_by_", 2:4))
#
#   # Output the results
#   shared_counts
#
# }
