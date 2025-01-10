
library(mice)
library(multilevLCA)

# Longstring Index per occasion ------------------------------------------

df_longstring <- df_clean %>%
  rowwise() %>%
  mutate(longstring_val = longstring(matrix(c_across(all_of(scs_cols)), nrow = 1))) %>%
  ungroup()

# IRV Index per occasion -------------------------------------------------

df_irv <- df_clean %>%
  rowwise() %>%
  mutate(irv_val = irv(as.data.frame(t(c_across(all_of(scs_cols)))), na.rm = TRUE, split = FALSE, num.split = 3)) %>%
  ungroup()

# Even-Odd Consistency per occasion ---------------------------------------

even_items <- c("scs_pos_1", "scs_pos_2", "scs_pos_3", "scs_pos_4")
odd_items <- c("scs_neg_1", "scs_neg_2", "scs_neg_3", "scs_neg_4")

df_evenodd <- df_clean %>%
  rowwise() %>%
  mutate(
    even_mean = mean(c_across(all_of(even_items)), na.rm = TRUE),
    odd_mean = mean(c_across(all_of(odd_items)), na.rm = TRUE),
    evenodd_val = 1 - abs(even_mean - odd_mean) / (even_mean + odd_mean)
  ) %>%
  ungroup() %>%
  mutate(evenodd_val = ifelse(is.infinite(evenodd_val), NA, evenodd_val)) %>%
  drop_na(evenodd_val)

# Mahalanobis Distance per occasion --------------------------------------

df_mahad_clean <- df_clean %>%
  filter(rowSums(is.na(select(., all_of(scs_cols)))) < length(scs_cols))

df_mahad <- df_mahad_clean %>%
  mutate(mahad_val = mahad(as.matrix(select(., all_of(scs_cols)))))

# Merge all indices ------------------------------------------------------

df_cr_indices <- df_clean %>%
  select(user_id, day, time_window) %>%
  distinct() %>%
  left_join(df_longstring %>% select(user_id, day, time_window, longstring_val), by = c("user_id", "day", "time_window")) %>%
  left_join(df_irv %>% select(user_id, day, time_window, irv_val), by = c("user_id", "day", "time_window")) %>%
  left_join(df_evenodd %>% select(user_id, day, time_window, evenodd_val), by = c("user_id", "day", "time_window")) %>%
  left_join(df_mahad %>% select(user_id, day, time_window, mahad_val), by = c("user_id", "day", "time_window"))

# Final Output -----------------------------------------------------------

# Preview the first few rows of CR indices
head(df_cr_indices)


# Perform multiple imputation
imputed_cr_data <- mice(df_cr_indices, m = 5, maxit = 50, method = 'pmm', seed = 123)

# Complete the data by pooling imputations
completed_cr_indices <- complete(imputed_cr_data)

# Final Output -----------------------------------------------------------

# Preview the imputed dataset
head(completed_cr_indices)


# Bin continuous variables into 3 categories (0, 1, 2)
completed_cr_indices$longstring_cat <- cut(completed_cr_indices$longstring_val, breaks = 3, labels = 0:2)
completed_cr_indices$irv_cat <- cut(completed_cr_indices$irv_val, breaks = 3, labels = 0:2)
completed_cr_indices$evenodd_cat <- cut(completed_cr_indices$evenodd_val, breaks = 3, labels = 0:2)
completed_cr_indices$mahad_cat <- cut(completed_cr_indices$mahad_val, breaks = 3, labels = 0:2)







Y = colnames(completed_cr_indices)[8:11]
data = completed_cr_indices
iT = 3

out = multiLCA(data, Y, iT, Z = "user_id", extout = TRUE)
out

plot(out)

