

# center3L() --------------------------------------------------------------

#' Function for centering the data at the level of Moment, Day, Person
center3L <- function(dataname, varname, idname, dayname){
  within = dataname %>% group_by({{idname}},{{dayname}}) %>%
    mutate("{{varname}}_DddM" := mean( {{ varname }}, na.rm=TRUE)) %>%  
    mutate(DddM = mean( {{ varname }}, na.rm=TRUE)) %>%
    mutate("{{varname}}_Moment" := {{ varname }} - mean({{ varname }}, na.rm=TRUE)) %>% ungroup()
  within2 = dataname %>% group_by({{idname}},{{dayname}}) %>%
    summarize(temp_dddm = mean({{ varname }}, na.rm=TRUE)) %>%
    summarize("{{varname}}_PppM" := mean(temp_dddm, na.rm=TRUE)) %>% ungroup()
  within3 = dataname %>% group_by({{idname}},{{dayname}}) %>%
    summarize(temp_dddm = mean({{ varname }}, na.rm=TRUE)) %>%
    summarize(PppM = mean(temp_dddm, na.rm=TRUE)) %>% ungroup()
  combinewithin <- merge(within2, within3)
  allwithin <- merge(within,combinewithin)
  allwithinc = allwithin %>% mutate("{{varname}}_Day" := DddM - PppM)
  between = dataname %>% group_by({{idname}},{{dayname}}) %>%
    summarize(temp_dddm = mean({{ varname }}, na.rm=TRUE)) %>%
    summarize(temp_pppm = mean(temp_dddm, na.rm=TRUE)) %>%
    summarize("{{varname}}_SssM" := mean(temp_pppm, na.rm=TRUE)) %>% ungroup()
  between2 = dataname %>% group_by({{idname}},{{dayname}}) %>%
    summarize(temp_dddm = mean({{ varname }}, na.rm=TRUE)) %>%
    summarize(temp_pppm = mean(temp_dddm, na.rm=TRUE)) %>%
    summarize(SssM = mean(temp_pppm, na.rm=TRUE)) %>% ungroup()
  between3 <- full_join(between, between2, by=character())
  combined <- full_join(allwithinc, between3, by=character())
  output = combined %>% mutate("{{varname}}_Person" := PppM - SssM)
  out <- select(output, -c(DddM, PppM, SssM))
  return(out)
}

# Centering syntax 
# dat <- center3L(dat,pa,id,studyday) #equivalent to: dat <- center3L(dataname = dat,varname = pa, idname = id, dayname = studyday)


# get_data() --------------------------------------------------------------

#' Function for combining the data of the two experiments, cleaning, and scaling.
#'
#' @param reverse_coding_ucs Logical value indicating whether to reverse the coding of UCS items.
#' @return A data frame containing the combined, cleaned, and scaled data.
#'
#' To reverse the coding of UCS, call get_data(reverse_coding_ucs = 1).
#' 
get_data <- function(reverse_coding_ucs = 0) {
  
  study_1_df <- rio::import(here::here("data", "study_1_data.csv")) |>
    dplyr::select(
      "user_id", "day", "time_window", "scs_pos_1",
      "scs_neg_2", "scs_pos_3", "scs_neg_4", "scs_neg_5",
      "scs_pos_6", "scs_pos_7", "scs_neg_8",
      "neg_aff_Moment", "neg_aff_Day", "neg_aff_Person",
      "context_Moment", "context_Day", "context_Person"
    )
  
  # Remove 3 observations with NA on time_window.
  study_1_df <- study_1_df[!is.na(study_1_df$time_window), ]
  
  # Import data from Study 2.
  study_2_temp <- rio::import(here::here("data", "study_2_data.csv"))
  
  # Scale Negative Affect in the same range (1, 5) as in Study 1.
  x_scaled <- function(x) {
    (x - min(x)) / (max(x) - min(x)) * (5 - 1) + 1
  } 
  
  study_2_temp$neg_aff_raw <- study_2_temp$neg_aff
  study_2_temp$neg_aff <- x_scaled(study_2_temp$neg_aff_raw)
  study_2_temp$neg_aff_raw <- NULL
  
  # Add negative affect scaled by occasion, day, person.
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
  df <- bind_rows(study_1_df, study_2_df)
  # length(unique(df$user_id))
  # [1] 495
  
  # Scale negative affect and context
  df$neg_aff_Moment <- scale(df$neg_aff_Moment) |> as.numeric()
  df$neg_aff_Day <- scale(df$neg_aff_Day) |> as.numeric()
  df$neg_aff_Person <- scale(df$neg_aff_Person) |> as.numeric()
  
  df$context_Moment <- scale(df$context_Moment) |> as.numeric()
  df$context_Day <- scale(df$context_Day) |> as.numeric()
  df$context_Person <- scale(df$context_Person) |> as.numeric()
  
  # Reverse coding of UCS items. 
  # For testing the idiographic Stan model, this is not needed.
  if (reverse_coding_ucs == 1) {
    neg_items <- grep("scs_neg_", colnames(df), value = TRUE)
    # Invert the values for all "scs_neg_" columns (assuming the maximum score is 3)
    df[neg_items] <- 3 - df[neg_items]
  }
  
  # Renaming the "scs_pos_" and "scs_neg_" columns.
  colnames(df)[colnames(df) == "scs_pos_1"] <- "scs_pos_1"
  colnames(df)[colnames(df) == "scs_pos_3"] <- "scs_pos_2"
  colnames(df)[colnames(df) == "scs_pos_6"] <- "scs_pos_3"
  colnames(df)[colnames(df) == "scs_pos_7"] <- "scs_pos_4"
  
  colnames(df)[colnames(df) == "scs_neg_2"] <- "scs_neg_1"
  colnames(df)[colnames(df) == "scs_neg_4"] <- "scs_neg_2"
  colnames(df)[colnames(df) == "scs_neg_5"] <- "scs_neg_3"
  colnames(df)[colnames(df) == "scs_neg_8"] <- "scs_neg_4"
  
  return(df)
}





