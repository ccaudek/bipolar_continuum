get_data <- function() {
  
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
  
  return(bind_rows(study_1_df, study_2_df))
}

data_cleaning <- function(df) {
  
  # Identify the columns that contain "scs_neg_" in their names
  neg_items <- grep("scs_neg_", colnames(df), value = TRUE)
  
  # Invert the values for all "scs_neg_" columns (assuming the maximum score is 3)
  df[neg_items] <- 3 - df[neg_items]
  
  # Check that the positive and negative dimensions are inversely related
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
  
  # Invert the coding for the negative Self-Compassion scales
  df$scs_neg_1 <- df$scs_neg_1 - 3
  df$scs_neg_2 <- df$scs_neg_2 - 3
  df$scs_neg_3 <- df$scs_neg_3 - 3
  df$scs_neg_4 <- df$scs_neg_4 - 3
  
  return(df)
  
}


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


