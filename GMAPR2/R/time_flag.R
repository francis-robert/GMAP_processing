time_flagging <- function(x, timestart = " ", timestop = " ", timeqt = " ",
                      analyte = " ", procedure = " ") {
  analyte_vec <- unique(analyte)
  print(analyte_vec)

  inter_df <- x %>%
    mutate(if_else(str_detect(colnames(x),"time_flag") == TRUE, paste0(time_flag), time_flag == "NA"))

  # df_change <- x %>%
  #   filter(., str_detect(header, paste(analyte_vec, collapse="|"))) %>%
  #   filter(TimeStamp >= timestart & TimeStamp <= timestop) %>%
  #   mutate(time_flag = timeqt)
  #     # mutate(paste0(analyte_vec[i],"_time_flag" = case_when(TimeStamp >= timestart & TimeStamp <= timestop & header == analyte_vec[i] ~ timeqt,
  #     #                              .default = "NA"))
  #
  # df_no_change <- x %>%
  #   filter(., !str_detect(header, paste(analyte_vec, collapse="|"))) %>%
  #   filter(TimeStamp < timestart | TimeStamp > timestop) %>%
  return(inter_df)
  }
