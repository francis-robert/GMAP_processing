time_flagging <- function(x, timestart = " ", timestop = " ", timeqt = " ",
                          analyte = " ", procedure = " ") {
  analyte_vec <- paste0("ANALYTE_",unique(analyte))
  print(analyte_vec)
  timestart <- as.POSIXct(timestart,tryFormats = c("%m/%d/%Y %H:%M:%S"))
  print(timestart)
  timestop <- as.POSIXct(timestop,tryFormats = c("%m/%d/%Y %H:%M:%S"))

  if (all(!str_detect(colnames(x),"time_flag"))){
    inter_df <- x %>%
      mutate(time_flag = "NA") %>%
      mutate(rn = row_number())
    print("Time Flagging Column Added")
  }else{
    inter_df <- x %>%
      mutate(rn = row_number())
    print("Time Flagging Column Already Present")}
  df_change <- inter_df %>%
    filter(., str_detect(header, paste(analyte_vec, collapse="|"))) %>%
    filter(TimeStamp >= timestart & TimeStamp <= timestop) %>%
    mutate(time_flag = paste(time_flag,timeqt,sep = ","))

  df_no_change <- subset(inter_df, !(rn %in% df_change$rn))


  output <- df_change %>%
    bind_rows(.,df_no_change) %>%
    select(-rn)%>%
    mutate(time_flag = gsub("NA,","",time_flag))

  return(output)
}
