output_csv_data <- function (x, loc= c("on", "off")){
  output_flag_data <- x %>%
    ungroup() %>%
    unite("flag",c(mdl_flag, time_flag), sep = ",", remove = FALSE) %>%
    mutate(flag = gsub("NA,","",flag)) %>%
    mutate(flag = gsub("NA","",flag)) %>%
    mutate(flag = gsub(", NA","",flag)) %>%
    filter(.,str_detect(instrument, c("Picarro|Syft"))) %>%
    group_by(instrument) %>%
    mutate(header = paste0(header, "_flag")) %>%
    mutate(value = flag) %>%
    ungroup () %>%
    select(TimeStamp, header, value) %>%
    pivot_wider(.,id_cols = TimeStamp, names_from = header)
  output_inter <- x %>%
    ungroup() %>%
    filter(!instrument == "AliCat-FP-25" & !header== "GPS-Time") %>%
    filter(loc_samp == loc) %>%
    select(TimeStamp, header, value) %>%
    pivot_wider(.,id_cols = TimeStamp, names_from = header)
  output_inter_2 <- x %>%
    ungroup() %>%
    filter(!instrument == "AliCat-FP-25" & !header== "GPS-Time") %>%
    filter(loc_samp == loc) %>%
    select(-header, -value,-mdl_flag,-time_flag)
  output_fin <- output_inter_2 %>%
    left_join(.,output_inter,by="TimeStamp") %>%
    left_join(.,output_flag_data,by="TimeStamp") %>%
    filter(.,str_detect(instrument, c("Picarro|Syft")))
  write.csv(output_fin,paste0(unique(output_fin$campaign),"_",unique(output_fin$loc_samp),".csv"))
  return(output_fin)
  }
