groundspeed_flagging <- function (x,flag="NA"){
  data <- x %>%
    mutate(gs_flag = case_when (header=="Ground-Speed" & value > 30 ~ flag,
                                   .default = "NA")) %>%
    filter(gs_flag == flag)
  
  data_flag <- x %>%
    semi_join(.,data, by = "TimeStamp") %>%
    filter(header == "Ground-Speed" | header == "ws" | header == "wd") %>%
    mutate(gs_flag = flag)
  
  data_noflag <- x %>%
    anti_join(.,data_flag,by = c("TimeStamp","header")) %>%
    mutate(gs_flag = "NA")
  
  data_all <- data_noflag %>%
    bind_rows(.,data_flag)
  }
