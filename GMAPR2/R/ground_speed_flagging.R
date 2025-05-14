groundspeed_flagging <- function (x,flag="NA"){
  data <- x %>%
    mutate(gs_flag = case_when (header=="Ground-Speed" & value > 30 ~ flag,
                                   .default = "NA")) %>%
    mutate(gs_flag = case_when (header=="ws" & is.na(gs_flag)==F ~ flag,
                                .default = gs_flag)) %>%
    mutate(gs_flag = case_when (header=="wd" & is.na(gs_flag)==F ~ flag,
                                .default = gs_flag))
  }
