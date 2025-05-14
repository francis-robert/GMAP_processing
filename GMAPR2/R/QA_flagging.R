groundspeed_flagging <- function (x){
  data <- x %>%
    group_by(TimeStamp) %>%
    mutate(wind_flag <- case_when (header=="Ground-Speed" & value> 30 ~ "QX",
                                   .default))
}
