gps_precision_flagging <- function(x, flag="NA"){
  data <- x %>%
    mutate(gps_flag = case_when (header=="GPS-DoP" & value >  10~ flag,
                                 .default = "NA"))
}
