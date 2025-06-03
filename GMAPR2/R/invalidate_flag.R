invalidate_flag <- function(x){
  data_invalid <- x %>%
    mutate(bool_invalid = case_when(str_detect(time_mdl_flag,"AV|BJ|AM|AN|AT|AZ|BA|BN") == TRUE ~ 1,
                                    .default = 0)) %>%
    mutate(value = if_else(bool_invalid == 1,NA,value))
}
