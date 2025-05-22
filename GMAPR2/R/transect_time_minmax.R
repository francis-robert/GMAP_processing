transect_time_minmax <-function (x){
  output <- x %>%
    filter(TimeStamp > as.POSIXct("1900-01-01 00:00:01", format = "%Y-%m-%d %H:%M:%S"))%>%
    group_by(campaign, id) %>%
    summarise(min = min(TimeStamp),
              max = max(TimeStamp)) %>%
    mutate(time_diff=max-min)
  return(output)
}
