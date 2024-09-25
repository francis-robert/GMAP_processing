transect_time_minmax <-function (x){
  output <- x %>%
    group_by(campaign, id) %>%
    summarise(min = min(TimeStamp),
              max = max(TimeStamp))
  return(output)
}
