rawlist_2_df <- function(x, transect = c("MA","ST"),campaign = " "){
  output <- x[[transect]] %>%
    bind_rows( ,.id = "id") %>%
    mutate(id = gsub('.*/',"\\1",id)) %>%
    mutate(id = gsub("\\..*","",id)) %>%
    rename_with(~ str_extract(.x, "TimeStamp"),
                matches("TimeStamp")) %>%
    mutate(campaign = campaign)

  output_time <- output %>%
    filter(TimeStamp>as.POSIXct("1900-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"))

  output_notime <- output %>%
    filter(TimeStamp<as.POSIXct("1900-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")) %>%
    group_by(id,TimeStamp)%>%
    mutate(TimeStamp = TimeStamp + years(cur_group_id()))

  out_fin <- output_time %>%
    bind_rows(.,output_notime) %>%
    separate(TimeStamp, c("Date", "Time"), sep=" ", remove = FALSE)

  return(out_fin)
}

