rawlist_2_df <- function(x, transect = c("MA","ST"),campaign = " ", loc = c("on", "off")){
  output <- test[[transect]] %>%
    bind_rows( ,.id = "id") %>%
    mutate(id = gsub(paste0(".*","_",transect,sep=""),transect,id)) %>%
    mutate(id = gsub("\\..*","",id)) %>%
    rename_with(~ str_extract(.x, "TimeStamp") ,
                matches("TimeStamp")) %>%
    mutate(TimeStamp = gsub("\\..*","",TimeStamp)) %>%
    mutate(TimeStamp = as.POSIXct(TimeStamp, tryFormats = c("%m/%d/%Y %H:%M:%S"))) %>%
    separate(TimeStamp, c("Date", "Time"), sep=" ", remove = FALSE) %>%
    mutate(campaign = campaign) %>%
    mutate(loc_samp = loc)
}

