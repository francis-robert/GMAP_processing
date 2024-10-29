transect_max <- function (x){
  output <- c()
  output_short <- x %>%
    filter(.,str_detect(instrument, c("Picarro|Syft"))) %>%
    group_by(campaign, instrument, id, header) %>%
    slice_max(value, n=1) %>%
    slice_max(TimeStamp, n=1) %>%
    mutate(header = gsub("ANALYTE_","",header)) %>%
    pivot_wider(.,id_cols = id,names_from = header)
  output_long <- x %>%
    filter(.,str_detect(instrument, c("Picarro|Syft"))) %>%
    group_by(campaign, instrument, id, header) %>%
    slice_max(value, n=1) %>%
    slice_max(TimeStamp, n=1) %>%
    mutate(header = gsub("ANALYTE_","",header))
  output[["short"]] = output_short
  output[["long"]] = output_long
  write.csv(output[["short"]],paste0(unique(output_long$campaign),"_transect_max_",unique(output_long$loc_samp),".csv"))

  return(output)
}
