transect_max <- function (x){
  output <- x %>%
    filter(.,str_detect(instrument, c("Picarro|Syft"))) %>%
    group_by(campaign, instrument, id, header) %>%
    slice_max(value, n=1) %>%
    slice_max(TimeStamp, n=1) %>%
    mutate(header = gsub("ANALYTE_","",header)) %>%
    select("id","TimeStamp","header","value","time_flag") %>%
    ungroup() %>%
    pivot_wider(.,id_cols=id,names_from = header)
  #write.csv(output,paste0(unique(output$campaign),"_transect_max_",unique(output$loc_samp),".csv"))
  return(output)
}
