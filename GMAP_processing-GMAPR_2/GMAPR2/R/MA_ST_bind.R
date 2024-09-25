MA_ST_bind <- function(x,y){
  output <- x %>%
    bind_rows(., y) %>%
    separate(name,c("type", "instrument", "residence_time_sec", "units", "header"), sep = "_") %>%
    filter(!header == "Mode") %>%
    mutate(value = as.numeric(value)) %>%
    mutate(header = case_when (str_detect(instrument, c("Picarro|Syft")) ~ paste0("ANALYTE_",header),
                               .default = header))
}
