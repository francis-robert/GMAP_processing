MA_ST_nobind <- function(x){
  output <- x %>%
    #bind_rows(., y) %>%
    separate(name,c("type", "instrument", "residence_time_sec", "units", "header"), sep = "_") %>%
    filter(!header == "Mode") %>%
    mutate(value = as.numeric(value)) %>%
    mutate(header = case_when (str_detect(instrument, c("Picarro|Syft")) ~ paste0("ANALYTE_",header),
                               str_detect(header, c("GPS-Longitude|GPS-Latitude")) ~ paste0("ANALYTE_",header),
                               str_detect(header, c("Wind-Speed|Wind-Direction")) ~ paste0("ANALYTE_",header),
                               .default = header)) %>%
    filter(!header == "Latitude" & !header == "Longitude") %>%
    mutate(header = gsub("Wind-Direction","wd",header)) %>%
    mutate(header = gsub("Wind-Speed","ws",header))
}
