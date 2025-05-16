MA_ST_bind <- function(x,y){
  output <- x %>%
    separate(name,c("type", "instrument", "residence_time_sec", "units", "header"), sep = "_") %>%
    filter(!header == "Mode") %>%
    mutate(value = as.numeric(value)) %>%
    filter(!header == "Latitude" & !header == "Longitude") %>%
    mutate(header = gsub("Wind-Direction","wd",header)) %>%
    mutate(header = gsub("Wind-Speed","ws",header)) %>%
    left_join(.,y, by= "id") %>%
    mutate(analyte_procedure= case_when(
      instrument=="Syft-i8-Tracer" ~ paste0(header,"_",transect_procedure),
      instrument=="Picarro-G2204" ~ paste0(header,"_picarro"),
      !instrument=="Syft-i8-Tracer" & !instrument=="Picarro-G2204" ~ "NA"))
 return(output)
  }
