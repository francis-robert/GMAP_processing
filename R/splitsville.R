splitsville <- function(x) {
  split_list<-c()
  syft <- x %>%
    filter(instrument=="Syft-i8-Tracer")
  picarro <- x %>%
    filter(instrument=="Picarro-G2204") %>%
    mutate(proc = "picarro")
  met_gps <- x %>%
    filter(instrument=="AirMar" | instrument == "AliCat-FP-25"|instrument=="GPS")

  split_list[["syft"]]<-syft
  split_list[["picarro"]]<-picarro
  split_list[["metgps"]]<-met_gps
  return(split_list)
}
