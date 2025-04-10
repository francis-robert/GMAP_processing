splitsville <- function(x) {
  split_list<-c()
  syft <- x %>%
    filter(instrument=="Syft-i8-Tracer")
  picarro <- x %>%
    filter(instrument=="Picarro-G2204") %>%
    mutate(proc = "picarro")
  gps <- x %>%
    filter(instrument=="GPS")
  met <- x %>%
    filter(instrument=="AirMar" | instrument == "AliCat-FP-25")

  split_list[["syft"]]<-syft
  split_list[["picarro"]]<-picarro
  split_list[["gps"]]<-gps
  split_list[["met"]]<-met
  return(split_list)
}
