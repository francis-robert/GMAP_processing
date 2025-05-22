splitsville <- function(x) {
  split_list<-c()
  syft <- x %>%
    filter(instrument=="Syft-i8-Tracer") %>%
    unite(id_TimeStamp,c("id","TimeStamp"),sep = "@",remove=F)
  picarro <- x %>%
    filter(instrument=="Picarro-G2204") %>%
    unite(id_TimeStamp,c("id","TimeStamp"),sep = "@",remove=F)
  met_gps <- x %>%
    filter(instrument=="AirMar" |
             instrument == "AliCat-FP-25"|
             instrument=="GPS"|
             instrument=="IonSciencePID") %>%
    unite(id_TimeStamp,c("id","TimeStamp"),sep = "@",remove=F)

  split_list[["syft"]]<-syft
  split_list[["picarro"]]<-picarro
  split_list[["metgps"]]<-met_gps
  return(split_list)
}
