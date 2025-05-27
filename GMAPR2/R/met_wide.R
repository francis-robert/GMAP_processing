met_wide <- function (x){
  analyte <- x %>%
    filter(instrument=="Syft-i8-Tracer"|instrument=="Picarro-G2204")

  met <- x %>%
    filter(!instrument=="Syft-i8-Tracer" & !instrument=="Picarro-G2204") %>%
    unite(id_TimeStamp,c("id","TimeStamp"),sep="@",remove = F) %>%
    pivot_wider(.,id_cols = c(id_TimeStamp), values_from=c(value,TimeStamp),
                names_from = header,names_glue = "{header}_{.value}")
  return(met)
}
