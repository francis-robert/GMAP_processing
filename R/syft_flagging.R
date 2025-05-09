syft_flagging <- function(x,y){
   mdl_syft <- y %>%
     unite(analyte_proc,c("analyte","procedure"),sep="_",remove=F)

   input <- x %>%
     unite(analyte_proc,c("header","procedure"),sep="_",remove=T) %>%
     left_join(.,mdl_syft, by="analyte_proc")

   output <- input %>%
     mutate(mdl_flag = case_when(value < mdl ~ "MD",
                                  value < sql ~ "SQ",
                                  value < (mdl*-1)~ "ND",
                                  .default = "NA"))
  return(output)
}
