syft_flagging <- function(x,y){
   mdl_syft <- y %>%
     unite(analyte_proc,c("analyte","syft.procedure"),sep="_",remove=F)

   input <- x %>%
     unite(analyte_proc,c("header","procedure"),sep="_",remove=F) %>%
     left_join(.,mdl_syft, by="analyte_proc")

   output <- input %>%
     # mutate(mdl=as.numeric(mdl)) %>%
     # mutate(sql=as.numeric(sql)) %>%
     # filter(!is.na(value))
     mutate(mdl_flag = case_when(value < `syft.mdl` ~ "MD",
                                  value < `syft.sql` ~ "SQ",
                                  value < (`syft.mdl`*(-1))~ "ND",
                                  .default = mdl_flag))
  return(output)
}
