syft_flagging <- function(x,y=NULL){

  if (!is.null(y)){
    print("If you are running Syft flagging prior to Picarro flagging you will need to provide an MDL dataframe, if you have already run picarro flagging and both Syft and Picarro MDLs are in the same shee you do NOT need to provide an MDL dataframe")
    input <- x %>%
      left_join(.,y,by=c("header" = "analyte"),relationship = "many-to-many") %>%
      unite(mdl_analyte_procedure_comb, c("header","mdl_procedure"),sep="_", remove = F) %>%
      mutate(mdl_flag="NA")


    output <- input %>%
      mutate(mdl_flag = case_when(instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < mdl ~ "MD",
                                  instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < sql~ "SQ",
                                  instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < (mdl*(-1))~ "ND",
                                  .default = mdl_flag))
  }else{
   input <- x %>%
     unite(mdl_analyte_procedure_comb, c("header","mdl_procedure"),sep="_", remove = F)

   output <- input %>%
     mutate(mdl_flag = case_when(instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < mdl ~ "MD",
                                 instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < sql~ "SQ",
                                 instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < (mdl*(-1))~ "ND",
                                 .default = mdl_flag))
   }
  return(output)
}
