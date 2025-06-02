syft_flagging <- function(x,y=NULL){

  if (!is.null(y)){
    print("If you are running Syft flagging prior to Picarro flagging you will need to provide an MDL dataframe, if you have already run picarro flagging and both Syft and Picarro MDLs are in the same sheet you do NOT need to provide an MDL dataframe")
    input <- x %>%
      filter(!analyte_procedure == "NA") %>%
      left_join(.,y,by=c("header" = "analyte"),relationship = "many-to-many") %>%
      unite(mdl_analyte_procedure_comb, c("header","mdl_procedure"),sep="_", remove = F) %>%
      filter(mdl_analyte_procedure_comb == analyte_procedure) %>%
      mutate(mdl_flag="NA")
    
    met_data <- x %>%
      filter(analyte_procedure == "NA")

    flagged_mdl <- input %>%
      mutate(mdl_flag = case_when(instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < mdl ~ "MD",
                                  instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < sql~ "SQ",
                                  instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < (mdl*(-1))~ "ND",
                                  .default = mdl_flag))
    output <- flagged_mdl %>%
      bind_rows(.,met_data)
  }else{
    input <- x %>%
      filter(!analyte_procedure == "NA") %>%
      unite(mdl_analyte_procedure_comb, c("header","mdl_procedure"),sep="_", remove = F) %>%
      filter(mdl_analyte_procedure_comb == analyte_procedure) %>%
      mutate(mdl_flag="NA")
    
    met_data <- x %>%
      filter(analyte_procedure == "NA")
    
    flagged_mdl <- input %>%
      mutate(mdl_flag = case_when(instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < mdl ~ "MD",
                                  instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < sql~ "SQ",
                                  instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < (mdl*(-1))~ "ND",
                                  .default = mdl_flag))
    output <- flagged_mdl %>%
      bind_rows(.,met_data)
   }
  return(output)
}
