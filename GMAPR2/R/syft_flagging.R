syft_flagging <- function(x,y=NULL,z=NULL){

  if (!is.null(y)&!is.null(z)){
    print("If you are running Syft flagging prior to Picarro flagging you will need to provide an MDL & HS dataframe, if you have already run picarro flagging and both Syft and Picarro MDLs are in the same sheet you do NOT need to provide an MDL dataframe")
    mdl_hs_df <- y %>%
      left_join(.,z,by=c("analyte","mdl_procedure"="hs_procedure"))
    input <- x %>%
      filter(!analyte_procedure == "NA") %>%
      left_join(.,mdl_hs_df,by=c("header" = "analyte"),relationship = "many-to-many") %>%
      unite(mdl_analyte_procedure_comb, c("header","mdl_procedure"),sep="_", remove = F) %>%
      filter(mdl_analyte_procedure_comb == analyte_procedure) %>%
      mutate(mdl_flag="NA")

    met_data <- x %>%
      filter(analyte_procedure == "NA")

    flagged_mdl <- input %>%
      mutate(mdl_flag = case_when(instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < mdl & value > (mdl*(-1)) ~ "MD",
                                  instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < sql & value > mdl ~ "SQ",
                                  instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < (mdl*(-1))~ "ND",
                                  instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value > hs ~ "EH",
                                  .default = mdl_flag))
    output <- flagged_mdl %>%
      bind_rows(.,met_data)
  }else{
    met_data <- x %>%
      filter(analyte_procedure == "NA")
    
    flagged_mdl <- x %>%
      filter(!analyte_procedure == "NA") %>%
      mutate(mdl_flag = case_when(instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < mdl & value > (mdl*(-1)) ~ "MD",
                                  instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < sql & value > mdl ~ "SQ",
                                  instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value < (mdl*(-1))~ "ND",
                                  instrument=="Syft-i8-Tracer" & analyte_procedure==mdl_analyte_procedure_comb & value > hs ~ "EH",
                                  .default = mdl_flag))
    output <- flagged_mdl %>%
      bind_rows(.,met_data)
   }
  return(output)
}
