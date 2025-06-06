pic_flagging <- function(x, y, z){
  print("Picarro mdl flagging cannot occur after Syft mdl flagging.")
  mdl_hs_df <- y %>%
    left_join(.,z,by=c("analyte","mdl_procedure"="hs_procedure"))
  met_data <- x %>%
    filter(analyte_procedure == "NA")
  data <- x %>%
    filter(!analyte_procedure == "NA") %>%
    left_join(.,mdl_hs_df, by=c("header" = "analyte"),relationship = "many-to-many") %>%
    unite(mdl_analyte_procedure_comb, c("header","mdl_procedure"),sep="_", remove = F) %>%
    filter(mdl_analyte_procedure_comb == analyte_procedure) %>%
    mutate(mdl_flag="NA")
  flagged_mdl_pic <- data %>%
    mutate(mdl_flag = case_when(instrument=="Picarro-G2204" & analyte_procedure==mdl_analyte_procedure_comb & value < mdl & value > (mdl*(-1)) ~ "MD",
                                instrument=="Picarro-G2204" & analyte_procedure==mdl_analyte_procedure_comb & value < sql & value > mdl ~ "SQ",
                                instrument=="Picarro-G2204" & analyte_procedure==mdl_analyte_procedure_comb & value < (mdl*(-1))~ "ND",
                                instrument=="Picarro-G2204" & analyte_procedure==mdl_analyte_procedure_comb & value > hs ~ "EH",
                                .default = mdl_flag))
  output <- flagged_mdl_pic %>%
    bind_rows(.,met_data)

}
