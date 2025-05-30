pic_flagging <- function(x, y, h2shs=NULL,ch4hs=NULL){
  mdl <- y

  data <- x %>%
    left_join(.,mdl, by=c("header" = "analyte")) %>%
    unite(mdl_analyte_procedure_comb, c("header","mdl_procedure"),sep="_", remove = F) %>%
    mutate(mdl_flag = "NA") %>%
    mutate(mdl_flag = case_when(instrument == "Picarro-G2204" & header == "CH4" & value < (-abs(mdl)) ~ "ND",
                                instrument == "Picarro-G2204" & header == "CH4" & value >= (-abs(mdl)) & value <= (abs(mdl)) ~ "MD",
                                instrument == "Picarro-G2204" & header == "CH4" & value > (abs(mdl)) & value <= (abs(3 * mdl)) ~ "PQ",
                                instrument == "Picarro-G2204" & header == "CH4" & value > ch4hs  ~ "EH",
                                instrument == "Picarro-G2204" & header == "H2S" & value < (-abs(mdl)) ~ "ND",
                                instrument == "Picarro-G2204" & header == "H2S" & value >= (-abs(mdl)) & value <= (abs(mdl)) ~ "MD",
                                instrument == "Picarro-G2204" & header == "H2S" & value > (abs(mdl)) & value <= (abs(3 * mdl)) ~ "PQ",
                                instrument == "Picarro-G2204" & header == "H2S" & value > h2shs  ~ "EH",
                                .default = mdl_flag)) %>%
    mutate(value = replace(value, str_detect(mdl_flag, "AV|BJ|AM|AN|AT|AZ|BA|BN|QX"),NA)) %>%
    ungroup()
}
