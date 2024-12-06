pic_flagging <- function(x, y, h2shs=NULL,ch4hs=NULL){
  mdl <- y %>%
    mutate(analyte = paste0("ANALYTE_",analyte))
  
  data <- x %>%
    left_join(.,mdl, by=c("header" = "analyte")) %>%
    mutate(mdl_flag = "NA") %>%
    group_by(instrument, header) %>%
    #rowwise() %>%
    mutate(mdl_flag = case_when(instrument == "Picarro-G2204" & header == "ANALYTE_CH4" & value < (-abs(mdl)) ~ "ND",
                                instrument == "Picarro-G2204" & header == "ANALYTE_CH4" & value >= (-abs(mdl)) & value <= (abs(mdl)) ~ "MD",
                                instrument == "Picarro-G2204" & header == "ANALYTE_CH4" & value > (abs(mdl)) & value <= (abs(3 * mdl)) ~ "PQ",
                                instrument == "Picarro-G2204" & header == "ANALYTE_CH4" & value > ch4hs  ~ "EH",
                                instrument == "Picarro-G2204" & header == "ANALYTE_H2S" & value < (-abs(mdl)) ~ "ND",
                                instrument == "Picarro-G2204" & header == "ANALYTE_H2S" & value >= (-abs(mdl)) & value <= (abs(mdl)) ~ "MD",
                                instrument == "Picarro-G2204" & header == "ANALYTE_H2S" & value > (abs(mdl)) & value <= (abs(3 * mdl)) ~ "PQ",
                                instrument == "Picarro-G2204" & header == "ANALYTE_H2S" & value > h2shs  ~ "EH",
                                .default = "NA")) %>%
    mutate(qa_flag = case_when(instrument == "Picarro-G2204" & TimeStamp == as.POSIXct("0001-01-01 00:00:01", format = "%Y-%m-%d %H:%M:%S")~"DA",
                               .default = "NA")) %>%
    unite("mdl_qa_flag", c("mdl_flag", "qa_flag"), sep = ",") %>%
    mutate(mdl_qa_flag = gsub("NA,","",mdl_qa_flag)) %>%
    mutate(mdl_qa_flag = gsub(",NA","",mdl_qa_flag)) %>%
    mutate(mdl_qa_flag = gsub("NA","",mdl_qa_flag)) %>%
    mutate(value = replace(value, str_detect(mdl_qa_flag, "AV|BJ|AM|AN|AT|AZ|BA|BN|QX|DA"),NA)) %>%
    ungroup()
}
