qual_sum <- function(x,y) {
  mdl_sql <- read.csv(y) %>%
    mutate(analyte = paste0("ANALYTE_",analyte))
  data <- x %>%
    left_join(.,mdl_sql,by = c("header" = "analyte")) %>%
    mutate(gtr_sql = if_else(value > SQL,1,0)) %>%
    mutate(gtr_il = if_else(value > IL,1,0)) %>%
    mutate(flagged = if_else(!mdl_qa_flag == "NA"|!time_flag == "NA",1,0)) %>%
    filter(.,str_detect(header,"ANALYTE_")) %>%
    group_by(header,id) %>%
    mutate(sum_tran = 1) %>%
    summarise(across(c(gtr_sql,gtr_il,flagged,sum_tran),sum)) %>%
    mutate(gtr_sql_perc = (gtr_sql/sum_tran)*100) %>%
    mutate(gtr_il_perc = (gtr_il/sum_tran)*100) %>%
    mutate(flagged_perc = (flagged/sum_tran)*100) %>%
    mutate(header = gsub("ANALYTE_","",header)) %>%
    rename("analyte" = "header")
}

