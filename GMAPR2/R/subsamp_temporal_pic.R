subsamp_temporal_pic<- function(x){
  if (!unique(x$instrument=="Picarro-G2204")){
    print("NOT Picarro data...do not pass go, do not collect $200")
  }else{
    output <-x %>%
      ungroup() %>%
      filter(!value==0) %>%
      filter(str_detect(header,"ANALYTE_")) %>%
      mutate(header = gsub("ANALYTE_","",header)) %>%
      group_by(header)%>%
      arrange(TimeStamp) %>%
      mutate(time_grp=rleid(value)) %>%
      group_by(header,time_grp) %>%
      mutate(group_id=n()) %>%
      unite("analyte_timegrp_idgrp",c("header","time_grp","group_id"),sep="_",remove = F) %>%
      mutate(cyl_time = 3) %>%
      ungroup() %>%
      group_by(analyte_timegrp_idgrp) %>%
      mutate(sec_div_cyl = floor(group_id/cyl_time)) %>%
      slice(.,seq(0, n(), by = unique(cyl_time))) %>%
      ungroup()

    input_test<-x %>%
      ungroup() %>%
      filter(!value==0) %>%
      filter(str_detect(header,"ANALYTE_")) %>%
      mutate(header = gsub("ANALYTE_","",header)) %>%
      group_by(header)%>%
      arrange(TimeStamp) %>%
      mutate(time_grp=rleid(value)) %>%
      group_by(header,time_grp) %>%
      mutate(group_id=n()) %>%
      unite("analyte_timegrp_idgrp",c("header","time_grp","group_id"),sep="_",remove = F) %>%
      mutate(cyl_time = 3) %>%
      ungroup() %>%
      # group_by(analyte_timegrp_idgrp) %>%
      mutate(sec_div_cyl = floor(group_id/cyl_time)) %>%
      distinct(analyte_timegrp_idgrp,.keep_all = T) %>%
      summarise(total=sum(sec_div_cyl))

    if(nrow(output)/input_test$total>1){
      print("Subset data has more rows than raw")
      print(paste("Output Row Count =",nrow(output)))
      print(paste("Column Total Seconds/Cycle Time=", input_test$total))
      print(paste("Ratio Output Row Count/ (Column Total Seconds/Cycle Time) =",nrow(output)/input_test$total))
    }else{
      print(paste("Ratio Output Row Count/ (Column Total Seconds/Cycle Time) =",nrow(output)/input_test$total))
      print(paste("Output Row Count =",nrow(output)))
      print(paste("Column Total Seconds/Cycle Time =", input_test$total))
    }

}

  return(input_test)
}
