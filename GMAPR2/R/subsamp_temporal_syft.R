subsamp_temporal_syft<- function(x,y){
  if (!unique(x$instrument=="Syft-i8-Tracer")){
    print("NOT Syft data...do not pass go, do not collect $200")
  }else{
  input <-x %>%
    ungroup() %>%
    filter(!value==0) %>%
    # filter(str_detect(header,"ANALYTE_")) %>%
    # mutate(header = gsub("ANALYTE_","",header)) %>%
    left_join(.,y,by="id",relationship = "many-to-many") %>%
    group_by(header)%>%
    arrange(TimeStamp) %>%
    mutate(time_grp=rleid(value)) %>%
    group_by(header,time_grp) %>%
    mutate(group_id=n()) %>%
    unite("analyte_timegrp_idgrp",c("header","time_grp","group_id"),sep="_",remove = F) %>%
    ungroup() %>%
    group_by(analyte_timegrp_idgrp)

  input_test <- input %>%
    mutate(sec_div_cyl=floor(group_id/cyl_time)) %>%
    mutate(sec_div_cyl=case_when(sec_div_cyl==0~1,
                               .default = sec_div_cyl)) %>%
    distinct(analyte_timegrp_idgrp,.keep_all = T) %>%
    ungroup() %>%
    summarise(total=sum(sec_div_cyl))

  filter_clean<-input %>%
    filter(group_id==cyl_time | group_id==cyl_time+1 | group_id==cyl_time-1) %>%
    slice_min(TimeStamp) %>%
    mutate("sec_div_cyl"=floor(group_id/cyl_time)) %>%
    ungroup()

  filter_dirty_below<-input %>%
    filter(!group_id==cyl_time & !group_id==cyl_time+1 & !group_id==cyl_time-1) %>%
    filter(group_id < cyl_time) %>%
    slice_min(TimeStamp) %>%
    mutate("sec_div_cyl"=floor(group_id/cyl_time)) %>%
    ungroup()

  filter_dirty_above<-input %>%
    filter(!group_id==cyl_time & !group_id==cyl_time+1 & !group_id==cyl_time-1) %>%
    filter(group_id > cyl_time+1) %>%
    mutate("sec_div_cyl"=floor(group_id/cyl_time))

  filter_dirty_above_1 <- filter_dirty_above %>%
    filter(sec_div_cyl==1) %>%
    slice_min(TimeStamp) %>%
    ungroup()

  filter_dirty_above_multi <- filter_dirty_above %>%
    filter(!sec_div_cyl==1) %>%
    arrange(TimeStamp) %>%
    # mutate(short_subset=group_id-cyl_time) %>%
    # slice(.,seq(1, unique(short_subset))) %>%
    slice(.,seq(0, n(), by = unique(cyl_time))) %>%
    ungroup()

  output <- filter_clean %>%
    bind_rows(.,filter_dirty_below,filter_dirty_above_1,
              filter_dirty_above_multi)

  }
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

  return(output)
}
