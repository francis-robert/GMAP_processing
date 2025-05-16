subsamp_temporal_syft_zero<- function(x,y){
  if (!unique(x$instrument=="Syft-i8-Tracer")){
    print("NOT Syft data...do not pass go, do not collect $200")
  }else{
    input <-x %>%
      ungroup() %>%
      filter(value==0) %>%
      # filter(str_detect(header,"ANALYTE_")) %>%
      # mutate(header = gsub("ANALYTE_","",header)) %>%
      left_join(.,y,by="id",relationship = "many-to-many") %>%
      group_by(id,header) %>%
      mutate(group_id=cumsum(c(TRUE,diff(TimeStamp)>1))) %>%
      ungroup()%>%
      unite(header_grpid_grpnum,c("id","header","group_id"),sep="_",remove = F) %>%
      group_by(header_grpid_grpnum) %>%
      mutate(group_num=n()) %>%
      mutate(sec_div_cyl=floor(group_num/cyl_time)) %>%
      mutate(cyl_time=as.numeric(cyl_time))

    input_zero_below <- input %>%
      filter(group_num < cyl_time) %>%
      slice_min(TimeStamp) %>%
      ungroup()
    input_2 <- input %>%
      filter(group_num >= cyl_time) %>%
      slice(.,seq(0,n(), by = unique(cyl_time))) %>%
      ungroup()

    output <- input_1 %>%
      bind_rows(input_2)

    input_test <- x %>%
      ungroup() %>%
      filter(value==0) %>%
      # filter(str_detect(header,"ANALYTE_")) %>%
      # mutate(header = gsub("ANALYTE_","",header)) %>%
      left_join(.,y,by="id",relationship = "many-to-many") %>%
      group_by(id,header) %>%
      mutate(group_id=cumsum(c(TRUE,diff(TimeStamp)>1))) %>%
      ungroup()%>%
      unite(header_grpid_grpnum,c("id","header","group_id"),sep="_",remove = F) %>%
      group_by(header_grpid_grpnum) %>%
      mutate(group_num=n()) %>%
      mutate(sec_div_cyl=floor(group_num/cyl_time)) %>%
      mutate(sec_div_cyl=case_when(sec_div_cyl==0~1,
                                   .default = sec_div_cyl)) %>%
      ungroup()%>%
      distinct(header_grpid_grpnum,.keep_all = T)%>%
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
  return(output)
}

