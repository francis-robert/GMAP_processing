subsamp_temporal_pic<- function(x,cyl_time){
  if (!unique(x$instrument=="Picarro-G2204")){
    print("NOT Picarro data...do not pass go, do not collect $200")
  }else{
    input_1 <-x %>%
      ungroup() %>%
      group_by(header)%>%
      arrange(TimeStamp) %>%
      mutate(time_grp=rleid(value)) %>%
      group_by(header,time_grp) %>%
      mutate(group_id=n()) %>%
      mutate(within_group_number=row_number()) %>%
      unite("analyte_timegrp_idgrp",c("header","time_grp","group_id"),sep="_",remove = F) %>%
      mutate(cyl_time = 3) %>%
      ungroup() %>%
      group_by(analyte_timegrp_idgrp) %>%
      mutate(sec_div_cyl = floor(group_id/cyl_time)) %>%
      ungroup()%>%
      arrange(TimeStamp) %>%
      group_by(analyte_procedure,within_group_number) %>%
      filter(!sec_div_cyl>1) %>%
      mutate(keep=case_when(sec_div_cyl < 1 & within_group_number==1~ 1,
                            sec_div_cyl == 1 & within_group_number == cyl_time ~1,
                            .default = 0)) %>%
      filter(keep==1) %>%
      mutate(time_grp=as.character(time_grp))

    input_2<- x %>%
      ungroup() %>%
      group_by(header)%>%
      arrange(TimeStamp) %>%
      mutate(time_grp=rleid(value)) %>%
      group_by(header,time_grp) %>%
      mutate(group_id=n()) %>%
      mutate(within_group_number=row_number()) %>%
      unite("analyte_timegrp_idgrp",c("header","time_grp","group_id"),sep="_",remove = F) %>%
      mutate(cyl_time = 3) %>%
      ungroup() %>%
      group_by(analyte_timegrp_idgrp) %>%
      mutate(sec_div_cyl = floor(group_id/cyl_time)) %>%
      ungroup()%>%
      arrange(TimeStamp) %>%
      group_by(analyte_procedure,within_group_number) %>%
      filter(sec_div_cyl>1) %>%
      slice(.,seq(0,n(),by=unique(cyl_time))) %>%
      mutate(keep=1) %>%
      unite(time_grp,c("time_grp","within_group_number"),sep = "_")

     output <- input_1 %>%
       bind_rows(.,input_2) %>%
       ungroup()

 print(nrow(output)/(nrow(x)/cyl_time))

     output_wide <- output %>%
       # unite(unique_id,c("analyte_procedure","time_grp","within_group_number"),sep="_",remove = F)%>%
       # distinct(.,.keep_all = T) %>%
       pivot_wider(.,id_cols=time_grp,names_from = analyte_procedure,
                   values_from =c(value,time_mdl_flag,TimeStamp))
}

  return(output_wide)
}
