subsamp_temporal_syft<- function(x){
  if (!unique(x$instrument=="Syft-i8-Tracer")){
    print("NOT Syft data...do not pass go, do not collect $200")
  }else{
    input_1 <-x %>%
      filter(!value==0) %>%
      ungroup() %>%
      # left_join(.,y,by=c("id","cyl_time")) %>%
      group_by(header) %>%
      arrange(TimeStamp) %>%
      mutate(time_grp=rleid(value)) %>%
      group_by(header,time_grp) %>%
      mutate(group_id=n()) %>%
      mutate(within_group_number=row_number()) %>%
      unite("analyte_timegrp_idgrp",c("header","time_grp","group_id"),sep="_",remove = F) %>%
      ungroup() %>%
      group_by(analyte_timegrp_idgrp) %>%
      mutate(sec_div_cyl = floor(group_id/cyl_time)) %>%
      ungroup()%>%
      arrange(TimeStamp) %>%
      group_by(analyte_timegrp_idgrp) %>%
      filter(!sec_div_cyl>1) %>%
      mutate(keep=case_when(sec_div_cyl < 1 & within_group_number == 1 ~ 1,
                            sec_div_cyl == 1 & within_group_number == cyl_time ~ 1,
                            .default = 0)) %>%
      filter(keep==1)


    input_2 <-x %>%
      filter(!value==0) %>%
      ungroup() %>%
      # left_join(.,y,by=c("id","cyl_time")) %>%
      group_by(header) %>%
      arrange(TimeStamp) %>%
      mutate(time_grp=rleid(value)) %>%
      group_by(header,time_grp) %>%
      mutate(group_id=n()) %>%
      mutate(within_group_number=row_number()) %>%
      unite("analyte_timegrp_idgrp",c("header","time_grp","group_id"),sep="_",remove = F) %>%
      ungroup() %>%
      group_by(analyte_timegrp_idgrp) %>%
      mutate(sec_div_cyl = floor(group_id/cyl_time)) %>%
      ungroup()%>%
      arrange(TimeStamp) %>%
      group_by(analyte_timegrp_idgrp) %>%
      filter(sec_div_cyl>1) %>%
      slice(.,seq(0,n(),by=unique(cyl_time))) %>%
      mutate(keep=1)

    input_zero <-x %>%
      ungroup() %>%
      filter(value==0) %>%
      # left_join(.,y,by=c("id","cyl_time"),relationship = "many-to-many") %>%
      group_by(id,header) %>%
      mutate(time_grp=cumsum(c(TRUE,diff(TimeStamp)>1))) %>%
      mutate(group_id=n()) %>%
      mutate(within_group_number=row_number()) %>%
      ungroup()%>%
      unite("analyte_timegrp_idgrp",c("header","time_grp","group_id"),sep="_",remove = F) %>%
      ungroup() %>%
      mutate(sec_div_cyl=floor(group_id/cyl_time)) %>%
      mutate(cyl_time=as.numeric(cyl_time))

    input_zero_below <- input_zero %>%
      filter(group_id < cyl_time) %>%
      slice_min(TimeStamp) %>%
      ungroup() %>%
      mutate(keep=1)
    input_zero_above <- input_zero %>%
      filter(group_id >= cyl_time) %>%
      group_by(analyte_timegrp_idgrp) %>%
      slice(.,seq(0,n(), by = unique(cyl_time))) %>%
      ungroup() %>%
      mutate(keep=1)

    out_zero <- input_zero_below %>%
      bind_rows(input_zero_above)

    output <- input_1 %>%
      bind_rows(input_2)%>%
      bind_rows(out_zero) %>%
      filter(keep==1)

    t<-nrow(output)
    r<-nrow(x)
    e<-unique(x$cyl_time)

    print(t/(r/e))


 }

  return(output)
}
