output_csv_data <- function (x, output_type=c("syft", "picarro", "met", "all")){

  out_begin <- x %>%
    select(id,Date,Time,TimeStamp,`GPS-Latitude`,`GPS-Longitude`,gps_flag,ws,gs_flag,
           wd) %>%
    mutate(wd_flag = gs_flag) %>%
    rename("ws_flag"="gs_flag")

  analyte_min <-x %>%
    slice_min(TimeStamp,n=1)

  analytes_values <- x %>%
    select(TimeStamp,header,value,id) %>%
    group_by(id,header)%>%
    mutate(ingroup_num=row_number()) %>%
    ungroup()%>%
    pivot_wider(.,id_cols = c(id,ingroup_num),names_from = header,
                 values_from = value)

  analyte_timestamp<-x %>%
    select(TimeStamp,header,id) %>%
    group_by(id,header)%>%
    mutate(ingroup_num=row_number()) %>%
    ungroup()%>%
    arrange(TimeStamp) %>%
    filter(header==analyte_min$header)

  analyte_out <- analytes_values %>%
    left_join(.,analyte_timestamp,by=c("ingroup_num","id"))

  # out <- out_begin %>%
  #   left_join(.,)

   }

