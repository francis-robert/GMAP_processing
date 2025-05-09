output_csv_data <- function (x,y=NULL, output_type=c("syft", "picarro", "met", "all")){
  if (is.null(y)==F){ met <- y %>%
    mutate(mdl_flag = "NA") %>%
    filter(header=="ws"|header=="wd"|header=="GPS-Latitude"|header=="GPS-Longitude"|
             header=="TVOC"|header=="Ambient-Volumetric-Flow"|header=="Ground-Speed")
  }

  if(output_type=="met") {
    output <- x

    check <-ifelse(output$instrument=="Syft-i8-Tracer"|output$instrument == "Picarro-G2204",
                   print("Non-meterological data (ie analytes) provided"),
                   print("Only meterological data provided!"))

    return(output)}
  if(output_type=="picarro"){

    min_time <- min(x$TimeStamp)

    analyte_min_time <- x %>%
      filter(TimeStamp==min_time)


    out_analyte <- x %>%
      bind_rows(.,met) %>%
      group_by(id,header) %>%
      arrange(desc(TimeStamp)) %>%
      mutate(ingroup_num=row_number()) %>%
      pivot_wider(.,id_cols = c(id,ingroup_num), values_from=c(value,TimeStamp),
                  names_from = header,names_glue = "{header}_{.value}") %>%
      select(id,contains("_value"), paste0(analyte_min_time$header,"_TimeStamp")) %>%
      rename("TimeStamp"= paste0(analyte_min_time$header,"_TimeStamp"))

    out_flag <- x %>%
      bind_rows(.,met) %>%
      # mutate(analyte_flag=paste0(header,"_flag",sep="")) %>%
      group_by(id,header) %>%
      arrange(desc(TimeStamp)) %>%
      mutate(ingroup_num=row_number()) %>%
      ungroup()%>%
      select(id,TimeStamp,ingroup_num,header,contains("_flag")) %>%
      mutate(mdl_flag=case_when(is.na(mdl_flag)==T ~ "NA",
                                .default = mdl_flag)) %>%
      mutate(gs_flag=case_when(is.na(gs_flag)==T ~ "NA",
                                 .default = gs_flag)) %>%
      mutate(time_flag=case_when(is.na(time_flag)==T ~ "NA",
                                 .default = time_flag)) %>%
      mutate(gps_flag=case_when(is.na(gps_flag)==T ~ "NA",
                                .default = gps_flag)) %>%
      unite("flag", c("mdl_flag","time_flag","gs_flag","gps_flag"),sep = ",") %>%
      filter(!flag=="NA,NA,NA,NA") %>%
      pivot_wider(.,id_cols = c(id,ingroup_num),names_from = header,
                  values_from=c(flag,TimeStamp),names_glue = "{header}_{.value}") %>%
      select(id,contains("_flag"), paste0(analyte_min_time$header,"_TimeStamp")) %>%
      rename("TimeStamp"= paste0(analyte_min_time$header,"_TimeStamp")) %>%
      unite(wind_flag, c("ws_flag","wd_flag"))

    # out_loc <- met %>%
    #   filter(header=="GPS-Latitude"|header=="GPS-Longitude") %>%
    #   filter(TimeStamp %in% out_analyte$TimeStamp) %>%
    #   select(TimeStamp,id, header, value, gps_flag) %>%
    #   pivot_wider(.,id_cols = TimeStamp, names_from = header, values_from = c(value,gps_flag),
    #               names_glue = "{header}_{.value}")
    #   # unite(gps_flag, c("GPS-Latitude_flag","GPS-Longitude_flag"))
    #
    # output <- out_analyte %>%
    #   left_join(.,out_flag, by=c("TimeStamp","id")) %>%
    #   left_join(.,out_loc, by=c("TimeStamp"))
    #   relocate(id,TimeStamp,GPS-Latitude_value,GPS-Longitude_value,
    #            gps_flag, ws_value,wd_value, wind_flag,H2S_value,H2S_flag,
    #            CH4_value,CH4_flag,)


    return(out_flag)
  }


#
  #   ungroup() %>%
  #   unite("flag",c(mdl_flag, time_flag), sep = ",", remove = FALSE) %>%
  #   mutate(flag = gsub("NA,","",flag)) %>%
  #   mutate(flag = gsub("NA","",flag)) %>%
  #   mutate(flag = gsub(", NA","",flag)) %>%
  #   filter(.,str_detect(instrument, c("Picarro|Syft"))|str_detect(header, c("ws|wd|GPS-Longitude|GPS-Latitude"))) %>%
  #   group_by(instrument) %>%
  #   mutate(header = paste0(header, "_flag")) %>%
  #   mutate(value = flag) %>%
  #   ungroup () %>%
  #   select(TimeStamp, header, value) %>%
  #   pivot_wider(.,id_cols = TimeStamp, names_from = header)
  # output_inter <- x %>%
  #   ungroup() %>%
  #   filter(!instrument == "AliCat-FP-25" & !header== "GPS-Time") %>%
  #   filter(loc_samp == loc) %>%
  #   select(TimeStamp, header, value) %>%
  #   pivot_wider(.,id_cols = TimeStamp, names_from = header)
  # output_inter_2 <- x %>%
  #   ungroup() %>%
  #   filter(!instrument == "AliCat-FP-25" & !header== "GPS-Time") %>%
  #   filter(loc_samp == loc) %>%
  #   select(-header, -value,-mdl_flag,-time_flag)
  # output_fin <- output_inter_2 %>%
  #   left_join(.,output_inter,by="TimeStamp") %>%
  #   left_join(.,output_flag_data,by="TimeStamp") %>%
  #   filter(.,str_detect(instrument, c("Picarro|Syft")))
  # write.csv(output_fin,paste0(unique(output_fin$campaign),"_",unique(output_fin$loc_samp),".csv"))
  # return(output_fin)
  }
