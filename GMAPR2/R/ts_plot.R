ts_plot <- function(x,y,grp = c(), analyte = c(" "), unit = NULL,
                    wd_time_interval=1,multi_analyte=TRUE, time_labels="60 sec",
                    user_in_top=1,user_in_bottom=1, mdl = NULL,proc=NULL) {
  header <- analyte
  groupings <- tibble(grp, header)
  print(groupings)
  groupings_1<-groupings %>%
    mutate(header=strsplit(header,","))%>%
    unnest(header)
  print(groupings_1)
  if(is.null(unit)){
    unit<-"(ppb)"
  }else{
    unit<-paste0("(",unit,")",sep="")
  }
  print(unit)
  input <- x %>%
    # filter(str_detect(header,"ANALYTE_")) %>%
    # mutate(header = gsub("ANALYTE_","",header)) %>%
    filter(!header == "ws" & !header == "wd") %>%
    left_join(.,groupings_1,by="header") %>%
    drop_na(grp)
  input_ws_wd_lat_long <- y %>%
    select(TimeStamp,id,header,value) %>%
    # filter(str_detect(header,"ANALYTE_")) %>%
    # mutate(header = gsub("ANALYTE_","",header)) %>%
    filter(header=="ws"|header=="wd"|header=="GPS-Latitude"|header=="GPS-Longitude") %>%
    filter(TimeStamp %in% input$TimeStamp) %>%
    pivot_wider(.,id_cols = c(TimeStamp,id),names_from = header)
  # time_test<-input_ws_wd_lat_long %>%
  #   group_by(id) %>%
  #   mutate(time_interval=floor_date(TimeStamp,unit="hour")+minutes(floor(minute(TimeStamp)/wd_time_interval)*wd_time_interval))

  mean_wd <- input_ws_wd_lat_long %>%
    mutate(direction_section = case_when(wd>=0 & wd <=22.5 ~ 360 ,
                                         wd>22.5 & wd <=67.5 ~ 45,
                                         wd>67.5 & wd <=112.5 ~ 90,
                                         wd>112.5 & wd <=157.5 ~ 135,
                                         wd>157.5 & wd <=202.5 ~ 180,
                                         wd>202.5 & wd <=247.5 ~ 225,
                                         wd>247.5 & wd <=292.5 ~ 270,
                                         wd>292.5 & wd <=337.5 ~ 315,
                                         wd>337.5 & wd <=360 ~ 360,
                                         .default = NA)) %>%
    mutate(direction_char = case_when(direction_section == 360 ~ "N",
                                      direction_section == 45 ~ "NE",
                                      direction_section == 90 ~ "E",
                                      direction_section == 135 ~ "SE",
                                      direction_section == 180 ~ "S",
                                      direction_section == 225 ~ "SW",
                                      direction_section == 270 ~ "W",
                                      direction_section == 315 ~ "NW",
                                      .default = "NA")) %>%
    group_by(direction_section) %>%
    mutate(direc_count = n())
    # group_by(time_interval) %>%
    # slice_max(direc_count ,n=1)
    # distinct(time_interval,direc_count,.keep_all = T)

  if (multi_analyte==TRUE){
    input_multi <- input %>%
      unite("id_grp",c(id,grp),sep="-") %>%
      left_join(.,mean_wd,by="TimeStamp") %>%
      group_by(id_grp)

    input_multi_wd_list<- input_multi %>%
      drop_na(direction_section) %>%
      mutate(Time= as.POSIXct(Time, format = "%H:%M:%S")) %>%
      mutate(mins = minute(Time)) %>%
      group_by(id_grp,mins) %>%
      slice_min(mins,n=1,with_ties = F) %>%
      ungroup() %>%
      group_by(id_grp) %>%
      {setNames(group_split(.), group_keys(.)[[1]])}

    plot_out_wd<-input_multi_wd_list %>%
      lapply(.,function(x)
        ggplot(x,aes(x=Time,y=ws,radius=0.0001,angle=-direction_section+90))+
          geom_text(label="→")+
          ylab("WS (m/s)")+
          theme(axis.title.x = element_blank(),axis.ticks.x = element_blank(),
                axis.text.x = element_blank(),legend.position = "none")+
          expand_limits(y=c(max(x$ws)+user_in_top,min(x$ws)-user_in_bottom)))

    input_multi_analyte_list <- input_multi %>%
      mutate(Time= as.POSIXct(Time, format = "%H:%M:%S")) %>%
      {setNames(group_split(.), group_keys(.)[[1]])}


    plot_out_analyte<-input_multi_analyte_list %>%
      lapply(.,function(x)
        ggplot(x, aes(x=Time,y=value,color=header))+
          geom_point()+
          geom_line()+
          scale_color_manual(values=c("blue3","darkorange","chartreuse3",
                                      "firebrick2","blueviolet","orange4",
                                      "violetred","honeydew4","gold2",
                                      "turquoise2"),drop=FALSE,
                             name=gsub("-.*","",unique(x$id_grp)))+
          scale_x_datetime(date_breaks= time_labels,date_labels = ("%H:%M:%S"))+
          ylab(paste0("Analyte Concentration ",unit))+
          xlab("Time"))

    plot_out<-Map(
      function(x,y){ggarrange(x,y, nrow = 2,ncol = 1, heights = c(1,3), common.legend = F,align = "hv")}
      ,plot_out_wd,plot_out_analyte)
  }else{
    input_multi <- input %>%
      unite("id_grp",c(id,grp),sep="-") %>%
      left_join(.,mean_wd,by="TimeStamp") %>%
      group_by(id_grp)

    input_multi_wd_list<- input_multi %>%
    drop_na(direction_section) %>%
    mutate(Time= as.POSIXct(Time, format = "%H:%M:%S")) %>%
      mutate(mins = minute(Time)) %>%
      group_by(id_grp,mins) %>%
      slice_min(mins,n=1,with_ties = F) %>%
      ungroup() %>%
      group_by(id_grp) %>%
      {setNames(group_split(.), group_keys(.)[[1]])}

  plot_out_wd<-input_multi_wd_list %>%
    lapply(.,function(x)
      ggplot(x,aes(x=Time,y=ws,radius=0.0001,angle=-direction_section+90))+
        geom_text(label="→")+
        ylab("WS (m/s)")+
        theme(axis.title.x = element_blank(),axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),legend.position = "left")+
        expand_limits(y=c(max(x$ws)+user_in_top,min(x$ws)-user_in_bottom)))


  input_multi_analyte_list <- input_multi %>%
    filter(header==analyte & syft.procedure==proc) %>%
    mutate(Time= as.POSIXct(Time, format = "%H:%M:%S"))
    # left_join(.,mdl,by=c("header"="analyte","syft.procedure"))
    # {setNames(group_split(.), group_keys(.)[[1]])}



  # plot_out_analyte<-input_multi_analyte_list %>%
  #   lapply(.,function(x)
  #     ggplot(x, aes(x=Time,y=value,color=header))+
  #       geom_point()+
  #       geom_line()+
  #       scale_color_manual(values=c("blue3","darkorange","chartreuse3",
  #                                   "firebrick2","blueviolet","orange4",
  #                                   "violetred","honeydew4","gold2",
  #                                   "turquoise2"),drop=FALSE,
  #                          name=gsub("-.*","",unique(x$id_grp)))+
  #       geom_hline(aes(yintercept=unique(mdl),linetype="MDL"))+
  #       geom_hline(aes(yintercept=unique(sql),linetype="SQL"))+
  #       scale_linetype_manual("Critical Values",values=c("MDL"="aa","SQL"="solid"))+
  #       scale_x_datetime(date_breaks= time_labels,date_labels = ("%H:%M:%S"))+
  #       ylab(paste0("Analyte Concentration ",unit))+
  #       xlab("Time"))
   }
  # plot_out<-Map(
  #   function(x,y){ggarrange(x,y, nrow = 2,ncol = 1, heights = c(1,3), common.legend = T,legend = "bottom",align = "hv")}
  #   ,plot_out_wd,plot_out_analyte)
  return(input_multi)
}
