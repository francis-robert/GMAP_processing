ts_plot <- function(x,grp = c(), analyte = c(" "), units = "", wd_avg_time_min= NULL){
  input <- x %>%
    filter(str_detect(header,"ANALYTE_")) %>%
    mutate(header = gsub("ANALYTE_","",header)) %>%
    filter(!header == "ws" & !header == "wd")
  input_ws_wd_lat_long <- x %>%
    filter(str_detect(header,"ANALYTE_")) %>%
    mutate(header = gsub("ANALYTE_","",header)) %>%
    filter(header=="ws"|header=="wd"|header=="GPS-Latitude"|header=="GPS-Longitude") %>%
    pivot_wider(.,id_cols = TimeStamp,names_from = header)
  header <- analyte
  groupings <- tibble(grp, header)
  print(groupings)
  groupings_1<-groupings %>%
    mutate(header=strsplit(header,","))%>%
    unnest(header)
  input_2<-input %>%
    left_join(.,groupings_1,by="header") %>%
    left_join(.,input_ws_wd_lat_long,by="TimeStamp")
    # filter(header==groupings_1$header)
  input_3 <-input_2 %>%
    filter(!is.na(grp))
  max_plots <-max(input_3$grp)
  if(missing(units)){
    unit<-"\u00b5g/m\u00b3"
  }else{
    unit<-units
  }
  output_ts_plot<-c()
  for(i in unique(input_3$grp)){
    plot_in<-input_3 %>%
      filter(grp==i)
    for (j in unique(input_3$id)){
      plot_in_tran <- plot_in %>%
        filter(id==j)
    plot_ts <- ggplot(data=plot_in_tran,aes(x=TimeStamp,y=value,color=header))+
      geom_line()+
      xlab("Time")+
      ylab(paste0("Analyte Value (",unit,")",sep=""))+
      labs(color="Analyte")+
      # scale_y_continuous(limits=c(0,NA))+
      scale_x_datetime(date_labels ="%d-%m-%Y %H:%M:%S")+
      scale_color_manual(values=c("blue3","darkorange","chartreuse3","firebrick2","blueviolet","orange4","violetred","honeydew4","gold2","turquoise2"),
                         drop=FALSE)+
      geom_hline(aes(yintercept=unique(mdl),linetype="MDL"))+
      geom_hline(aes(yintercept=unique(SQL),linetype="SQL"))+
      scale_linetype_manual("Critical Values",values=c("MDL"="dashed","SQL"="dotted"))+
      theme(axis.text.x = element_text(angle = 90),axis.ticks.x = element_blank())

    time_test<-input_ws_wd_lat_long %>%
      group_by(id) %>%
      mutate(time_interval=floor_date(TimeStamp,unit="hour")+minutes(floor(minute(TimeStamp)/5)*5))

    mean_wd<- time_test %>%
      group_by(time_interval) %>%
      mutate(ones=1) %>%
      mutate(ns=(1/sum(ones)) * sum(sin(wd))) %>%
      mutate(ew=(1/sum(ones)) * sum(cos(wd))) %>%
      mutate(avg_wd=90-atan(ns/ew)) %>%
      distinct(id,.keep_all = T) %>%
      mutate(wd_rad=((avg_wd*-1)+90+180))
    wd_plot_prep <- input_3 %>%
      filter(TimeStamp %in% plot_in_tran$TimeStamp) %>%
      mutate(wd_rad=((wd*-1)+90+180))%>%
      group_by(TimeStamp)%>%
      distinct(TimeStamp,.keep_all = T)

      # ungroup()%>%
      # mutate(direction_section = case_when(wd_rad>=0 & wd_rad <=22.5 ~ "N",
      #                                      wd_rad>22.5 & wd_rad <=67.5 ~ "NE",
      #                                      wd_rad>67.5 & wd_rad <=112.5 ~ "E",
      #                                      wd_rad>112.5 & wd_rad <=157.5 ~ "SE",
      #                                      wd_rad>157.5 & wd_rad <=202.5 ~ "S",
      #                                      wd_rad>202.5 & wd_rad <=247.5 ~ "SW",
      #                                      wd_rad>247.5 & wd_rad <=292.5 ~ "W",
      #                                      wd_rad>292.5 & wd_rad <=337.5 ~ "NW",
      #                                      wd_rad>337.5 & wd_rad <=360 ~ "N",
      #                                      .default = "NA"))%>%
      # # mutate(ws=as.numeric(smooth(ws,kind="3RS3R"))) %>%
      # mutate(date_fact=seq_along(nrow(.)))

    # plot_wd <- ggplot(data=wd_plot_prep,aes(x=TimeStamp,y=ws,group=id,color=direction_section))+
    #   geom_path(size=1)+
    #   scale_y_continuous(labels=label_number(accuracy = 0.1))+
    #   scale_color_manual(values=c("#fbbaa7","#fc906d","#ff580a","#cf4900","#a72f10","#782312","#3d231d","#1c1615")) +
    #   # geom_text(aes(angle=wd_rad),label="→",size=4)+
    #   ylab("Windspeed (m/s)")+
    #   theme_bw()+
    #   theme(axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),legend.position = "none")
    #
    # plot_out <-ggarrange(plot_wd,plot_ts,nrow = 2,ncol = 1,heights = c(0.4,1.25),common.legend = F)
    # output_ts_plot[[j]] <- plot_out
    }
  }
  #output_ts_plot[[max_plots+1]]<-input_3
  return(wd_plot_prep)
}

