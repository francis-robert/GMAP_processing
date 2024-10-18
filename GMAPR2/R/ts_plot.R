ts_plot <- function(x,grp = c(), analyte = c(" "), units = ""){
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
      theme(axis.text.x = element_text(angle = 90),axis.ticks.x = element_blank(),legend.position = "none")
    wd_plot_prep <- input_3 %>%
      filter(TimeStamp %in% plot_in_tran$TimeStamp) %>%
      mutate(wd_rad=((wd*-1)+90+180))%>%
      group_by(TimeStamp)%>%
      distinct(TimeStamp,.keep_all = T) %>%
      ungroup()%>%
      mutate(direction_section = case_when(wd_rad>=0 & wd_rad <=22.5 ~ "N",
                                           wd_rad>22.5 & wd_rad <=67.5 ~ "NE",
                                           wd_rad>67.5 & wd_rad <=112.5 ~ "E",
                                           wd_rad>112.5 & wd_rad <=157.5 ~ "SE",
                                           wd_rad>157.5 & wd_rad <=202.5 ~ "S",
                                           wd_rad>202.5 & wd_rad <=247.5 ~ "SW",
                                           wd_rad>247.5 & wd_rad <=292.5 ~ "W",
                                           wd_rad>292.5 & wd_rad <=337.5 ~ "NW",
                                           wd_rad>337.5 & wd_rad <=360 ~ "N",
                                           .default = "NA"))
      # # # mutate(ws=as.numeric(smooth(ws,kind="3RS3R"))) %>%
      # mutate(date_fact=seq_along(nrow(.)))

    plot_wd <- ggplot(data=wd_plot_prep,aes(x=TimeStamp,y=ws,group=id,color=direction_section))+
      geom_path(linewidth=1)+
      # scale_y_continuous(labels=label_number(accuracy = 0.1))+
      scale_color_manual(values=c("#fbbaa7","#fc906d","#ff580a","#cf4900","#a72f10","#782312","#3d231d","#1c1615")) +
      # geom_text(aes(angle=wd_rad),label="→",size=4)+
      ylab("Windspeed (m/s)")+
      theme_bw()+
      theme(axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),legend.position = "none")

    wind_legend<-readJPEG("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/Picture1.jpg")

    wind_legend_plot<-ggplot() +
      background_image(wind_legend) +
      theme_transparent()

    plot_legend<-get_legend(ggplot(data=plot_in_tran,aes(x=TimeStamp,y=value,color=header))+
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
                              theme(axis.text.x = element_text(angle = 90),axis.ticks.x = element_blank()))

    plot_out <-ggarrange(plot_wd,wind_legend_plot,plot_ts,plot_legend, nrow = 2,ncol = 2,heights = c(0.4,1.25),widths = c(1.5,0.5),common.legend = F)
    output_ts_plot[[j]] <- plot_out
    }
  }
  output_ts_plot[[length(output_ts_plot)+1]]<-input_3
  return(output_ts_plot)
}


