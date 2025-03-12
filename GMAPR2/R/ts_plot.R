ts_plot <- function(x,grp = c(), analyte = c(" "), unit = NULL, wd_time_interval=1,multi_analyte=TRUE, time_labels=" ") {
  header <- analyte
  groupings <- tibble(grp, header)
  print(groupings)
  groupings_1<-groupings %>%
    mutate(header=strsplit(header,","))%>%
    unnest(header)
  if(is.null(unit)){
    unit<-"\u00b5g/m\u00b3"
  }else{
    unit<-unit
  }
  print(unit)
  input <- x %>%
    filter(str_detect(header,"ANALYTE_")) %>%
    mutate(header = gsub("ANALYTE_","",header)) %>%
    filter(!header == "ws" & !header == "wd") %>%
    left_join(.,groupings_1,by="header") %>%
    drop_na(grp)
  input_ws_wd_lat_long <- x %>%
    select(TimeStamp,id,header,value) %>%
    filter(str_detect(header,"ANALYTE_")) %>%
    mutate(header = gsub("ANALYTE_","",header)) %>%
    filter(header=="ws"|header=="wd"|header=="GPS-Latitude"|header=="GPS-Longitude") %>%
    pivot_wider(.,id_cols = c(TimeStamp,id),names_from = header)
  time_test<-input_ws_wd_lat_long %>%
    group_by(id) %>%
    mutate(time_interval=floor_date(TimeStamp,unit="hour")+minutes(floor(minute(TimeStamp)/wd_time_interval)*wd_time_interval))
  mean_wd<- time_test %>%
    group_by(time_interval) %>%
    mutate(ones=1) %>%
    mutate(ns=(1/sum(ones)) * sum(sin(wd))) %>%
    mutate(ew=(1/sum(ones)) * sum(cos(wd))) %>%
    mutate(avg_wd=90-atan(ns/ew)) %>%
    distinct(id,.keep_all = T) %>%
    mutate(wd_rad=((avg_wd*-1)+90+180)) %>%
    select(TimeStamp,ws,wd,time_interval,ns,ew,avg_wd,wd_rad)
  if (multi_analyte==TRUE){
    input_multi <- input %>%
      unite("id_grp",c(id,grp)) %>%
      left_join(.,mean_wd,by="TimeStamp") %>%
      group_by(id_grp)
    input_multi_wd_list<- input_multi %>%
      drop_na(wd_rad) %>%
      {setNames(group_split(.), group_keys(.)[[1]])}

    plot_out_wd<-input_multi_wd_list %>%
      lapply(.,function(x)
        ggplot(x,aes(x=Time,y=ws))+
          geom_text(aes(angle=wd_rad),label="→",size=7)+
          ylab("WS (m/h)")+
          theme(axis.title.x = element_blank(),axis.ticks.x = element_blank(),
                axis.text.x = element_blank(),legend.position = "none"))

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
                           name=gsub("_.*","",unique(x$id_grp)))+
        scale_x_datetime(date_breaks= time_labels,date_labels = ("%H:%M:%S"))+
        ylab(paste0("Analyte Concentration ",unit))+
        xlab("Time"))

  plot_out<-Map(
    function(x,y){ggarrange(x,y, nrow = 2,ncol = 1, heights = c(1,3), common.legend = T,legend = "bottom",align = "hv")}
    ,plot_out_wd,plot_out_analyte)
  }else{input_multi <- input %>%
    unite("id_grp",c(id,grp)) %>%
    left_join(.,mean_wd,by="TimeStamp") %>%
    group_by(id_grp)
  input_multi_wd_list<- input_multi %>%
    drop_na(wd_rad) %>%
    {setNames(group_split(.), group_keys(.)[[1]])}

  plot_out_wd<-input_multi_wd_list %>%
    lapply(.,function(x)
      ggplot(x,aes(x=Time,y=ws))+
        geom_text(aes(angle=wd_rad),label="→",size=7)+
        ylab("WS (m/h)")+
        theme(axis.title.x = element_blank(),axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),legend.position = "none"))

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
                           name=gsub("_.*","",unique(x$id_grp)))+
        geom_hline(aes(yintercept=unique(mdl),linetype="MDL"))+
        geom_hline(aes(yintercept=unique(SQL),linetype="SQL"))+
        scale_linetype_manual("Critical Values",values=c("MDL"="dashed","SQL"="dotted"))+
        scale_x_datetime(date_breaks= time_labels,date_labels = ("%H:%M:%S"))+
        ylab(paste0("Analyte Concentration ",unit))+
        xlab("Time"))
  }
  plot_out<-Map(
    function(x,y){ggarrange(x,y, nrow = 2,ncol = 1, heights = c(1,3), common.legend = T,legend = "bottom",align = "hv")}
    ,plot_out_wd,plot_out_analyte)
  return(plot_out)
}

