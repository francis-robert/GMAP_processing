#time_series_figures

time_series_plot <- function (x, analyte = c("H2S(ppb)", "CH4(ppm)", "BEN(ppb)",
                                             "TOL(ppb)", "XYP(ppb)"), all_indiv=c("all","indiv"),
                                  report_limit = c("rl","all"),
                                  h2smdl = NULL, ch4mdl = NULL,benmdl = NULL,
                                  tolmdl = NULL, xypmdl = NULL) {
if(report_limit=="all"){
  if (all_indiv=="all"){
  #ppb_analytes
  list_fig_ppb_all <- c()
  input_ppb <- x %>%
    pivot_longer(., analyte) %>%
    filter(!name== "CH4(ppm)") %>%
    filter(!is.na(value)) %>%
    group_by(Transect,name) %>%
    mutate(Time=as.POSIXct(Time))
  max_ppb_all<-max(input_ppb$value)+2
  #ppm analytes
  list_fig_ppm_all<-c()
  input_ppm <- x %>%
    pivot_longer(., analyte) %>%
    filter(name== "CH4(ppm)") %>%
    filter(!is.na(value)) %>%
    group_by(Transect) %>%
    mutate(Time=as.POSIXct(Time))
  max_ppm_all<-max(input_ppm$value)+2

  for (i in unique(input_ppb$Transect)){
    input_tran_ppb <- input_ppb %>%
      filter(Transect==i)
    plot_all_ppb <- ggplot(data=input_tran_ppb, aes(x=Time,y=value,color=name))+
      geom_line(lwd=1.5)+
      geom_point(size=1.5,pch=21,color="black",aes(fill=name))+
      scale_y_continuous(expand = c(0,0),limits = c(0,max_ppb_all)) +
      ylab("Analyte (ppb)") +
      scale_color_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
                        breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
      scale_fill_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
                         breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
      {if("H2S(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 3*h2smdl,color="green",linetype="dashed")}+
      {if("BEN(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 5*benmdl,color="cyan",linetype="dashed")}+
      {if("TOL(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 5*tolmdl,color="azure3",linetype="dashed")}+
      {if("XYP(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 5*xypmdl,color="darkolivegreen",linetype="dashed")}+
      theme_bw() +
      labs(color = element_blank()) +
      labs(fill = "Analyte")+
      ggtitle(i)

    list_fig_ppb_all[[i]]<-plot_all_ppb
  }
  for (i in unique(input_ppm$Transect)){
    input_tran_ppm <- input_ppm %>%
      filter(Transect==i)
    plot_all_ppm <- ggplot(data=input_tran_ppm, aes(x=Time,y=value))+
      geom_line(lwd=1,color="red")+
      geom_point(size=1.5,pch=21,color="black",fill="red")+
      scale_y_continuous(expand = c(0,0),limits = c(0,max_ppm_all)) +
      {if("CH4(ppm)"%in%input_tran_ppm$name)geom_hline(yintercept = 3*ch4mdl,color="red",linetype="dashed")}+
      ylab("Analyte (ppm)") +
      # scale_color_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
      #                    breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
      # scale_fill_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
      #                   breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
      theme_bw() +
      labs(color = "Analyte")+
      ggtitle(paste("CH4",i))

    list_fig_ppm_all[[i]]<-plot_all_ppm
  }

  return(c(list_fig_ppb_all,list_fig_ppm_all))
}
  if(all_indiv=="indiv"){
#ppb_analytes
  list_fig_ppb_indiv <- c()
  input_ppb <- x %>%
    pivot_longer(., analyte) %>%
    filter(!name== "CH4(ppm)") %>%
    filter(!is.na(value)) %>%
    mutate(Time=as.POSIXct(Time))%>%
    unite(tran_name,c("Transect","name"),remove = F) %>%
    group_by(tran_name)
  max_ppb_all<-max(input_ppb$value)+2

#ppm analytes
  list_fig_ppm_indiv<-c()
  input_ppm <- x %>%
    pivot_longer(., analyte) %>%
    filter(!name== "CH4(ppm)") %>%
    filter(!is.na(value)) %>%
    mutate(Time=as.POSIXct(Time))%>%
    unite(tran_name,c("Transect","name"),remove = F) %>%
    group_by(tran_name)
  max_ppm_all<-max(input_ppm$value)+2
  for(i in unique(input_ppb$tran_name)){
    input_tran_ppb <- input_ppb %>%
      filter(tran_name==i)
    plot_all_ppb <- ggplot(data=input_tran_ppb, aes(x=Time,y=value,color=name))+
      geom_line(lwd=1.5)+
      geom_point(size=1.5,pch=21,color="black",aes(fill=name))+
      scale_y_continuous(expand = c(0,0),limits = c(0,max_ppb_all)) +
      ylab("Analyte (ppb)") +
      scale_color_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
                         breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
      scale_fill_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
                        breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
      {if("H2S(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 3*h2smdl,color="green",linetype="dashed")}+
      {if("BEN(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 5*benmdl,color="cyan",linetype="dashed")}+
      {if("TOL(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 5*tolmdl,color="azure3",linetype="dashed")}+
      {if("XYP(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 5*xypmdl,color="darkolivegreen",linetype="dashed")}+
      theme_bw() +
      labs(color = element_blank()) +
      labs(fill = "Analyte")+
      ggtitle(i)

    list_fig_ppb_indiv[[i]]<-plot_all_ppb
  }
  for (i in unique(input_ppm$Transect)){
    input_tran_ppm <- input_ppm %>%
      filter(Transect==i)
    plot_all_ppm <- ggplot(data=input_tran_ppm, aes(x=Time,y=value))+
      geom_line(lwd=1,color="red")+
      #geom_point(size=1.5,pch=21,color="black",fill="red")+
      scale_y_continuous(expand = c(0,0),limits = c(0,max_ppm_all)) +
      {if("CH4(ppm)"%in%input_tran_ppm$name)geom_hline(yintercept = 3*ch4mdl,color="red",linetype="dashed")}+
      ylab("Analyte (ppm)") +
      # scale_color_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
      #                    breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
      # scale_fill_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
      #                   breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
      theme_bw() +
      labs(color = "Analyte")+
      ggtitle(paste("CH4",i))

    list_fig_ppm_indiv[[i]]<-plot_all_ppm
  }

  return(c(list_fig_ppb_indiv,list_fig_ppm_indiv))}
}
if(report_limit=="rl"){
  if (all_indiv=="all"){
    #ppb_analytes
    list_fig_ppb_all <- c()
    input_ppb <- x %>%
      pivot_longer(., analyte) %>%
      filter(!name== "CH4(ppm)") %>%
      filter(!is.na(value)) %>%
      group_by(Transect,name) %>%
      mutate(Time=as.POSIXct(Time)) %>%
      ungroup()%>%
      group_by(name) %>%
      mutate(above_rl=case_when(name=="H2S(ppb)" & value>=3*h2smdl~1,
                                name=="BEN(ppb)" & value>=5*benmdl~1,
                                name=="TOL(ppb)" & value>=5*tolmdl~1,
                                name=="XYP(ppb)" & value>=5*xypmdl~1,
                                .default=0)) %>%
      filter(above_rl==1)
    max_ppb_all<-max(input_ppb$value)+2
    #ppm analytes
    list_fig_ppm_all<-c()
    input_ppm <- x %>%
      pivot_longer(., analyte) %>%
      filter(name== "CH4(ppm)") %>%
      filter(!is.na(value)) %>%
      mutate(above_rl=case_when(name=="CH4(ppm)" & value>=3*ch4mdl~1,
                                .default=0)) %>%
      filter(above_rl==1) %>%
      group_by(Transect) %>%
      mutate(Time=as.POSIXct(Time))

    max_ppm_all<-max(input_ppm$value)+2

    for (i in unique(input_ppb$Transect)){
      input_tran_ppb <- input_ppb %>%
        filter(Transect==i)
      plot_all_ppb <- ggplot(data=input_tran_ppb, aes(x=Time,y=value,color=name))+
        geom_line(lwd=1.5)+
        #geom_point(size=1.5,pch=21,color="black",aes(fill=name))+
        scale_y_continuous(expand = c(0,0),limits = c(0,max_ppb_all)) +
        ylab("Analyte (ppb)") +
        scale_color_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
                           breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
        scale_fill_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
                          breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
        {if("H2S(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 3*h2smdl,color="green",linetype="dashed")}+
        {if("BEN(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 5*benmdl,color="cyan",linetype="dashed")}+
        {if("TOL(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 5*tolmdl,color="azure3",linetype="dashed")}+
        {if("XYP(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 5*xypmdl,color="darkolivegreen",linetype="dashed")}+
        theme_bw() +
        labs(color = element_blank()) +
        labs(fill = "Analyte")+
        ggtitle(i)

      list_fig_ppb_all[[i]]<-plot_all_ppb
    }
    for (i in unique(input_ppm$Transect)){
      input_tran_ppm <- input_ppm %>%
        filter(Transect==i)
      plot_all_ppm <- ggplot(data=input_tran_ppm, aes(x=Time,y=value))+
        geom_line(lwd=1,color="red")+
        #geom_point(size=1.5,pch=21,color="black",fill="red")+
        scale_y_continuous(expand = c(0,0),limits = c(0,max_ppm_all)) +
        {if("CH4(ppm)"%in%input_tran_ppm$name)geom_hline(yintercept = 3*ch4mdl,color="red",linetype="dashed")}+
        ylab("Analyte (ppm)") +
        # scale_color_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
        #                    breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
        # scale_fill_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
        #                   breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
        theme_bw() +
        labs(color = "Analyte")+
        ggtitle(paste("CH4",i))

      list_fig_ppm_all[[i]]<-plot_all_ppm
    }

    return(c(list_fig_ppb_all,list_fig_ppm_all))
  }
  if(all_indiv=="indiv"){
    #ppb_analytes
    list_fig_ppb_indiv <- c()
    input_ppb <- x %>%
      pivot_longer(., analyte) %>%
      filter(!name== "CH4(ppm)") %>%
      filter(!is.na(value)) %>%
      mutate(Time=as.POSIXct(Time))%>%
      unite(tran_name,c("Transect","name"),remove = F) %>%
      group_by(tran_name)%>%
      ungroup()%>%
      group_by(name) %>%
      mutate(above_rl=case_when(name=="H2S(ppb)" & value>=3*h2smdl~1,
                                name=="BEN(ppb)" & value>=5*benmdl~1,
                                name=="TOL(ppb)" & value>=5*tolmdl~1,
                                name=="XYP(ppb)" & value>=5*xypmdl~1,
                                .default=0)) %>%
      filter(above_rl==1)
    max_ppb_all<-max(input_ppb$value)+2

    #ppm analytes
    list_fig_ppm_indiv<-c()
    input_ppm <- x %>%
      pivot_longer(., analyte) %>%
      filter(name== "CH4(ppm)") %>%
      filter(!is.na(value)) %>%
      mutate(above_rl=case_when(name=="CH4(ppm)" & value>=3*ch4mdl~1,
                                .default=0)) %>%
      filter(above_rl==1) %>%
      mutate(Time=as.POSIXct(Time))%>%
      unite(tran_name,c("Transect","name"),remove = F) %>%
      group_by(tran_name)
    max_ppm_all<-max(input_ppm$value)+2
    for(i in unique(input_ppb$tran_name)){
      input_tran_ppb <- input_ppb %>%
        filter(tran_name==i)
      plot_all_ppb <- ggplot(data=input_tran_ppb, aes(x=Time,y=value,color=name))+
        geom_line(lwd=1.5)+
        geom_point(size=1.5,pch=21,color="black",aes(fill=name))+
        scale_y_continuous(expand = c(0,0),limits = c(0,max_ppb_all)) +
        ylab("Analyte (ppb)") +
        scale_color_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
                           breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
        scale_fill_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
                          breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
        {if("H2S(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 3*h2smdl,color="green",linetype="dashed")}+
        {if("BEN(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 5*benmdl,color="cyan",linetype="dashed")}+
        {if("TOL(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 5*tolmdl,color="azure3",linetype="dashed")}+
        {if("XYP(ppb)"%in%input_tran_ppb$name)geom_hline(yintercept = 5*xypmdl,color="darkolivegreen",linetype="dashed")}+
        theme_bw() +
        labs(color = element_blank()) +
        labs(fill = "Analyte")+
        ggtitle(i)

      list_fig_ppb_indiv[[i]]<-plot_all_ppb
    }
    for (i in unique(input_ppm$Transect)){
      input_tran_ppm <- input_ppm %>%
        filter(Transect==i)
      plot_all_ppm <- ggplot(data=input_tran_ppm, aes(x=Time,y=value))+
        geom_line(lwd=1,color="red")+
        #geom_point(size=1.5,pch=21,color="black",fill="red")+
        scale_y_continuous(expand = c(0,0),limits = c(0,max_ppm_all)) +
        {if("CH4(ppm)"%in%input_tran_ppm$name)geom_hline(yintercept = 3*ch4mdl,color="red",linetype="dashed")}+
        ylab("Analyte (ppm)") +
        # scale_color_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
        #                    breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
        # scale_fill_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
        #                   breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
        theme_bw() +
        labs(color = "Analyte")+
        ggtitle(paste("CH4",i))

      list_fig_ppm_indiv[[i]]<-plot_all_ppm
    }

    return(c(list_fig_ppb_indiv,list_fig_ppm_indiv))}
}
}

