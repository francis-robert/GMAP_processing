#' Time series plot
#'
#' @param x flagged analyte data frame
#' @param analyte vector of analytes
#'
#' @return returns plots for each data/ transect combo in the plotting window
#' @export
#'
#' @examples
time_series_plot <- function (x, analyte = c("H2S(ppb)", "CH4(ppm)", "BEN(ppb)",
                                             "TOL(ppb)", "XYP(ppb)")) {
  list_fig_ppb_all <- c()
  input_ppb <- x %>%
    pivot_longer(., analyte) %>%
    filter(!name== "CH4(ppm)") %>%
    filter(!is.na(value)) %>%
    group_by(Transect,name) %>%
    mutate(Time=as.POSIXct(Time))
  max_ppb_all<-max(input_ppb$value)+2

  for (i in unique(input_ppb$Transect)){
    input_tran_ppb <- input_ppb %>%
      filter(Transect==i)
    plot_all_ppb <- ggplot(data=input_tran_ppb, aes(x=Time,y=value,color=name))+
      geom_line(lwd=1)+
      geom_point(size=1.5,pch=21,color="black",aes(fill=name))+
      scale_y_continuous(expand = c(0,0),limits = c(0,max_ppb_all)) +
      ylab("Analyte (ppb)") +
      scale_color_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
                        breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
      scale_fill_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
                         breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
      theme_bw() +
      labs(color = element_blank()) +
      labs(fill = "Analyte")+
      ggtitle(i)

    list_fig_ppb_all[[i]]<-plot_all_ppb
  }

  list_fig_ppm_all<-c()

  input_ppm <- x %>%
    pivot_longer(., analyte) %>%
    filter(name== "CH4(ppm)") %>%
    filter(!is.na(value)) %>%
    group_by(Transect) %>%
    mutate(Time=as.POSIXct(Time))

  max_ppm_all<-max(input_ppm$value)+2

  for (i in unique(input_ppm$Transect)){
    input_tran_ppm <- input_ppm %>%
      filter(Transect==i)
    plot_all_ppm <- ggplot(data=input_tran_ppm, aes(x=Time,y=value))+
      geom_line(lwd=1,color="red")+
      #geom_point(size=1.5,pch=21,color="black",fill="red")+
      scale_y_continuous(expand = c(0,0),limits = c(0,max_ppm_all)) +
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

