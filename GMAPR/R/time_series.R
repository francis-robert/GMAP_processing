#Time series for Mapping and Stationary transects

#' Title
#'
#' @param x
#' @param analyte
#'
#' @return
#' @export
#'
#' @examples
time_series <- function(x, analyte = c("H2S(ppb)","CH4(ppm)","BEN(ppb)",
                                       "TOL(ppb)", "XYP(ppb)")) {
  analyte <- c(analyte)
  print(analyte)
  input <- x %>%
    pivot_longer(.,cols = analyte) %>%
    na.omit() %>%
    separate(Time,c("Date","Time_only"), sep = " ") %>%
    group_by(Transect) %>%
    mutate(keep = case_when(name=="H2S(ppb)" & H2Sflag == " " ~ 1,
                            name=="CH4(ppm)" & CH4flag == " " ~ 1,
                            name=="BEN(ppb)" & BENflag == " " ~ 1,
                            name=="TOL(ppb)" & TOLflag == " " ~ 1,
                            name=="XYP(ppb)" & XYPflag == " " ~ 1,
                           .default = 0)) %>%
    filter(keep == 1)
  if("H2S(ppb)" %in% input$name) {
    input_h2s <- input %>%
      filter(name=="H2S(ppb)")
    print(input_h2s)
  }else {print("No H2S above RL")
    }
  print(input)
  # list_1 <- c()
  #   for(i in unique(input$Transect)) {
  #     if("H2S(ppb)" %in% input$name) {
  #     h2s_sub <- input %>%
  #       filter(name == "H2S(ppb)")
  #     h2s_plot <- ggplot(h2s_sub, aes(x = Time_only, y = value))+
  #       geom_line()+
  #       geom_point(pch=21, fill="green")+
  #       xlab("Time")+
  #       ylab("H2S (ppb)")+
  #       ggtitle(paste(i))
  #     list_1[[i]] <- h2s_plot
  #   }
  #     else {
  #     print("H2S not present in data")
  #       }
  #     if("CH4(ppm)" %in% input$name) {
  #     ch4_sub <- input %>%
  #       filter(name =="CH4(ppm)")
  #     ch4_plot <- ggplot(ch4_sub, aes(x = Time_only, y = value))+
  #       geom_line()+
  #       geom_point(pch=21, fill="pink")+
  #       xlab("Time")+
  #       ylab("CH4 (ppb)")+
  #       ggtitle(paste(i))
  #     list_1[[i]] <- ch4_plot
  #     }
  #     else {
  #     print("CH4 not present in data")
  #       }
  #     if("BEN(ppb)" %in% input$name) {
  #     ben_sub <- input %>%
  #       filter(name =="CH4(ppb)")
  #     ben_plot <- ggplot(ben_sub, aes(x = Time_only, y = value))+
  #       geom_line()+
  #       geom_point(pch=21, fill="blue")
  #     list_1[[i]] <- ben_plot
  #     }
  #     else {
  #     print("BEN not present in data")
  #       }
  #     if("TOL(ppb)" %in% input$name) {
  #     tol_sub <- input %>%
  #       filter(name =="CH4(ppb)")
  #     tol_plot <- ggplot(tol_sub, aes(x = Time_only, y = value))+
  #       geom_line()+
  #       geom_point(pch=21, fill="yellow")
  #     list_1[[i]] <- tol_plot
  #     }
  #     else {
  #     print("TOL not present in data")
  #       }
  #     if("XYP(ppb)" %in% input$name) {
  #       xyp_sub <- input %>%
  #         filter(name =="CH4(ppb)")
  #       xyp_plot <- ggplot(xyp_sub, aes(x = Time_only, y = value))+
  #         geom_line()+
  #         geom_point(pch=21, fill="purple")
  #       list_1[[i]] <- xyp_plot
  #       }
  #     else {
  #       print("XYP not present in data")
  #     }
  #     }
  # return(list_1)


}
