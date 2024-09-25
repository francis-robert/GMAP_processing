#' max histograms
#'
#' @param df data frame of flagged GMAP data
#' @param analyte vector of analytes to be included in the analysis
#'
#' @return returns a plot of the maxes for each transect for the vector of analytes provided
#' @export
#'
#' @examples
maxes_hist <- function(df, analyte = c("H2S(ppb)", "CH4(ppm)", "BEN(ppb)",
                                       "TOL(ppb)", "XYP(ppb)")) {
  input <- df %>%
    pivot_longer(., analyte) %>%
    separate(Transect,c("Date","Transect"),remove = F) %>%
    filter(!name== "CH4(ppm)") %>%
    mutate(value = as.numeric(value))
  max_ppb <- max(input$value,na.rm=TRUE) + 2
  input_CH4 <- df %>%
    pivot_longer(., analyte) %>%
    separate(Transect,c("Date","Transect"),remove = F) %>%
    filter(name== "CH4(ppm)")
  max_ppm <- max(input_CH4$value,na.rm=TRUE) + 2
  list_1 <- c()
  list_2 <- c()
  for (i in unique(input$Date)) {
    input_ppb <- input %>%
      filter(Date == i)
    plot_ppb <- ggplot(data = input_ppb, aes(x = Transect, y = value, fill = name)) +
      geom_bar(stat = "identity",position = "dodge") +
      scale_y_continuous(expand = c(0,0),limits = c(0,max_ppb)) +
      ylab("Analyte (ppb)") +
      scale_fill_manual(values = c("green", "cyan", "azure3", "darkolivegreen"),
                        breaks = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")) +
      labs(fill = "Analyte") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
      ggtitle(i)
    list_1[[i]] <- plot_ppb
  }
  for (i in unique(input_CH4$Date)) {
    input_ppm <- input_CH4 %>%
      filter(Date == i)
    plot_ppm <- ggplot(data = input_ppm, aes(x = Transect, y = value, fill = name)) +
      geom_bar(stat = "identity",position = "dodge") +
      scale_y_continuous(expand = c(0,0),limits = c(0,max_ppm)) +
      ylab("Analyte (ppm)") +
      scale_fill_manual(values = c("red")) +
      labs(fill = "Analyte") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
      ggtitle(i)
    list_2[[i]] <- plot_ppm
  }
  return(list(list_1,list_2))

}
