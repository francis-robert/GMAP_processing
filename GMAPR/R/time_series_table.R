#Time series for Mapping and Stationary transects

time_series_table <- function(x) {
  list_tab <- c()
  input <- x %>%
    pivot_longer(.,cols = analyte) %>%
    na.omit() %>%
    separate(Time,c("Date", "Time_only"), sep = " ",remove = FALSE) %>%
    group_by(Transect) %>%
    mutate(keep = case_when(name=="H2S(ppb)" & H2Sflag == " " ~ 1,
                            name=="CH4(ppm)" & CH4flag == " " ~ 1,
                            name=="BEN(ppb)" & BENflag == " " ~ 1,
                            name=="TOL(ppb)" & TOLflag == " " ~ 1,
                            name=="XYP(ppb)" & XYPflag == " " ~ 1,
                           .default = 0)) %>%
    filter(keep == 1)
  if("H2S(ppb)" %in% input$name) {
    input_h2s <- for(i in unique(Transect)) {
      h2s_list <- input %>%
        filter(name=="H2S(ppb)") %>%
        filter(Transect == i)

      print(input_h2s)
      list_tab[[1]] <- groups(input_h2s)
  }}else {print("No H2S above RL")
  }
  if("CH4(ppm)" %in% input$name) {
    input_ch4 <- input %>%
      filter(name=="CH4(ppm)")
    print(head(input_ch4))
    list_tab[[2]] <- input_ch4
  }else {print("No CH4 above RL")
    }
  if("BEN(ppm)" %in% input$name) {
    input_ben <- input %>%
      filter(name=="BEN(ppm)")
    print(head(input_ben))
    list_tab[[3]] <- input_ben
  }else {print("No BEN above RL")
  }
  if("TOL(ppm)" %in% input$name) {
    input_tol <- input %>%
      filter(name=="TOL(ppm)")
    print(head(input_tol))
    list_tab[[4]] <- input_tol
  }else {print("No TOL above RL")
  }
  if("XYP(ppm)" %in% input$name) {
    input_xyp <- input %>%
      filter(name=="XYP(ppm)")
    print(head(input_xyp))
    list_tab[[5]] <- input_xyp
  }else {print("No XYP above RL")
  }
  return(list_tab)
}
