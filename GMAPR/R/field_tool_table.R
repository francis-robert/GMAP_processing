field_tool_table <- function(path = " ", trantype = " ", rl = NULL,
                             analyte = c("H2S", "CH4", "BEN", "TOL", "XYP")) {
  rl_df <- data.frame(c(rl), c(analyte))
  colnames(rl_df) <- c("rl", "analyte")
  list_ <- c()
  if (trantype == "MA") {
    files <- list.files(path = path, pattern = "MA", full.names = TRUE)
    input <- lapply(files, FUN = read.table,
                    skip = 7, header = TRUE, sep = "\t", fill = TRUE,
                    stringsAsFactors = FALSE, na.strings = "NaN",
                    col.names = c("Time", "CO2", "CH4", "H2S", "C2H2",
                                  "M3DSU", "M3DSV", "M3DSW",
                                  "M3DS2DSpeed", "M3DS3DSpeed",
                                  "M3DSAzimuth", "M3DSElevation",
                                  "M3DSSoS", "M3DSSonicTemp",
                                  "M3DSErrorCode", "GPSTrack",
                                  "GPSGroundSpeed", "GPSTime",
                                  "GPSLatitude", "GPSLongitude", "GPSDoP",
                                  "BEN", "TOL", "ETB", "XYO", "XYM",
                                  "XYP", "SO2", "STY", "FOR", "NH3", "NO",
                                  "NO2", "O3", "CellPressure",
                                  "CellTemperature", "SUMMAPressure",
                                  "VOC", "AirMar.Wind.Speed..m.s.",
                                  "AirMar.Wind.Direction..TRUE.",
                                  "AirMarAirTemp[oC]",
                                  "AirMarRelHumidity[%]",
                                  "AirMarBarometer[mBar]",
                                  "AirMarHeading",
                                  "AirMarGroundSpeed[MPH]",
                                  "AirMar.Latitude",
                                  "AirMar.Longitude")) %>%
      bind_rows(., .id = "Transect") %>%
      mutate(Transect = as.numeric(Transect)) %>%
      mutate(Transect = files[Transect]) %>%
      mutate(Transect = gsub(".*/", "", Transect)) %>%
      mutate(Transect = gsub("\\..*", "", Transect))
    print(head(input))
    list_[[1]] <- input

    max_tab <- input %>%
      group_by(Transect) %>%
      mutate(MAX_H2S = case_when(is.element("H2S", analyte) ~ max(H2S))) %>%
      mutate(MAX_CH4 = case_when(is.element("CH4", analyte) ~ max(CH4))) %>%
      mutate(MAX_BEN = case_when(is.element("BEN", analyte) ~ max(BEN))) %>%
      mutate(MAX_TOL = case_when(is.element("TOL", analyte) ~ max(TOL))) %>%
      mutate(MAX_XYP = case_when(is.element("XYP", analyte) ~ max(XYP))) %>%
      ungroup() %>%
      distinct(Transect, .keep_all = TRUE) %>%
      dplyr::select(Transect, MAX_H2S, MAX_CH4, MAX_BEN, MAX_TOL, MAX_XYP)
    print(max_tab)
    list_[[2]] <- max_tab

    precent_rl <- input %>%
      group_by(Transect) %>%
      mutate(h2s_grt = ifelse(is.element("H2S", analyte) &
                                H2S > subset(rl_df, analyte == "H2S",
                                             select = rl), 1, 0)) %>%
      mutate(ch4_grt = ifelse(is.element("CH4", analyte) &
                                CH4 > subset(rl_df, analyte == "CH4",
                                             select = rl), 1, 0)) %>%
      mutate(ben_grt = ifelse(is.element("BEN", analyte) &
                                BEN > subset(rl_df, analyte == "BEN",
                                             select = rl), 1, 0)) %>%
      mutate(tol_grt = ifelse(is.element("TOL", analyte) &
                                TOL > subset(rl_df, analyte == "TOL",
                                             select = rl), 1, 0)) %>%
      mutate(xyp_grt = ifelse(is.element("XYP", analyte) &
                                XYP > subset(rl_df, analyte == "XYP",
                                             select = rl), 1, 0)) %>%
      summarise(h2s_grt = sum(h2s_grt), ch4_grt = sum(ch4_grt),
                ben_grt = sum(ben_grt), tol_grt = sum(tol_grt),
                xyp_grt = sum(xyp_grt))

    precent_rl_2 <- input %>%
      group_by(Transect) %>%
      reframe(cnt = n())

    grt_rl <- left_join(precent_rl, precent_rl_2, by = "Transect") %>%
      mutate(h2s_per = (h2s_grt / cnt) * 100) %>%
      mutate(ch4_per = (ch4_grt / cnt) * 100) %>%
      mutate(ben_per = (ben_grt / cnt) * 100) %>%
      mutate(tol_per = (tol_grt / cnt) * 100) %>%
      mutate(xyp_per = (xyp_grt / cnt) * 100) %>%
      dplyr::select(h2s_per, ch4_per, ben_per, tol_per, xyp_per)
    print(grt_rl)
    list_[[3]] <- grt_rl
  }
  if (trantype == "ST") {
    files <- list.files(path = path, pattern = "ST", full.names = TRUE)
    input <- lapply(files, FUN = read.table,
                    skip = 33, header = TRUE, sep = "\t", fill = TRUE,
                    stringsAsFactors = FALSE, na.strings = "NaN",
                    col.names = c("Time", "CO2", "CH4", "H2S", "C2H2",
                                  "M3DSU", "M3DSV", "M3DSW",
                                  "M3DS2DSpeed", "M3DS3DSpeed",
                                  "M3DSAzimuth", "M3DSElevation",
                                  "M3DSSoS", "M3DSSonicTemp",
                                  "M3DSErrorCode", "GPSTrack",
                                  "GPSGroundSpeed", "GPSTime",
                                  "GPSLatitude", "GPSLongitude", "GPSDoP",
                                  "BEN", "TOL", "ETB", "XYO", "XYM",
                                  "XYP", "SO2", "STY", "FOR", "NH3", "NO",
                                  "NO2", "O3", "CellPressure",
                                  "CellTemperature", "SUMMAPressure",
                                  "VOC", "AirMar.Wind.Speed..m.s.",
                                  "AirMar.Wind.Direction..TRUE.",
                                  "AirMarAirTemp[oC]",
                                  "AirMarRelHumidity[%]",
                                  "AirMarBarometer[mBar]",
                                  "AirMarHeading",
                                  "AirMarGroundSpeed[MPH]",
                                  "AirMar.Latitude",
                                  "AirMar.Longitude")) %>%
      bind_rows(., .id = "Transect") %>%
      mutate(Transect = as.numeric(Transect)) %>%
      mutate(Transect = files[Transect]) %>%
      mutate(Transect = gsub(".*/", "", Transect)) %>%
      mutate(Transect = gsub("\\..*", "", Transect))
    print(head(input))
    list_[[1]] <- input

    max_tab <- input %>%
      group_by(Transect) %>%
      mutate(MAX_H2S = case_when(is.element("H2S", analyte) ~ max(H2S))) %>%
      mutate(MAX_CH4 = case_when(is.element("CH4", analyte) ~ max(CH4))) %>%
      mutate(MAX_BEN = case_when(is.element("BEN", analyte) ~ max(BEN))) %>%
      mutate(MAX_TOL = case_when(is.element("TOL", analyte) ~ max(TOL))) %>%
      mutate(MAX_XYP = case_when(is.element("XYP", analyte) ~ max(XYP))) %>%
      ungroup() %>%
      distinct(Transect, .keep_all = TRUE) %>%
      dplyr::select(Transect, MAX_H2S, MAX_CH4, MAX_BEN, MAX_TOL, MAX_XYP)
    print(max_tab)
    list_[[2]] <- max_tab

    precent_rl <- input %>%
      group_by(Transect) %>%
      mutate(h2s_grt = ifelse(is.element("H2S", analyte) &
                                H2S > subset(rl_df, analyte == "H2S",
                                             select = rl), 1, 0)) %>%
      mutate(ch4_grt = ifelse(is.element("CH4", analyte) &
                                CH4 > subset(rl_df, analyte == "CH4",
                                             select = rl), 1, 0)) %>%
      mutate(ben_grt = ifelse(is.element("BEN", analyte) &
                                BEN > subset(rl_df, analyte == "BEN",
                                             select = rl), 1, 0)) %>%
      mutate(tol_grt = ifelse(is.element("TOL", analyte) &
                                TOL > subset(rl_df, analyte == "TOL",
                                             select = rl), 1, 0)) %>%
      mutate(xyp_grt = ifelse(is.element("XYP", analyte) &
                                XYP > subset(rl_df, analyte == "XYP",
                                             select = rl), 1, 0)) %>%
      summarise(h2s_grt = sum(h2s_grt), ch4_grt = sum(ch4_grt),
                ben_grt = sum(ben_grt), tol_grt = sum(tol_grt),
                xyp_grt = sum(xyp_grt))
    precent_rl_2 <- input %>%
      group_by(Transect) %>%
      reframe(cnt = n())
    grt_rl <- left_join(precent_rl, precent_rl_2, by = "Transect") %>%
      mutate(h2s_per = (h2s_grt / cnt) * 100) %>%
      mutate(ch4_per = (ch4_grt / cnt) * 100) %>%
      mutate(ben_per = (ben_grt / cnt) * 100) %>%
      mutate(tol_per = (tol_grt / cnt) * 100) %>%
      mutate(xyp_per = (xyp_grt / cnt) * 100) %>%
      select(h2s_per, ch4_per, ben_per, tol_per, xyp_per)
    print(grt_rl)
    list_[[3]] <- grt_rl
  }
  return(list_)
}
