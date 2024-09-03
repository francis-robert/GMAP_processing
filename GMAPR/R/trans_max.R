#' Transect Maximum
#'
#' @param path path to the raw MEM output from GMAP
#' @param onoff Must be "on" or "off". Whether the recording were done onsite "on" i.e. on facility property - within fenceline, or offsite "off" i.e. off facility property - outside of facility fenceline
#' @param trantype Must be "MA" or "ST". Whether the recorded transect was a "MA", mapping i.e GMAP vehicle moving or "ST", stationary i.e. GMAP vehicle stationary
#' @param h2smdl The Hydrogen sulfide Minimum Detection Limit in ppb
#' @param h2shs The Hydrogen sulfide high span in ppb
#' @param ch4mdl The Methane Minimum Detection Limit in ppm
#' @param ch4hs The Methane high span in ppm
#' @param benmdl The Benzene Minimum Detection Limit in ppb
#' @param benhs The Benzene high span in ppb
#' @param tolmdl The Toluene Minimum Detection Limit in ppb
#' @param tolhs The Toluene high span in ppb
#' @param xypmdl The p-Xylene Minimum Detection Limit in ppb
#' @param xyphs The p-Xylene high span in ppb
#'
#' @return Returns the maximum value per transect from GMAP MEM output
#' @export
#'
#' @examples
trans_max <- function(path, onoff = " ", trantype = " ", h2smdl = NULL,
                      h2shs = NULL, ch4mdl = NULL, ch4hs = NULL, benmdl = NULL,
                      benhs = NULL, tolmdl = NULL, tolhs = NULL, xypmdl = NULL,
                      xyphs = NULL) {


  if (onoff == "off") {
    if (trantype == "MA") {
      files <- list.files(path = path, pattern = "MA", full.names =T)
      print(files)
      input <- lapply(files, FUN = read.table,
                      skip = 7, header = TRUE, sep = "\t", fill = TRUE,
                      stringsAsFactors = FALSE, na.strings = "NaN",
                      col.names = c("Time", "CO2", "CH4", "H2S", "C2H2",
                                    "M3DSU", "M3DSV", "M3DSW",
                                    "M3DS2DSpeed",
                                    "M3DS3DSpeed", "M3DSAzimuth",
                                    "M3DSElevation", "M3DSSoS",
                                    "M3DSSonicTemp", "M3DSErrorCode",
                                    "GPSTrack", "GPSGroundSpeed", "GPSTime",
                                    "GPSLatitude", "GPSLongitude",
                                    "GPSDoP", "BEN", "TOL", "ETB", "XYO",
                                    "XYM", "XYP", "SO2",
                                    "STY", "FOR", "NH3", "NO", "NO2", "O3",
                                    "CellPressure", "CellTemperature",
                                    "SUMMAPressure", "VOC",
                                    "AirMar.Wind.Speed..m.s.",
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
        mutate(Transect = files[Transect])%>%
        mutate(Transect = gsub(".*/", "", Transect)) %>%
        mutate(Transect = gsub("\\..*", "", Transect)) %>%
        dplyr::select("Time", "Transect", "CH4", "H2S", "BEN", "TOL", "XYP",
               "GPSLatitude",
               "GPSLongitude", "AirMar.Wind.Speed..m.s.",
               "AirMar.Wind.Direction..TRUE.") %>%
        mutate(Time = as.POSIXlt(Time, format = "%m/%d/%y %H:%M:%S")) %>%
        mutate(CH4 = round(CH4, digits = 2)) %>%
        mutate(H2S = round(H2S, digits = 2)) %>%
        mutate(BEN = round(BEN, digits = 2)) %>%
        mutate(TOL = round(TOL, digits = 2)) %>%
        mutate(XYP = round(XYP, digits = 2)) %>%
        mutate(H2Sflag = case_when(H2S < (-abs(h2smdl)) ~ "MD",
                                   H2S >= (-abs(h2smdl)) &
                                     H2S <= abs(h2smdl) ~ "ND",
                                   H2S > abs(h2smdl) &
                                     H2S <= (3 * abs(h2smdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(CH4flag = case_when(CH4 < (-abs(ch4mdl)) ~ "MD",
                                   CH4 >= (-abs(ch4mdl)) &
                                     CH4 <= abs(ch4mdl) ~ "ND",
                                   CH4 > abs(ch4mdl) &
                                     CH4 <= (3 * abs(ch4mdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(BENflag = case_when(BEN < (-abs(benmdl)) ~ "MD",
                                   BEN >= (-abs(benmdl)) &
                                     BEN <= abs(benmdl) ~ "ND",
                                   BEN > abs(benmdl) &
                                     BEN <= (5 * abs(benmdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(TOLflag = case_when(TOL < (-abs(tolmdl)) ~ "MD",
                                   TOL >= (-abs(tolmdl)) &
                                     TOL <= abs(tolmdl) ~ "ND",
                                   TOL > abs(tolmdl) &
                                     TOL <= (5 * abs(tolmdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(XYPflag = case_when(XYP < (-abs(xypmdl)) ~ "MD",
                                   XYP >= (-abs(xypmdl)) &
                                     XYP <= abs(xypmdl) ~ "ND",
                                   XYP > abs(xypmdl) &
                                     XYP <= (5 * abs(xypmdl)) ~ "PQ",
                                   .default = ""))


      if (!is.null(h2shs)) {
        input <- input %>%
          mutate(H2Sflag = ifelse(H2S >= h2shs, "EH", H2Sflag)) %>%
          mutate(H2Sflag = replace_na(H2Sflag, " "))
      }else {
        print("H2S High Span Absent")
      }
      if (!is.null(ch4hs)) {
        input <- input %>%
          mutate(CH4flag = ifelse(CH4 >= ch4hs, "EH", CH4flag)) %>%
          mutate(CH4flag = replace_na(CH4flag, " "))
      }else {
        print("CH4 High Span Absent")
      }
      if (!is.null(benhs)) {
        input <- input %>%
          mutate(BENflag = ifelse(BEN >= benhs, "EH", BENflag)) %>%
          mutate(BENflag = replace_na(BENflag, " "))
      }else {
        print("BEN High Span Absent")
      }
      if (!is.null(tolhs)) {
        input <- input %>%
          mutate(TOLflag = ifelse(TOL >= tolhs, "EH", TOLflag)) %>%
          mutate(TOLflag = replace_na(TOLflag, " "))
      }else {
        print("TOL High Span Absent")
      }
      if (!is.null(xyphs)) {
        input <- input %>%
          mutate(XYPflag = ifelse(XYP >= xyphs, "EH", XYPflag)) %>%
          mutate(XYPflag = replace_na(XYPflag, " "))
      }else {
        print("XYP High Span Absent")
      }

      input_maxlong <- input %>%
        dplyr::rename(., "H2S(ppb)" = "H2S", "CH4(ppm)" = "CH4",
                      "BEN(ppb)" = "BEN", "TOL(ppb)" = "TOL",
                      "XYP(ppb)" = "XYP",
                      "Latitude" = "GPSLatitude", "Longitude" = "GPSLongitude",
                      "ws" = "AirMar.Wind.Speed..m.s.",
                      "wd" = "AirMar.Wind.Direction..TRUE.") %>%
        relocate("Time", "Transect", "H2S(ppb)", "H2Sflag", "CH4(ppm)",
                 "CH4flag",
                 "BEN(ppb)", "BENflag",
                 "TOL(ppb)", "TOLflag", "XYP(ppb)", "XYPflag",  "Latitude",
                 "Longitude", "ws", "wd") %>%
        mutate(`H2S(ppb)` =
                 replace(`H2S(ppb)`,
                         str_detect(H2Sflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`CH4(ppm)` =
                 replace(`CH4(ppm)`,
                         str_detect(CH4flag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`BEN(ppb)` =
                 replace(`BEN(ppb)`,
                         str_detect(BENflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`TOL(ppb)` =
                 replace(`TOL(ppb)`,
                         str_detect(TOLflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`XYP(ppb)` =
                 replace(`XYP(ppb)`,
                         str_detect(XYPflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        dplyr::select("Time","Transect", "H2S(ppb)", "CH4(ppm)", "BEN(ppb)",
               "TOL(ppb)",
                "XYP(ppb)") %>%
        pivot_longer(., cols = c("H2S(ppb)", "CH4(ppm)", "BEN(ppb)", "TOL(ppb)",
                                  "XYP(ppb)")) %>%
        dplyr::rename("analyte" = "name") %>%
        group_by(Transect, analyte) %>%
        arrange(analyte, desc(value)) %>%
        slice_max(value, n = 1) %>%
        distinct(analyte = analyte, value = value) %>%
        pivot_wider(., id_cols = Transect, names_from = analyte) %>%
        relocate("Transect", "H2S(ppb)", "CH4(ppm)", "BEN(ppb)", "TOL(ppb)",
                 "XYP(ppb)")



      return(input_maxlong)


    }
    if (trantype == "ST") {
      files <- list.files(path = path, pattern = "ST", full.names =T)
      print(files)
      input <- lapply(files, FUN = read.table,
                      skip = 33, header = TRUE, sep = "\t", fill = TRUE,
                      stringsAsFactors = FALSE, na.strings = "NaN",
                      col.names = c("Time", "CO2", "CH4", "H2S", "C2H2",
                                    "M3DSU", "M3DSV", "M3DSW",
                                    "M3DS2DSpeed",
                                    "M3DS3DSpeed", "M3DSAzimuth",
                                    "M3DSElevation", "M3DSSoS",
                                    "M3DSSonicTemp", "M3DSErrorCode",
                                    "GPSTrack", "GPSGroundSpeed", "GPSTime",
                                    "GPSLatitude", "GPSLongitude",
                                    "GPSDoP", "BEN", "TOL", "ETB", "XYO",
                                    "XYM", "XYP", "SO2",
                                    "STY", "FOR", "NH3", "NO", "NO2", "O3",
                                    "CellPressure", "CellTemperature",
                                    "SUMMAPressure", "VOC",
                                    "AirMar.Wind.Speed..m.s.",
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
        mutate(Transect = files[Transect])%>%
        mutate(Transect = gsub(".*/", "", Transect)) %>%
        mutate(Transect = gsub("\\..*", "", Transect)) %>%
        dplyr::select("Time", "Transect", "CH4", "H2S", "BEN", "TOL", "XYP",
               "GPSLatitude",
               "GPSLongitude", "AirMar.Wind.Speed..m.s.",
               "AirMar.Wind.Direction..TRUE.") %>%
        mutate(Time = as.POSIXlt(Time, format = "%m/%d/%y %H:%M:%S")) %>%
        mutate(CH4 = round(CH4, digits = 2)) %>%
        mutate(H2S = round(H2S, digits = 2)) %>%
        mutate(BEN = round(BEN, digits = 2)) %>%
        mutate(TOL = round(TOL, digits = 2)) %>%
        mutate(XYP = round(XYP, digits = 2)) %>%
        mutate(H2Sflag = case_when(H2S < (-abs(h2smdl)) ~ "MD",
                                   H2S >= (-abs(h2smdl)) &
                                     H2S <= abs(h2smdl) ~ "ND",
                                   H2S > abs(h2smdl) &
                                     H2S <= (3 * abs(h2smdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(CH4flag = case_when(CH4 < (-abs(ch4mdl)) ~ "MD",
                                   CH4 >= (-abs(ch4mdl)) &
                                     CH4 <= abs(ch4mdl) ~ "ND",
                                   CH4 > abs(ch4mdl) &
                                     CH4 <= (3 * abs(ch4mdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(BENflag = case_when(BEN < (-abs(benmdl)) ~ "MD",
                                   BEN >= (-abs(benmdl)) &
                                     BEN <= abs(benmdl) ~ "ND",
                                   BEN > abs(benmdl) &
                                     BEN <= (5 * abs(benmdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(TOLflag = case_when(TOL < (-abs(tolmdl)) ~ "MD",
                                   TOL >= (-abs(tolmdl)) &
                                     TOL <= abs(tolmdl) ~ "ND",
                                   TOL > abs(tolmdl) &
                                     TOL <= (5 * abs(tolmdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(XYPflag = case_when(XYP < (-abs(xypmdl)) ~ "MD",
                                   XYP >= (-abs(xypmdl)) &
                                     XYP <= abs(xypmdl) ~ "ND",
                                   XYP > abs(xypmdl) &
                                     XYP <= (5 * abs(xypmdl)) ~ "PQ",
                                   .default = ""))


      if (!is.null(h2shs)) {
        input <- input %>%
          mutate(H2Sflag = ifelse(H2S >= h2shs, "EH", H2Sflag)) %>%
          mutate(H2Sflag = replace_na(H2Sflag, " "))
      }else {
        print("H2S High Span Absent")
      }
      if (!is.null(ch4hs)) {
        input <- input %>%
          mutate(CH4flag = ifelse(CH4 >= ch4hs, "EH", CH4flag)) %>%
          mutate(CH4flag = replace_na(CH4flag, " "))
      }else {
        print("CH4 High Span Absent")
      }
      if (!is.null(benhs)) {
        input <- input %>%
          mutate(BENflag = ifelse(BEN >= benhs, "EH", BENflag)) %>%
          mutate(BENflag = replace_na(BENflag, " "))
      }else {
        print("BEN High Span Absent")
      }
      if (!is.null(tolhs)) {
        input <- input %>%
          mutate(TOLflag = ifelse(TOL >= tolhs, "EH", TOLflag)) %>%
          mutate(TOLflag = replace_na(TOLflag, " "))
      }else {
        print("TOL High Span Absent")
      }
      if (!is.null(xyphs)) {
        input <- input %>%
          mutate(XYPflag = ifelse(XYP >= xyphs, "EH", XYPflag)) %>%
          mutate(XYPflag = replace_na(XYPflag, " "))
      }else {
        print("XYP High Span Absent")
      }

      input_maxlong <- input %>%
        dplyr::rename(., "H2S(ppb)" = "H2S", "CH4(ppm)" = "CH4",
                      "BEN(ppb)" = "BEN", "TOL(ppb)" = "TOL",
                      "XYP(ppb)" = "XYP",
                      "Latitude" = "GPSLatitude", "Longitude" = "GPSLongitude",
                      "ws" = "AirMar.Wind.Speed..m.s.",
                      "wd" = "AirMar.Wind.Direction..TRUE.") %>%
        relocate("Time", "Transect", "H2S(ppb)", "H2Sflag", "CH4(ppm)",
                 "CH4flag",
                 "BEN(ppb)", "BENflag",
                 "TOL(ppb)", "TOLflag", "XYP(ppb)", "XYPflag",  "Latitude",
                 "Longitude", "ws", "wd") %>%
        mutate(`H2S(ppb)` =
                 replace(`H2S(ppb)`,
                         str_detect(H2Sflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`CH4(ppm)` =
                 replace(`CH4(ppm)`,
                         str_detect(CH4flag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`BEN(ppb)` =
                 replace(`BEN(ppb)`,
                         str_detect(BENflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`TOL(ppb)` =
                 replace(`TOL(ppb)`,
                         str_detect(TOLflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`XYP(ppb)` =
                 replace(`XYP(ppb)`,
                         str_detect(XYPflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        dplyr::select("Time","Transect", "H2S(ppb)", "CH4(ppm)", "BEN(ppb)",
               "TOL(ppb)",
               "XYP(ppb)") %>%
        pivot_longer(., cols = c("H2S(ppb)", "CH4(ppm)", "BEN(ppb)", "TOL(ppb)",
                                 "XYP(ppb)")) %>%
        dplyr::rename("analyte" = "name") %>%
        group_by(Transect, analyte) %>%
        arrange(analyte, desc(value)) %>%
        slice_max(value, n = 1) %>%
        distinct(analyte = analyte, value = value) %>%
        pivot_wider(., id_cols = Transect, names_from = analyte) %>%
        relocate("Transect", "H2S(ppb)", "CH4(ppm)", "BEN(ppb)", "TOL(ppb)",
                 "XYP(ppb)")


      return(input_maxlong)


    }
  }
  if (onoff == "on") {
    if (trantype == "MA") {
      files <- list.files(path = path, pattern = "MA", full.names =T)
      print(files)
      input <- lapply(files, FUN = read.table,
                      skip = 7, header = TRUE, sep = "\t", fill = TRUE,
                      stringsAsFactors = FALSE, na.strings = "NaN",
                      col.names = c("Time", "CO2", "CH4", "H2S", "C2H2",
                                    "M3DSU", "M3DSV", "M3DSW",
                                    "M3DS2DSpeed",
                                    "M3DS3DSpeed", "M3DSAzimuth",
                                    "M3DSElevation", "M3DSSoS",
                                    "M3DSSonicTemp", "M3DSErrorCode",
                                    "GPSTrack", "GPSGroundSpeed", "GPSTime",
                                    "GPSLatitude", "GPSLongitude",
                                    "GPSDoP", "BEN", "TOL", "ETB", "XYO",
                                    "XYM", "XYP", "SO2",
                                    "STY", "FOR", "NH3", "NO", "NO2", "O3",
                                    "CellPressure", "CellTemperature",
                                    "SUMMAPressure", "VOC",
                                    "AirMar.Wind.Speed..m.s.",
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
        mutate(Transect = files[Transect])%>%
        mutate(Transect = gsub(".*/", "", Transect)) %>%
        mutate(Transect = gsub("\\..*", "", Transect)) %>%
        dplyr::select("Time", "Transect", "CH4", "H2S", "BEN", "TOL", "XYP",
               "GPSLatitude",
               "GPSLongitude", "AirMar.Wind.Speed..m.s.",
               "AirMar.Wind.Direction..TRUE.") %>%
        mutate(Time = as.POSIXlt(Time, format = "%m/%d/%y %H:%M:%S")) %>%
        mutate(CH4 = round(CH4, digits = 2)) %>%
        mutate(H2S = round(H2S, digits = 2)) %>%
        mutate(BEN = round(BEN, digits = 2)) %>%
        mutate(TOL = round(TOL, digits = 2)) %>%
        mutate(XYP = round(XYP, digits = 2)) %>%
        mutate(H2Sflag = case_when(H2S < (-abs(h2smdl)) ~ "MD",
                                   H2S >= (-abs(h2smdl)) &
                                     H2S <= abs(h2smdl) ~ "ND",
                                   H2S > abs(h2smdl) &
                                     H2S <= (3 * abs(h2smdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(CH4flag = case_when(CH4 < (-abs(ch4mdl)) ~ "MD",
                                   CH4 >= (-abs(ch4mdl)) &
                                     CH4 <= abs(ch4mdl) ~ "ND",
                                   CH4 > abs(ch4mdl) &
                                     CH4 <= (3 * abs(ch4mdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(BENflag = case_when(BEN < (-abs(benmdl)) ~ "MD",
                                   BEN >= (-abs(benmdl)) &
                                     BEN <= abs(benmdl) ~ "ND",
                                   BEN > abs(benmdl) &
                                     BEN <= (5 * abs(benmdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(TOLflag = case_when(TOL < (-abs(tolmdl)) ~ "MD",
                                   TOL >= (-abs(tolmdl)) &
                                     TOL <= abs(tolmdl) ~ "ND",
                                   TOL > abs(tolmdl) &
                                     TOL <= (5 * abs(tolmdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(XYPflag = case_when(XYP < (-abs(xypmdl)) ~ "MD",
                                   XYP >= (-abs(xypmdl)) &
                                     XYP <= abs(xypmdl) ~ "ND",
                                   XYP > abs(xypmdl) &
                                     XYP <= (5 * abs(xypmdl)) ~ "PQ",
                                   .default = ""))


      if (!is.null(h2shs)) {
        input <- input %>%
          mutate(H2Sflag = ifelse(H2S >= h2shs, "EH", H2Sflag)) %>%
          mutate(H2Sflag = replace_na(H2Sflag, " "))
      }else {
        print("H2S High Span Absent")
      }
      if (!is.null(ch4hs)) {
        input <- input %>%
          mutate(CH4flag = ifelse(CH4 >= ch4hs, "EH", CH4flag)) %>%
          mutate(CH4flag = replace_na(CH4flag, " "))
      }else {
        print("CH4 High Span Absent")
      }
      if (!is.null(benhs)) {
        input <- input %>%
          mutate(BENflag = ifelse(BEN >= benhs, "EH", BENflag)) %>%
          mutate(BENflag = replace_na(BENflag, " "))
      }else {
        print("BEN High Span Absent")
      }
      if (!is.null(tolhs)) {
        input <- input %>%
          mutate(TOLflag = ifelse(TOL >= tolhs, "EH", TOLflag)) %>%
          mutate(TOLflag = replace_na(TOLflag, " "))
      }else {
        print("TOL High Span Absent")
      }
      if (!is.null(xyphs)) {
        input <- input %>%
          mutate(XYPflag = ifelse(XYP >= xyphs, "EH", XYPflag)) %>%
          mutate(XYPflag = replace_na(XYPflag, " "))
      }else {
        print("XYP High Span Absent")
      }

      input_maxlong <- input %>%
        dplyr::rename(., "H2S(ppb)" = "H2S", "CH4(ppm)" = "CH4",
                      "BEN(ppb)" = "BEN", "TOL(ppb)" = "TOL",
                      "XYP(ppb)" = "XYP",
                      "Latitude" = "GPSLatitude", "Longitude" = "GPSLongitude",
                      "ws" = "AirMar.Wind.Speed..m.s.",
                      "wd" = "AirMar.Wind.Direction..TRUE.") %>%
        relocate("Time", "Transect", "H2S(ppb)", "H2Sflag", "CH4(ppm)",
                 "CH4flag",
                 "BEN(ppb)", "BENflag",
                 "TOL(ppb)", "TOLflag", "XYP(ppb)", "XYPflag",  "Latitude",
                 "Longitude", "ws", "wd") %>%
        mutate(`H2S(ppb)` =
                 replace(`H2S(ppb)`,
                         str_detect(H2Sflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`CH4(ppm)` =
                 replace(`CH4(ppm)`,
                         str_detect(CH4flag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`BEN(ppb)` =
                 replace(`BEN(ppb)`,
                         str_detect(BENflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`TOL(ppb)` =
                 replace(`TOL(ppb)`,
                         str_detect(TOLflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`XYP(ppb)` =
                 replace(`XYP(ppb)`,
                         str_detect(XYPflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        dplyr::select("Time","Transect", "H2S(ppb)", "CH4(ppm)", "BEN(ppb)",
               "TOL(ppb)",
               "XYP(ppb)") %>%
        pivot_longer(., cols = c("H2S(ppb)", "CH4(ppm)", "BEN(ppb)", "TOL(ppb)",
                                 "XYP(ppb)")) %>%
        dplyr::rename("analyte" = "name") %>%
        group_by(Transect, analyte) %>%
        arrange(analyte, desc(value)) %>%
        slice_max(value, n = 1) %>%
        distinct(analyte = analyte, value = value) %>%
        pivot_wider(., id_cols = Transect, names_from = analyte) %>%
        relocate("Transect", "H2S(ppb)", "CH4(ppm)", "BEN(ppb)", "TOL(ppb)",
                 "XYP(ppb)")


      return(input_maxlong)


    }
    if (trantype == "ST") {
      files <- list.files(path = path, pattern = "ST", full.names =T)
      print(files)
      input <- lapply(files, FUN = read.table,
                      skip = 33, header = TRUE, sep = "\t", fill = TRUE,
                      stringsAsFactors = FALSE, na.strings = "NaN",
                      col.names = c("Time", "CO2", "CH4", "H2S", "C2H2",
                                    "M3DSU", "M3DSV", "M3DSW",
                                    "M3DS2DSpeed",
                                    "M3DS3DSpeed", "M3DSAzimuth",
                                    "M3DSElevation", "M3DSSoS",
                                    "M3DSSonicTemp", "M3DSErrorCode",
                                    "GPSTrack", "GPSGroundSpeed", "GPSTime",
                                    "GPSLatitude", "GPSLongitude",
                                    "GPSDoP", "BEN", "TOL", "ETB", "XYO",
                                    "XYM", "XYP", "SO2",
                                    "STY", "FOR", "NH3", "NO", "NO2", "O3",
                                    "CellPressure", "CellTemperature",
                                    "SUMMAPressure", "VOC",
                                    "AirMar.Wind.Speed..m.s.",
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
        mutate(Transect = files[Transect])%>%
        mutate(Transect = gsub(".*/", "", Transect)) %>%
        mutate(Transect = gsub("\\..*", "", Transect)) %>%
        dplyr::select("Time", "Transect", "CH4", "H2S", "BEN", "TOL", "XYP",
               "GPSLatitude",
               "GPSLongitude", "AirMar.Wind.Speed..m.s.",
               "AirMar.Wind.Direction..TRUE.") %>%
        mutate(Time = as.POSIXlt(Time, format = "%m/%d/%y %H:%M:%S")) %>%
        mutate(CH4 = round(CH4, digits = 2)) %>%
        mutate(H2S = round(H2S, digits = 2)) %>%
        mutate(BEN = round(BEN, digits = 2)) %>%
        mutate(TOL = round(TOL, digits = 2)) %>%
        mutate(XYP = round(XYP, digits = 2)) %>%
        mutate(H2Sflag = case_when(H2S < (-abs(h2smdl)) ~ "MD",
                                   H2S >= (-abs(h2smdl)) &
                                     H2S <= abs(h2smdl) ~ "ND",
                                   H2S > abs(h2smdl) &
                                     H2S <= (3 * abs(h2smdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(CH4flag = case_when(CH4 < (-abs(ch4mdl)) ~ "MD",
                                   CH4 >= (-abs(ch4mdl)) &
                                     CH4 <= abs(ch4mdl) ~ "ND",
                                   CH4 > abs(ch4mdl) &
                                     CH4 <= (3 * abs(ch4mdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(BENflag = case_when(BEN < (-abs(benmdl)) ~ "MD",
                                   BEN >= (-abs(benmdl)) &
                                     BEN <= abs(benmdl) ~ "ND",
                                   BEN > abs(benmdl) &
                                     BEN <= (5 * abs(benmdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(TOLflag = case_when(TOL < (-abs(tolmdl)) ~ "MD",
                                   TOL >= (-abs(tolmdl)) &
                                     TOL <= abs(tolmdl) ~ "ND",
                                   TOL > abs(tolmdl) &
                                     TOL <= (5 * abs(tolmdl)) ~ "PQ",
                                   .default = "")) %>%
        mutate(XYPflag = case_when(XYP < (-abs(xypmdl)) ~ "MD",
                                   XYP >= (-abs(xypmdl)) &
                                     XYP <= abs(xypmdl) ~ "ND",
                                   XYP > abs(xypmdl) &
                                     XYP <= (5 * abs(xypmdl)) ~ "PQ",
                                   .default = ""))


      if (!is.null(h2shs)) {
        input <- input %>%
          mutate(H2Sflag = ifelse(H2S >= h2shs, "EH", H2Sflag)) %>%
          mutate(H2Sflag = replace_na(H2Sflag, " "))
      }else {
        print("H2S High Span Absent")
      }
      if (!is.null(ch4hs)) {
        input <- input %>%
          mutate(CH4flag = ifelse(CH4 >= ch4hs, "EH", CH4flag)) %>%
          mutate(CH4flag = replace_na(CH4flag, " "))
      }else {
        print("CH4 High Span Absent")
      }
      if (!is.null(benhs)) {
        input <- input %>%
          mutate(BENflag = ifelse(BEN >= benhs, "EH", BENflag)) %>%
          mutate(BENflag = replace_na(BENflag, " "))
      }else {
        print("BEN High Span Absent")
      }
      if (!is.null(tolhs)) {
        input <- input %>%
          mutate(TOLflag = ifelse(TOL >= tolhs, "EH", TOLflag)) %>%
          mutate(TOLflag = replace_na(TOLflag, " "))
      }else {
        print("TOL High Span Absent")
      }
      if (!is.null(xyphs)) {
        input <- input %>%
          mutate(XYPflag = ifelse(XYP >= xyphs, "EH", XYPflag)) %>%
          mutate(XYPflag = replace_na(XYPflag, " "))
      }else {
        print("XYP High Span Absent")
      }

      input_maxlong <- input %>%
        dplyr::rename(., "H2S(ppb)" = "H2S", "CH4(ppm)" = "CH4",
                      "BEN(ppb)" = "BEN", "TOL(ppb)" = "TOL",
                      "XYP(ppb)" = "XYP",
                      "Latitude" = "GPSLatitude", "Longitude" = "GPSLongitude",
                      "ws" = "AirMar.Wind.Speed..m.s.",
                      "wd" = "AirMar.Wind.Direction..TRUE.") %>%
        relocate("Time", "Transect", "H2S(ppb)", "H2Sflag", "CH4(ppm)",
                 "CH4flag",
                 "BEN(ppb)", "BENflag",
                 "TOL(ppb)", "TOLflag", "XYP(ppb)", "XYPflag",  "Latitude",
                 "Longitude", "ws", "wd") %>%
        mutate(`H2S(ppb)` =
                 replace(`H2S(ppb)`,
                         str_detect(H2Sflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`CH4(ppm)` =
                 replace(`CH4(ppm)`,
                         str_detect(CH4flag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`BEN(ppb)` =
                 replace(`BEN(ppb)`,
                         str_detect(BENflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`TOL(ppb)` =
                 replace(`TOL(ppb)`,
                         str_detect(TOLflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        mutate(`XYP(ppb)` =
                 replace(`XYP(ppb)`,
                         str_detect(XYPflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                         NA)) %>%
        dplyr::select("Time","Transect", "H2S(ppb)", "CH4(ppm)", "BEN(ppb)",
               "TOL(ppb)",
               "XYP(ppb)") %>%
        pivot_longer(., cols = c("H2S(ppb)", "CH4(ppm)", "BEN(ppb)", "TOL(ppb)",
                                 "XYP(ppb)")) %>%
        dplyr::rename("analyte" = "name") %>%
        group_by(Transect, analyte) %>%
        arrange(analyte, desc(value)) %>%
        slice_max(value, n = 1) %>%
        distinct(analyte = analyte, value = value) %>%
        pivot_wider(., id_cols = Transect, names_from = analyte) %>%
        relocate("Transect", "H2S(ppb)", "CH4(ppm)", "BEN(ppb)", "TOL(ppb)",
                 "XYP(ppb)")


      return(input_maxlong)


    }
  }
}
