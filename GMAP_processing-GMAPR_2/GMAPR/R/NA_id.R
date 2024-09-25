

#' #identification of NA in the data (NAs indicate that the system was switched to dry air)
#'
#' @param path path to the raw output of the GMAP file
#' @param trantype The transect type either "MA" mapping, or "ST" stationary
#' @param analyte The analytes to check for NAs
#'
#' @return returns data frame with NAs removed
#' @export
#'
#' @examples
NA_id<-function(path = " ", trantype = "",
                analyte = c("BEN", "TOL", "XYP")) {
  analyte_vec <- unique(analyte)
  trantype <- as.character(trantype)
  if (trantype == "MA") {
    input <- list.files(path=path, pattern = "MA", full.names =T)
    print(input)
     input_2 <- lapply(input, FUN=read.table,
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
       bind_rows(., .id = "Transect")

      if("Time" %in% colnames(input_2)) {
          print("NA(s) found")
        }else {
          stop("No NA(s) found")
        }
     input_3 <- input_2 %>%
       dplyr::select("Time", "CH4", "H2S", "BEN", "TOL", "XYP", "GPSLatitude",
              "GPSLongitude", "AirMar.Wind.Speed..m.s.",
              "AirMar.Wind.Direction..TRUE.") %>%
       mutate(Time = as.POSIXlt(Time, format = "%m/%d/%y %H:%M:%S")) %>%
       mutate(BEN = round(BEN, digits = 2)) %>%
       mutate(TOL = round(TOL, digits = 2)) %>%
       mutate(XYP = round(XYP, digits = 2)) %>%
       filter(if_all(analyte_vec, all_vars(is.na(.))))
     return(input_3)
  }
  if (trantype == "ST") {
    input <- list.files(path=path, pattern = "ST", full.names =T)
    print(input)
    input_2 <- lapply(input, FUN=read.table,
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
      bind_rows(., .id = "Transect")

    if("Time" %in% colnames(input_2)) {
      print("NA(s) found")
    }else {
      stop("No NA(s) found")
    }
    input_3 <- input_2 %>%
      dplyr::select("Time", "CH4", "H2S", "BEN", "TOL", "XYP", "GPSLatitude",
             "GPSLongitude", "AirMar.Wind.Speed..m.s.",
             "AirMar.Wind.Direction..TRUE.") %>%
      mutate(Time = as.POSIXlt(Time, format = "%m/%d/%y %H:%M:%S")) %>%
      mutate(BEN = round(BEN, digits = 2)) %>%
      mutate(TOL = round(TOL, digits = 2)) %>%
      mutate(XYP = round(XYP, digits = 2)) %>%
      filter(if_all(analyte_vec, all_vars(is.na(.))))
    return(input_3)
  }
}

