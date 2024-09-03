field_tool_polar <-function(path,
                            analyte= c("H2S", "CH4", "BEN", "TOL", "XYP")){
  if (analyte == "H2S") {
    files <- list.files(path = path, pattern = "ST", full.names = TRUE)
    print(head(files))
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
      mutate(Transect = gsub("\\..*", "", Transect)) %>%
      select(Transect, H2S, GPSLatitude, GPSLongitude,
             `AirMar.Wind.Speed..m.s.`, `AirMar.Wind.Direction..TRUE.`) %>%
      rename("ws" = "AirMar.Wind.Speed..m.s.",
             "wd" = "AirMar.Wind.Direction..TRUE.",
             "latitude" = "GPSLatitude",
             "longitude" = "GPSLongitude")
    print(head(input))
    h2s_nozero <- input %>%
      filter(H2S > 0)
    print(h2s_nozero)
    map <- openairmaps::polarMap(h2s_nozero, pollutant = "H2S", ws = "ws",
                                 wd = "wd", latitude = "latitude",
                                 longitude = "longitude",
                                 provider = "Esri.WorldImagery")
    return(map)

  }
  if (analyte == "CH4") {
    files <- list.files(path = path, pattern = "ST", full.names = TRUE)
    print(head(files))
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
      mutate(Transect = gsub("\\..*", "", Transect)) %>%
      select(Transect, CH4, GPSLatitude, GPSLongitude,
             `AirMar.Wind.Speed..m.s.`, `AirMar.Wind.Direction..TRUE.`) %>%
      rename("ws" = "AirMar.Wind.Speed..m.s.",
             "wd" = "AirMar.Wind.Direction..TRUE.",
             "latitude" = "GPSLatitude",
             "longitude" = "GPSLongitude")
    ch4_nozero <- input %>%
      filter(CH4 > 0)
    map <- openairmaps::polarMap(ch4_nozero, pollutant = "CH4", ws = "ws",
                                 wd = "wd", latitude = "latitude",
                                 longitude = "longitude",
                                 provider = "Esri.WorldImagery")
    return(map)

  }
  if (analyte == "BEN") {
    files <- list.files(path = path, pattern = "ST", full.names = TRUE)
    print(head(files))
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
      mutate(Transect = gsub("\\..*", "", Transect)) %>%
      select(Transect, BEN, GPSLatitude, GPSLongitude,
             `AirMar.Wind.Speed..m.s.`, `AirMar.Wind.Direction..TRUE.`) %>%
      rename("ws" = "AirMar.Wind.Speed..m.s.",
             "wd" = "AirMar.Wind.Direction..TRUE.",
             "latitude" = "GPSLatitude",
             "longitude" = "GPSLongitude")
    ben_nozero <- input %>%
      filter(BEN > 0)
    map <- openairmaps::polarMap(ben_nozero, pollutant = "BEN", ws = "ws",
                                 wd = "wd", latitude = "latitude",
                                 longitude = "longitude",
                                 provider = "Esri.WorldImagery")
    return(map)

  }
  if (analyte == "TOL") {
    files <- list.files(path = path, pattern = "ST", full.names = TRUE)
    print(head(files))
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
      mutate(Transect = gsub("\\..*", "", Transect)) %>%
      select(Transect, TOL, GPSLatitude, GPSLongitude,
             `AirMar.Wind.Speed..m.s.`, `AirMar.Wind.Direction..TRUE.`) %>%
      rename("ws" = "AirMar.Wind.Speed..m.s.",
             "wd" = "AirMar.Wind.Direction..TRUE.",
             "latitude" = "GPSLatitude",
             "longitude" = "GPSLongitude")
    tol_nozero <- input %>%
      filter(TOL > 0)
    map <- openairmaps::polarMap(tol_nozero, pollutant = "TOL", ws = "ws",
                                 wd = "wd", latitude = "latitude",
                                 longitude = "longitude",
                                 provider = "Esri.WorldImagery")
    return(map)

  }
  if (analyte == "XYP") {
    files <- list.files(path = path, pattern = "ST", full.names = TRUE)
    print(head(files))
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
      mutate(Transect = gsub("\\..*", "", Transect)) %>%
      select(Transect, XYP, GPSLatitude, GPSLongitude,
             `AirMar.Wind.Speed..m.s.`, `AirMar.Wind.Direction..TRUE.`) %>%
      rename("ws" = "AirMar.Wind.Speed..m.s.",
             "wd" = "AirMar.Wind.Direction..TRUE.",
             "latitude" = "GPSLatitude",
             "longitude" = "GPSLongitude")
    xyp_nozero <- input %>%
      filter(XYP > 0)
    map <- openairmaps::polarMap(xyp_nozero, pollutant = "XYP", ws = "ws",
                                 wd = "wd", latitude = "latitude",
                                 longitude = "longitude",
                                 provider = "Esri.WorldImagery")
    return(map)

  }
}
