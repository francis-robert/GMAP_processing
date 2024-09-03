#' field tool mobile mapping
#'
#' @param path_data path to the data downloaded in the field
#' @param path_imagery path to the NAIP imagery downloaded prior to the field collection day
#' @param analyte vector of analytes to be visualized
#' @param pattern they type of the data file (either MA for mapping or ST for stationary )
#'
#' @return returns a plot of the data on a map of the area
#' @export
#'
#' @examples
field_tool_mapping <- function(path_data, path_imagery,
                               analyte = " ",
                               pattern = " ") {
  naip_imagery <- stack(path_imagery)
  pattern <- pattern
  files <-  list.files(path = path_data, pattern = pattern, full.names = TRUE)
  data <- lapply(files, FUN = read.table,
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

  data_sf <- sf::st_as_sf(data, coords = c("GPSLongitude", "GPSLatitude"),
                          crs = "+proj=longlat +ellps=GRS80 +no_defs +type=crs")
  data_sf_trsm <- sf::st_transform(data_sf, crs = crs(naip_imagery))

  ext <- round(st_bbox(data_sf_trsm))
  ext[1] <- ext[1] - 50
  ext[2] <- ext[2] - 50
  ext[3] <- ext[3] + 50
  ext[4] <- ext[4] + 50

  data_fix <- data_sf_trsm %>%
    mutate(x = sf::st_coordinates(.)[, 1],
           y = sf::st_coordinates(.)[, 2]) %>%
    st_drop_geometry() %>%
    dplyr::rename("ws" = "AirMar.Wind.Speed..m.s.",
                  "wd" = "AirMar.Wind.Direction..TRUE.") %>%
    mutate(ws = ws * 10)
  naip_crop <- raster::crop(naip_imagery, ext)
  naip_crop_3band <- stack(naip_crop[[1]], naip_crop[[2]],
                           naip_crop[[3]])
  naip_crop_df <- as.data.frame(naip_crop_3band, xy = TRUE)
  naip_crop_df_rename <- naip_crop_df %>%
    rename(Red = 3,
           Green = 4,
           Blue = 5)
  data_map <- data_fix %>%
    unite("geom", c("x", "y"), sep = ",", remove = FALSE) %>%
    distinct(geom, .keep_all = TRUE)
  print(head(data_map))
  list_ <- c()
  list_[[1]] <- data_map


  if (analyte == "ALL") {
    all_plot <- ggplot(data = data_map, aes(x = x, y = y)) +
      geom_raster(data = naip_crop_df_rename, aes(x = x, y = y),
                  fill = rgb(r = naip_crop_df_rename$Red,
                             g = naip_crop_df_rename$Green,
                             b = naip_crop_df_rename$Blue,
                             maxColorValue = 255)) +
      geom_point(aes(x = x, y = y), pch = 21) +
      theme(panel.background = element_blank(),
            axis.title = element_text(color = "white"),
            axis.text = element_text(color = "white")) +
      ggtitle("All Transects")
    list_[[2]] <- all_plot
  }
  if (analyte == "H2S") {
    h2s_only <- data.frame(data_map) %>%
      dplyr::select(Transect, H2S, ws, wd, x, y)
    for (i in unique(h2s_only$Transect)) {
      op <- par(ask = TRUE)
      h2s_only_tran <- h2s_only %>%
        filter(Transect == i)
      plot <- ggplot(data = h2s_only_tran, aes(x = x, y = y)) +
        geom_raster(data = naip_crop_df_rename, aes(x = x, y = y),
                    fill = rgb(r = naip_crop_df_rename$Red,
                               g = naip_crop_df_rename$Green,
                               b = naip_crop_df_rename$Blue,
                               maxColorValue = 255)) +
        scale_fill_identity() +
        geom_spoke(aes(x = x, y = y, angle = wd, radius = ws),
                   arrow = arrow(length = unit(0.15, "cm"))) +
        geom_point(aes(x = x, y = y, color = H2S)) +
        scale_color_gradient(low = "yellow", high = "red") +
        theme(panel.background = element_blank(),
              axis.title = element_text(color = "white"),
              axis.text = element_text(color = "white")) +
        ggtitle(paste(i, analyte, sep = "_"))
      list_[[i]]<-(plot)
      par(op)
    }
  }
  if (analyte == "CH4") {
    ch4_only <- data.frame(data_map) %>%
      dplyr::select(Transect, CH4, ws, wd, x, y)
    for (i in unique(ch4_only$Transect)) {
      op <- par(ask = TRUE)
      ch4_only_tran <- ch4_only %>%
        filter(Transect == i)
      plot <- ggplot(data = ch4_only_tran, aes(x = x, y = y)) +
        geom_raster(data = naip_crop_df_rename, aes(x = x, y = y),
                    fill = rgb(r = naip_crop_df_rename$Red,
                               g = naip_crop_df_rename$Green,
                               b = naip_crop_df_rename$Blue,
                               maxColorValue = 255)) +
        scale_fill_identity() +
        geom_spoke(aes(x = x, y = y, angle = wd, radius = ws),
                   arrow = arrow(length = unit(0.15, "cm"))) +
        geom_point(aes(x = x, y = y, color = CH4)) +
        scale_color_gradient(low = "yellow", high = "red") +
        theme(panel.background = element_blank(),
              axis.title = element_text(color = "white"),
              axis.text = element_text(color = "white")) +
        ggtitle(paste(i, analyte, sep = "_"))
      list_[[i]]<-(plot)
      par(op)
    }
  }
  if (analyte == "BEN") {
    ben_only <- data.frame(data_map) %>%
      dplyr::select(Transect, BEN, ws, wd, x, y)
    for (i in unique(ben_only$Transect)) {
      op <- par(ask = TRUE)
      ben_only_tran <- ben_only %>%
        filter(Transect == i)
      plot <- ggplot(data = ben_only_tran, aes(x = x, y = y)) +
        geom_raster(data = naip_crop_df_rename, aes(x = x, y = y),
                    fill = rgb(r = naip_crop_df_rename$Red,
                               g = naip_crop_df_rename$Green,
                               b = naip_crop_df_rename$Blue,
                               maxColorValue = 255)) +
        scale_fill_identity() +
        geom_spoke(aes(x = x, y = y, angle = wd, radius = ws),
                   arrow = arrow(length = unit(0.15, "cm"))) +
        geom_point(aes(x = x, y = y, color = BEN)) +
        scale_color_gradient(low = "yellow", high = "red") +
        theme(panel.background = element_blank(),
              axis.title = element_text(color = "white"),
              axis.text = element_text(color = "white")) +
        ggtitle(paste(i, analyte, sep = "_"))
      list_[[i]]<-(plot)
      par(op)
    }
  }
  if (analyte == "TOL") {
    tol_only <- data.frame(data_map) %>%
      dplyr::select(Transect, TOL, ws, wd, x, y)
    for (i in unique(tol_only$Transect)) {
      op <- par(ask = TRUE)
      tol_only_tran <- tol_only %>%
        filter(Transect == i)
      plot <- ggplot(data = tol_only_tran, aes(x = x, y = y)) +
        geom_raster(data = naip_crop_df_rename, aes(x = x, y = y),
                    fill = rgb(r = naip_crop_df_rename$Red,
                               g = naip_crop_df_rename$Green,
                               b = naip_crop_df_rename$Blue,
                               maxColorValue = 255)) +
        scale_fill_identity() +
        geom_spoke(aes(x = x, y = y, angle = wd, radius = ws),
                   arrow = arrow(length = unit(0.15, "cm"))) +
        geom_point(aes(x = x, y = y, color = TOL)) +
        scale_color_gradient(low = "yellow", high = "red") +
        theme(panel.background = element_blank(),
              axis.title = element_text(color = "white"),
              axis.text = element_text(color = "white")) +
        ggtitle(paste(i, analyte, sep = "_"))
      list_[[i]]<-(plot)
      par(op)
    }
  }
  if (analyte == "XYP") {
    xyp_only <- data.frame(data_map) %>%
      dplyr::select(Transect, XYP, ws, wd, x, y)
    for (i in unique(xyp_only$Transect)) {
      op <- par(ask = TRUE)
      xyp_only_tran <- xyp_only %>%
        filter(Transect == i)
      plot <- ggplot(data = xyp_only_tran, aes(x = x, y = y)) +
        geom_raster(data = naip_crop_df_rename, aes(x = x, y = y),
                    fill = rgb(r = naip_crop_df_rename$Red,
                               g = naip_crop_df_rename$Green,
                               b = naip_crop_df_rename$Blue,
                               maxColorValue = 255)) +
        scale_fill_identity() +
        geom_spoke(aes(x = x, y = y, angle = wd, radius = ws),
                   arrow = arrow(length = unit(0.15, "cm"))) +
        geom_point(aes(x = x, y = y, color = XYP)) +
        scale_color_gradient(low = "yellow", high = "red") +
        theme(panel.background = element_blank(),
              axis.title = element_text(color = "white"),
              axis.text = element_text(color = "white")) +
        ggtitle(paste(i, analyte, sep = "_"))
      list_[[i]] <- (plot)
      par(op)
    }
  }
  return(list_)
}
