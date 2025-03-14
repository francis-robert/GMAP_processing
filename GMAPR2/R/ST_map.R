ST_map <- function(x, rast_path, transect=NULL, campaign = NULL, multi_rast = NULL){
  #once I get a better data set the below commented out code will be used to process
  #the spatial data to get it ready for plotting
  # data_nolocs <- x %>%
  #   filter(str_detect(header,"ANALYTE_")) %>%
  #   mutate(header = gsub("ANALYTE_","",header)) %>%
  #   filter(!header == "GPS-Latitude" | !header == "GPS-Longitude")
  #
  # data_locs <- x %>%
  #   filter(str_detect(header,"ANALYTE_")) %>%
  #   mutate(header = gsub("ANALYTE_","",header)) %>%
  #   filter(header == "GPS-Latitude" | header == "GPS-Longitude") %>%
  #   pivot_wider(.,id_cols = c(TimeStamp,id),names_from = header)
  #
  # data_comb <- data_nolocs %>%
  #   left_join(., data_locs, by=c("TimeStamp","id"))
  #
  data_sf <- x %>%
    # data_comb %>% #to be added back in
    st_as_sf(.,coords = c("GPS-Longitude","GPS-Latitude"),crs=4326) %>%
    filter(grepl("ST",id))
  if(!is.null(multi_rast)){
    raster_list <- list.files(path=rast_path, pattern = ".tif",full.names = T)
    raster_raw <-lapply(raster_list,rast)
    raster <- do.call(mosaic,raster_raw)
    writeRaster(raster,paste0(campaign,"_interim_raster_mosaic.tif"))
  }else {
    raster <- rast(rast_path)
  }
}
