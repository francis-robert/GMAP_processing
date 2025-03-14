MA_map <- function(x, rast_path, analyte, extent, transect=NULL, pt_size=10, campaign = NULL, multi_rast = NULL,
                   color_vec =c(" ") ){
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
    filter(grepl("MA",id))
if(!is.null(multi_rast)){
  raster_list <- list.files(path=rast_path, pattern = ".tif",full.names = T)
  raster_raw <-lapply(raster_list,rast)
  raster <- do.call(mosaic,raster_raw)
  writeRaster(raster,paste0(campaign,"_interim_raster_mosaic.tif"))
}else {
  raster <- rast(rast_path)
}

if(extent=="w"){
  if(is.null(transect)){
    data_sf_w <-data_sf %>%
      st_transform(.,crs=st_crs(crs(raster)))
  ext<- data_sf_w %>%
    st_bbox(.)
  ext[1] <- ext[1] - 50
  ext[2] <- ext[2] - 50
  ext[3] <- ext[3] + 50
  ext[4] <- ext[4] + 50

  raster_clip <- crop(raster,ext) %>%
    as.data.frame (raster,xy=T) %>%
    rename(Red = 3,
           Green = 4,
           Blue = 5)

  data_sf_w_sub <- data_sf_w %>%
    filter(header==analyte)

   ggplot(data = data_sf_w_sub)+
    geom_raster(data = raster_clip, aes(x = x, y = y),
                fill = rgb(r = raster_clip$Red,
                           g = raster_clip$Green,
                           b = raster_clip$Blue,
                           maxColorValue = 255)) +
     geom_sf(aes(color=value),size=pt_size)+
     labs(color=analyte)+
     scale_color_gradientn(colors = color_vec)+
     theme(axis.title = element_blank(), panel.background = element_blank(),
           axis.ticks = element_blank())
  }
}else{
    data_sf_s <-data_sf %>%
      st_transform(.,crs=st_crs(crs(raster))) %>%
      filter(id==transect)
    ext<- data_sf_s %>%
      st_bbox(.)
    ext[1] <- ext[1] - 50
    ext[2] <- ext[2] - 50
    ext[3] <- ext[3] + 50
    ext[4] <- ext[4] + 50

    raster_clip <- crop(raster,ext) %>%
      as.data.frame (raster,xy=T) %>%
      rename(Red = 3,
             Green = 4,
             Blue = 5)

    data_sf_s_sub <- data_sf_s %>%
      filter(header==analyte)

    ggplot(data = data_sf_s_sub)+
      geom_raster(data = raster_clip, aes(x = x, y = y),
                  fill = rgb(r = raster_clip$Red,
                             g = raster_clip$Green,
                             b = raster_clip$Blue,
                             maxColorValue = 255)) +
      geom_sf(aes(color=value),size=pt_size)+
      labs(color=analyte)+
      scale_color_gradientn(colors = color_vec)+
      theme(axis.title = element_blank(), panel.background = element_blank(),
            axis.ticks = element_blank())

}
}
