MA_map <- function(x, rast_path, analyte, extent, transect=NULL, pt_size=10,
                   campaign = NULL, multi_rast = NULL,color_vec =c(" "),
                   rast_red=3, rast_green=4, rast_blue=5,
                   rast_type = c("naip","landsat")){
#once I get a better data set the below commented out code will be used to process
#the spatial data to get it ready for plotting
  data_nolocs <- x %>%
    filter(str_detect(header,"ANALYTE_")) %>%
    mutate(header = gsub("ANALYTE_","",header)) %>%
    filter(!header == "GPS-Latitude" | !header == "GPS-Longitude")

  data_locs <- x %>%
    filter(str_detect(header,"ANALYTE_")) %>%
    mutate(header = gsub("ANALYTE_","",header)) %>%
    filter(header == "GPS-Latitude" | header == "GPS-Longitude") %>%
    pivot_wider(.,id_cols = c(TimeStamp,id),names_from = header)

  data_comb <- data_nolocs %>%
    left_join(., data_locs, by=c("TimeStamp","id"))

  data_sf <-data_comb %>%
    st_as_sf(.,coords = c("GPS-Longitude","GPS-Latitude"),crs=4326) %>%
    filter(grepl("MA",id))

    raster_list <- list.files(path=rast_path, pattern = c(".tif|.TIF"),full.names = T)
  if(!is.null(multi_rast)){
    raster_raw <-lapply(raster_list,rast)
    raster <- do.call(mosaic,raster_raw)
    writeRaster(raster,paste0(campaign,"_interim_raster_mosaic.tif"))
    print("Mosaiced Raster has been generated! Change rast_path
          to utilize the novel generated raster.")
  }else {
    raster <- rast(raster_list)
  }
  raster_crs <- crs(raster)
  if(extent=="w"){
      data_sf_clip <-data_sf %>%
        st_transform(.,crs=st_crs(crs(raster_crs)))
      ext<- data_sf_clip %>%
        st_bbox(.)
        ext[1] <- ext[1] - 50
        ext[2] <- ext[2] - 50
        ext[3] <- ext[3] + 50
        ext[4] <- ext[4] + 50
    }else{
      data_sf_clip<-data_sf %>%
        st_transform(.,crs=st_crs(crs(raster_crs))) %>%
        filter(id==transect)
      ext<- data_sf_clip %>%
        st_bbox(.)

        ext[1] <- ext[1] - 50
        ext[2] <- ext[2] - 50
        ext[3] <- ext[3] + 50
        ext[4] <- ext[4] + 50
      }
  if(rast_type=="landsat"){
    # raster_landsat <-rast(red=raster[[grepl("B4",raster)]],# this is where things are getting screwy I'm gonnna have to tease it apart more(noted 3/25/25)
    #                                           green=raster[[grepl("B3",raster)]],
    #                                           blue=raster[[grepl("B2",raster)]])
   raster_clip <- terra::crop(raster,ext) %>%
    as.data.frame (raster,xy=T) %>%
    rename(x=1,
           y=2,
           b1=3,
           b2=4,
           b3=5,
           b4=6,
           b5=7,
           b6=8,
           b7=9)
    }else{
  raster_clip <- crop(raster,ext) %>%
    as.data.frame (raster,xy=T) %>%
    rename(red = rast_red,
           green = rast_green,
           blue = rast_blue)
}

data_sf_sub <- data_sf_clip %>%
  filter(header==analyte)
if(rast_type=="landsat"){
  output<-ggplot(data = data_sf_sub)+
    geom_spatial_rgb(,data=raster_clip,
                     mapping = aes(
                       x = x,
                       y = y,
                       r = b4,
                       g = b3,
                       b = b2))+
    geom_sf(aes(color=value),size=pt_size)+
    labs(color=analyte)+
    scale_color_gradientn(colors = color_vec)+
    theme(axis.title = element_blank(), panel.background = element_blank(),
          axis.ticks = element_blank())

}else{
output<-ggplot(data = data_sf_sub)+
    geom_raster(data = raster_clip, aes(x = x, y = y),
                fill = rgb(r = raster_clip$red,
                           g = raster_clip$green,
                           b = raster_clip$blue,
                           maxColorValue = 255)) +
     geom_sf(aes(color=value),size=pt_size)+
     labs(color=analyte)+
     scale_color_gradientn(colors = color_vec)+
     theme(axis.title = element_blank(), panel.background = element_blank(),
           axis.ticks = element_blank())
}
return(output)
}
