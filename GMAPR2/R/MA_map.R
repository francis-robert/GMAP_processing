MA_map <- function(x,y, rast_path, z, analyte, extent, transect=NULL, pt_size=10,
                   campaign = NULL, multi_rast = NULL,color_pal=switch(o),
                   rast_red=3, rast_green=4, rast_blue=5,zoom_scale=550,
                   rast_type = c("naip","landsat")){
#once I get a better data set the below commented out code will be used to process
#the spatial data to get it ready for plotting
  analyte_vec<-as.vector(analyte)
  data_analyte <- x
    # filter(str_detect(header,"ANALYTE_")) %>%
    # mutate(header = gsub("ANALYTE_","",header)) %>%
    # filter(!header == "GPS-Latitude" | !header == "GPS-Longitude")

  locs <- y %>%
    # filter(str_detect(header,"ANALYTE_")) %>%
    # mutate(header = gsub("ANALYTE_","",header)) %>%
    filter(header == "GPS-Latitude" | header == "GPS-Longitude") %>%
    pivot_wider(.,id_cols = c(TimeStamp,id),names_from = header)

  data_comb <- data_analyte %>%
    left_join(., locs, by=c("TimeStamp","id"))

  data_sf <-data_comb %>%
    st_as_sf(.,coords = c("GPS-Longitude","GPS-Latitude"),crs=4326) %>%
    filter(grepl("MA",id))

  color_breaks <- z %>%
    filter(analyte==analyte_vec) %>%
    unite("thresh", c(thresh_low,thresh_high), sep = "-", remove = FALSE) %>%
    mutate(thresh=str_replace(thresh, "-9999-","< ")) %>%
    mutate(thresh=str_replace(thresh, "-9998-","> "))

  o<-c("#FFF0E6","#FFD9B7","#FFC78F","#FFB560",
      "#FFA339","#FF8100","#E67000","#CC5700")
  b<-c("#f7fbff","#deebf7","#c6dbef","#9ecae1",
        "#6baed6","#4292c6","#2171b5","#084594")
  g<-c("#d6ff68","#bbf74d","#a0dc2f","#86c200",
        "#6ca800","#529000","#377700","#1d6000")
  y<-c("#ffff72","#f9e558","#decb3d","#c3b21e",
        "#a89900","#8e8100","#746a00","#5c5300")
  wed<-c("#ffc1ff","#ffa6ff","#ff8aff","#ff6fff",
          "#f652e6","#da32cb","#bf00b2","#a30098")
  p<-c("#ffefff","#ffd5ff","#e5bbff","#caa2e7",
          "#b189cd","#9771b3","#7f5a9a","#674482")

  raster_list <- list.files(path=rast_path, pattern = c(".tif|.TIF"),full.names = T)
  # if(!is.null(multi_rast)){
  #   raster_raw <-lapply(raster_list,rast)
  #   raster <- do.call(mosaic,raster_raw)
  #   print("Mosaiced Raster has been generated! Change rast_path
  #         to utilize the novel generated raster.")
  #   writeRaster(raster_raw,paste0(campaign,"_interim_raster_mosaic.tif"))
    # print("Mosaiced Raster has been generated! Change rast_path
    #       to utilize the novel generated raster.")
  # }else {
  raster <- rast(raster_list)
  raster_crs <- crs(raster)
  print(raster_crs)
  if(extent=="w"){
      data_sf_clip <-data_sf %>%
        st_transform(.,crs=st_crs(crs(raster_crs)))
      ext<- data_sf_clip %>%
        st_bbox(.)
        ext[1] <- ext[1] - zoom_scale
        ext[2] <- ext[2] - zoom_scale
        ext[3] <- ext[3] + zoom_scale
        ext[4] <- ext[4] + zoom_scale
    }else{
      data_sf_clip<-data_sf %>%
        st_transform(.,crs=st_crs(crs(raster_crs))) %>%
        filter(id==transect)
      ext<- data_sf_clip %>%
        st_bbox(.)

        ext[1] <- ext[1] - zoom_scale
        ext[2] <- ext[2] - zoom_scale
        ext[3] <- ext[3] + zoom_scale
        ext[4] <- ext[4] + zoom_scale
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
  filter(header==analyte_vec) %>%
  mutate(thresh_color = case_when(value <= color_breaks$thresh_high[1] ~ paste0(color_breaks$thresh[1]),
                            value > color_breaks$thresh_low[2] & value <= color_breaks$thresh_high[2] ~ paste0(color_breaks$thresh[2]),
                            value > color_breaks$thresh_low[2] & value <= color_breaks$thresh_high[3] ~ paste0(color_breaks$thresh[3]),
                            value > color_breaks$thresh_low[2] & value <= color_breaks$thresh_high[4] ~ paste0(color_breaks$thresh[4]),
                            value > color_breaks$thresh_low[2] & value <= color_breaks$thresh_high[5] ~ paste0(color_breaks$thresh[5]),
                            value > color_breaks$thresh_low[2] & value <= color_breaks$thresh_high[6] ~ paste0(color_breaks$thresh[6]),
                            value > color_breaks$thresh_low[2] & value <= color_breaks$thresh_high[7] ~ paste0(color_breaks$thresh[7]),
                            value > color_breaks$thresh_high[8] ~ paste0(color_breaks$thresh[8]),
                            .default = "NA")) %>%
  left_join(., color_breaks, by = c("header"="analyte_name"),relationship="many-to-many")

if(rast_type=="landsat"){
  output<-ggplot(data = data_sf_sub)+
    geom_spatial_rgb(,data=raster_clip,
                     mapping = aes(
                       x = x,
                       y = y,
                       r = b4,
                       g = b3,
                       b = b2))+
    geom_sf(aes(color=thresh),size=pt_size,show.legend = T)+
    geom_sf(aes(color=thresh_color),size=pt_size, show.legend = F)+
    scale_color_manual(name=analyte,
                       limits=c(color_breaks$thresh[1],
                                color_breaks$thresh[2],
                                color_breaks$thresh[3],
                                color_breaks$thresh[4],
                                color_breaks$thresh[5],
                                color_breaks$thresh[6],
                                color_breaks$thresh[7],
                                color_breaks$thresh[8]),
                       values = eval(parse(text=color_pal)),
                       drop=FALSE)+
    theme(axis.title = element_blank(), panel.background = element_blank(),
          axis.ticks = element_blank())

}else{
output<-ggplot(data = data_sf_sub)+
  geom_spatial_rgb(,data=raster_clip,
                   mapping = aes(
                     x = x,
                     y = y,
                     r = red,
                     g = green,
                     b = blue))+
  geom_sf(aes(color=thresh),size=pt_size,show.legend = T)+
  geom_sf(aes(color=thresh_color),size=pt_size, show.legend = F)+
  scale_color_manual(name=analyte,
                     limits=c(color_breaks$thresh[1],
                              color_breaks$thresh[2],
                              color_breaks$thresh[3],
                              color_breaks$thresh[4],
                              color_breaks$thresh[5],
                              color_breaks$thresh[6],
                              color_breaks$thresh[7],
                              color_breaks$thresh[8]),
                     values = eval(parse(text=color_pal)),
                     drop=FALSE)+
  theme(axis.title = element_blank(), panel.background = element_blank(),
        axis.ticks = element_blank())
}
 return(output)
}
