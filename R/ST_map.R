ST_map <- function(x,y,z, rast_path, analyte, transect, campaign = NULL,
                   multi_rast = NULL,color_pal=o,zoom_scale=550){
  #once I get a better data set the below commented out code will be used to process
  #the spatial data to get it ready for plotting
  data_analyte <- x %>%
    filter(header==analyte)
  # filter(str_detect(header,"ANALYTE_")) %>%
  # mutate(header = gsub("ANALYTE_","",header)) %>%
  # filter(!header == "GPS-Latitude" | !header == "GPS-Longitude")

  locs <- y %>%
    # filter(str_detect(header,"ANALYTE_")) %>%
    # mutate(header = gsub("ANALYTE_","",header)) %>%
    filter(header=="wd"| header=="ws"|header == "GPS-Latitude" | header == "GPS-Longitude") %>%
    pivot_wider(.,id_cols = c(TimeStamp,id),names_from = header)

  data_comb <- data_analyte %>%
    left_join(., locs, by=c("TimeStamp","id"))
#
  data_sf <-
    # x %>%
    data_comb %>% #to be added back in
    st_as_sf(.,coords = c("GPS-Longitude","GPS-Latitude"),crs=4326) %>%
    filter(grepl("ST",id))
#
  color_breaks <- z %>%
    filter(analyte_name==analyte) %>%
    mutate(thresh_low=as.numeric(thresh_low)) %>%
    mutate(thresh_high=as.numeric(thresh_high)) %>%
    unite("thresh", c(thresh_low,thresh_high), sep = "-", remove = FALSE) %>%
    mutate(thresh=as.character(thresh))%>%
    mutate(thresh=str_replace(thresh, "-9999-","< ")) %>%
    mutate(thresh=str_replace(thresh, "-9998-","> "))
#
  o<-c("#FFF0E6","#FFD9B7","#FFC78F","#FFB560",
       "#FFA339","#FF8100","#E67000","#CC5700")
  b<-c("#f7fbff","#deebf7","#c6dbef","#9ecae1",
       "#6baed6","#4292c6","#2171b5","#084594")
  g<-c("#d6ff68","#bbf74d","#a0dc2f","#86c200",
       "#6ca800","#529000","#377700","#1d6000")
  y<-c("#ffff72","#f9e558","#decb3d","#c3b21e",
       "#a89900","#8e8100","#746a00","#5c5300")
  p<-c("#ffefff","#ffd5ff","#e5bbff","#caa2e7",
         "#b189cd","#9771b3","#7f5a9a","#674482")
  wed<-c("#ffc1ff","#ffa6ff","#ff8aff","#ff6fff",
         "#f652e6","#da32cb","#bf00b2","#a30098")

  if(!is.null(multi_rast)){
    raster_list <- list.files(path=rast_path, pattern = ".tif",full.names = T)
    raster_raw <-lapply(raster_list,rast)
    raster <- do.call(mosaic,raster_raw)
    writeRaster(raster,paste0(campaign,"_interim_raster_mosaic.tif"))
  }else {
    raster <- terra::aggregate(terra::rast(rast_path), fact=4)
  }

  data_sf_clip<-data_sf %>%
    st_transform(.,crs=st_crs(crs(raster))) %>%
    filter(id==transect)
  ext<- data_sf_clip %>%
    st_bbox(.)
  ext[1] <- ext[1] - zoom_scale
  ext[2] <- ext[2] - zoom_scale
  ext[3] <- ext[3] + zoom_scale
  ext[4] <- ext[4] + zoom_scale

raster_rename <-crop(raster,ext) %>%
  as.data.frame (raster,xy=T) %>%
  rename("Red" = 3,
         "Green" = 4,
         "Blue" = 5)
#
data_sf_sub <- data_sf_clip %>%
  filter(id==transect) %>%
#   filter(header==analyte) %>%
  ungroup() %>%
  mutate(value_bin=chop_equally(value,groups=5)) %>%
  rowwise() %>%
  # mutate(divs=chop_equally(wd,groups=8)) %>%
  mutate(bins=case_when(wd>=348.75 ~ "N",
                      wd<=11.25  ~ "N",
                      wd>11.25 & wd<=33.75 ~"NNE",
                      wd>33.75 & wd<=56.25 ~"NE",
                      wd>56.25 & wd<=78.75 ~"ENE",
                      wd>78.75 & wd<=101.25 ~"E",
                      wd>101.25 & wd<=123.75 ~"ESE",
                      wd>123.75 & wd<=146.25 ~"SE",
                      wd>146.25 & wd<=168.75 ~"SSE",
                      wd>168.75 & wd<=191.25 ~"S",
                      wd>191.25 & wd<=213.75 ~"SSW",
                      wd>213.75 & wd<=236.25 ~"SW",
                      wd>236.25 & wd<=258.75 ~"WSW",
                      wd>258.75 & wd<=281.25 ~"W",
                      wd>281.25 & wd<=303.75 ~"WNW",
                      wd>303.75 & wd<=326.25 ~"NW",
                      wd>326.25 & wd<=348.75 ~"NNW",)) %>%
# mutate(divs=as.factor(divs)) %>%
group_by(bins) %>%
mutate(freq=(n())) %>%
ungroup()%>%
mutate(freq_perc=freq/sum(freq))%>%
mutate(bins=factor(bins,levels=c("N","NNE","NE","ENE","E","ESE","SE","SSE",
                                 "S","SSW","SW","WSW","W","WNW","NW","NNW"))) %>%
st_as_sf(.,coords = c("GPS-Longitude","GPS-Latitude"),crs=st_crs(4326)) %>%
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

wr_plot<-ggplot(data=data_sf_sub, aes(x=bins,y=freq_perc,fill=thresh_color))+
  geom_bar(stat="identity",position = "stack",width = .5)+
  scale_fill_manual(name=analyte,
                     limits=c(color_breaks$thresh[1],
                              color_breaks$thresh[2],
                              color_breaks$thresh[3],
                              color_breaks$thresh[4],
                              color_breaks$thresh[5],
                              color_breaks$thresh[6],
                              color_breaks$thresh[7],
                              color_breaks$thresh[8]),
                    values = eval(parse(text=color_pal)))+
  coord_polar(start = 2 * pi - pi/8)+
    theme(panel.grid.major = element_line(colour = NA),
          axis.line = element_line(colour = NA),
          panel.background = element_rect(fill= "transparent", colour = NA),
          plot.background = element_rect(fill= "transparent", colour = NA),
          text = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",)+
    labs(fill=paste0(data_sf_sub$header))

ggsave(wr_plot, file=paste(campaign,"_wr_plot_temp.png",sep=""), bg= "transparent", device = NULL ,width = 10, height = 8, units = "in")

  data_sf_sub_tsfm <- data_sf_sub %>%
    st_transform(.,crs=st_crs(raster)) %>%
    # mutate(thresh=as.factor(thresh)) %>%
    mutate(long = unique(st_coordinates(.)[1,1])) %>%
    mutate(lat = unique(st_coordinates(.)[1,2])) %>%
    distinct(thresh,.keep_all = T)

wr_plot_img_df<-data.frame(x=unique(data_sf_sub_tsfm$long),y=unique(data_sf_sub_tsfm$lat),image=sample(paste(getwd(),"/",campaign,"_wr_plot_temp.png",sep=""),size = 1))

# north_arrow_df <-data.frame(narrow_x,narrow_y,image=sample(narrow_path,size = 1))

output <- ggplot(data=data_sf_sub_tsfm)+
  geom_sf(data = data_sf_sub_tsfm,aes(color=thresh),size=10,pch=15)+
  scale_color_manual(name=analyte,
                    limits=c(color_breaks$thresh[1],
                             color_breaks$thresh[2],
                             color_breaks$thresh[3],
                             color_breaks$thresh[4],
                             color_breaks$thresh[5],
                             color_breaks$thresh[6],
                             color_breaks$thresh[7],
                             color_breaks$thresh[8]),
                    values = eval(parse(text=color_pal)))+
  geom_raster(data = raster_rename, aes(x = x, y = y),
              fill = rgb(r = raster_rename$Red,
                         g = raster_rename$Green,
                         b = raster_rename$Blue,
                         maxColorValue = 255))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  geom_image(data=wr_plot_img_df,aes(x=x,y=y,image=image), size=.75)+
  labs(color=paste0(data_sf_sub_tsfm$header))+
  theme(legend.key = element_rect(fill = NA, color = NA),
        axis.title = element_blank())

return(output)
}
