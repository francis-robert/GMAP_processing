
mdl<-read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/2024_mdl_list.csv")
#rawdataprep_test#####
test<-rawdataprep("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/GMAP2 Data test")


MA_temp <- read.table("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/GMAP2 Data test/Mapping/250203/250203_MA01.txt",skip=20, sep = "\t",
                      fill = TRUE, na.strings = "NaN")
header_MA_temp <- read.table("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/GMAP2 Data test/Mapping/250203/250203_MA01.txt",sep = "\t", skip = 15, nrows = 5,fill=T,header = F) %>%
  replace(is.na(.),"NA") %>%
  mutate(across(everything(),~str_replace_all(.,"_","-"))) %>%
  mutate(across(everything(),~str_replace_all(.," ","-"))) %>%
  mutate(across(everything(),~sub("^$","BLANK",.))) %>%
  pivot_longer(.,cols = 1:ncol(.)) %>%
  mutate(name = as.numeric(gsub("V", "", name))) %>%
  group_by(name) %>%
  summarise(value = str_c(value, collapse="_")) %>%
  pivot_wider(.)

#rawlist_2_df test####
df_test <- rawlist_2_df(test,"MA",campaign = "GMAPTEST",loc="off")
df_test_st <-rawlist_2_df(test,"ST",campaign = "GMAPTEST",loc="off")

#MA_ST_bind test ####
comb<-MA_ST_bind(df_test,df_test_st)

##pic_flag test ####
test_pic_flag <- pic_flagging(comb, mdl, h2shs = 10110, ch4hs = 25)

# unique(test_pic_flag$mdl_flag)
# x <- test_pic_flag %>% filter(header == "ANALYTE_H2S")
# View(x)



#transect start and stop time (can use comb, time_pic_flag, or tf after run time flagging)

tran_time_minmax <- transect_time_minmax(comb)

#time flag test####
# test_pic_flag <- test_pic_flag %>%
#   mutate(time_flag="NA")


tf<-time_flagging(test_pic_flag, timestart = "02/03/2025 11:18:00", timestop = "02/03/2025 11:18:11", timeqt = "TESTTESTTEST",
              analyte = c("H2S","acrolein","ccl4"))

tf_2<-time_flagging(tf,timestart = "02/03/2025 11:21:30", timestop = "02/03/2025 11:24:00", timeqt = "WHOOPER",
                    analyte = c("H2S","CH4","acetone"))

#transect_max test, trans time min max test####

tran_max<- transect_max(tf_2)


#time series test #####
tst<- ts_table(tf_2,rm_flagged = "no")

#output data & max csv test ####
output <- output_csv_data(tf_2,loc="off")

out_distinct<-output %>%
  distinct(.)



# tf_2 |>
#   dplyr::summarise(n = dplyr::n(), .by = c(TimeStamp, header)) |>
#   dplyr::filter(n > 1L)
#time series plot test####

######
analyte = c("H2S,benzene,xyleth","acetone,thf")
grp = c(1,2)

input <- tf_2 %>%
  filter(str_detect(header,"ANALYTE_")) %>%
  mutate(header = gsub("ANALYTE_","",header)) %>%
  filter(!header == "ws" & !header == "wd") %>%
  left_join(.,groupings_1,by="header") %>%
  drop_na(grp)
input_ws_wd_lat_long <- tf_2 %>%
  select(TimeStamp,id,header,value) %>%
  filter(str_detect(header,"ANALYTE_")) %>%
  mutate(header = gsub("ANALYTE_","",header)) %>%
  filter(header=="ws"|header=="wd"|header=="GPS-Latitude"|header=="GPS-Longitude") %>%
  pivot_wider(.,id_cols = c(TimeStamp,id),names_from = header)
header <- analyte
groupings <- tibble(grp, header)
print(groupings)
groupings_1<-groupings %>%
  mutate(header=strsplit(header,","))%>%
  unnest(header)
# input_2<-input %>%
#   left_join(.,groupings_1,by="header") %>%
#   left_join(.,input_ws_wd_lat_long,by="TimeStamp")
# # filter(header==groupings_1$header)
# input_3 <-input_2 %>%
#   filter(!is.na(grp))

time_test<-input_ws_wd_lat_long %>%
  group_by(id) %>%
  mutate(time_interval=floor_date(TimeStamp,unit="hour")+minutes(floor(minute(TimeStamp)/1)*1))

mean_wd<- time_test %>%
  group_by(time_interval) %>%
  mutate(ones=1) %>%
  mutate(ns=(1/sum(ones)) * sum(sin(wd))) %>%
  mutate(ew=(1/sum(ones)) * sum(cos(wd))) %>%
  mutate(avg_wd=90-atan(ns/ew)) %>%
  distinct(id,.keep_all = T) %>%
  mutate(wd_rad=((avg_wd*-1)+90+180))

mean_wd_test<-mean_wd %>%
  filter(id=="MA01")

input_multi <- input %>%
  unite("id_grp",c(id,grp)) %>%
  left_join(.,mean_wd,by="TimeStamp") %>%
  group_by(id_grp) %>%
  drop_na(wd_rad)

input_multi_list <- input_multi %>%
  group_split(.,.keep = T)
wd_plot<-ggplot(input_multi_list[[1]],aes(x=Time,y=ws))+
  geom_text(aes(angle=wd_rad),label="→",size=7)
  # theme(axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),legend.position = "none")

analyte_plot<- input %>%
  left_join(.,groupings_1,by="header") %>%
  filter(id=="MA01", grp==1)

y<-ggplot(analyte_plot, aes(x=TimeStamp,y=value,color=header))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("blue3","darkorange","chartreuse3","firebrick2","blueviolet","orange4","violetred","honeydew4","gold2","turquoise2"),
                     drop=FALSE)+
  geom_hline(aes(yintercept=unique(mdl),linetype="MDL"))+
  geom_hline(aes(yintercept=unique(SQL),linetype="SQL"))+
  scale_linetype_manual("Critical Values",values=c("MDL"="dashed","SQL"="dotted"))

ggarrange(wd_plot,y, nrow = 2,ncol = 1, heights = c(1,3), common.legend = T,legend = "bottom",align = "hv")


ts_test<-ts_plot(tf_2, grp = c("A","B"), analyte = c("H2S,dbe,pce","benzene,thf"),multi_analyte = TRUE, time_labels="15 sec")

ts_test_2<-ts_plot(tf_2, grp = c("A"), analyte = c("H2S"),multi_analyte = FALSE, time_labels="15 sec")


#####dashboard prep (forcing it maybe not the final function )####

latlong_dash <-tf_2 %>%
  filter(header=="ANALYTE_GPS-Latitude" | header=="ANALYTE_GPS-Longitude") %>%
  mutate(header = gsub(".*ANALYTE_GPS-","",header)) %>%
  pivot_wider(.,id_cols=TimeStamp,names_from = header)

wdws_dash <-tf_2 %>%
  filter(header=="ANALYTE_ws" | header=="ANALYTE_wd") %>%
  mutate(header = gsub(".*ANALYTE_","",header)) %>%
  pivot_wider(.,id_cols=TimeStamp,names_from = header)


dash_ex<-tf_2 %>%
  unite("flag",c(mdl_flag,time_flag),sep = ",")%>%
  mutate(flag = gsub(".*NA,","",flag)) %>%
  mutate(flag = gsub("\\,NA","",flag)) %>%
  mutate(syft_method = case_when(type=="SyftGas"~"TO15",
                                 .default = "NA")) %>%
  mutate(samp_interval = NA) %>%
  mutate(particle_size = NA) %>%
  filter(!header=="ANALYTE_GPS-Latitude" | !header=="ANALYTE_GPS-Longitude") %>%
  left_join(.,latlong_dash,by="TimeStamp") %>%
  mutate(crs = "EPSG:4326") %>%
  mutate(epa_region = 5) %>%
  mutate(ma_st = substr(id,start=1,stop=2)) %>%
  mutate(met_bool= 1) %>%
  filter(!header=="ANALYTE_ws" | !header=="ANALYTE_wd") %>%
  left_join(.,wdws_dash,by="TimeStamp") %>%
  mutate(naics="NA") %>%
  mutate(naics_industry="NA") %>%
  mutate(tags="NA") %>%
  mutate(operators = "Haile.Kate") %>%
  mutate(validated_bool=0) %>%
  mutate(report_bool=0) %>%
  mutate(links="NA") %>%
  mutate(QAPP="NA") %>%
  mutate(requestor="NA") %>%
  mutate(Can_trigger_ppb = NA) %>%
  mutate(Can_trigger_wd = NA) %>%
  mutate(GMAP_bool=1) %>%
  mutate(header = gsub(".*ANALYTE_","",header)) %>%
  rename("transect" = "id","date"="Date","time_local"="Time",
         "analyte"="header","CasNo"="cas","value_units"="units",
         "lat"="Latitude","long"="Longitude","on_off"="loc_samp")
dash_ex_2 <- dash_ex%>%
select(TimeStamp,date,time_local,analyte,CasNo,value,value_units,flag,
       syft_method,samp_interval,particle_size,lat,long,crs,epa_region,
       ma_st,on_off,instrument, met_bool,wd,ws,campaign,naics,naics_industry,
       tags,operators,transect,validated_bool,report_bool,links,QAPP,
       requestor,Can_trigger_ppb,Can_trigger_wd,GMAP_bool)

write.csv(dash_ex_2,"GMAP_VAN_examp.csv")


#setting up the mapping functions####

#the calls for sf_data_noloc and sf_data_loc are required but for the current sample
#data (as of 3/13/2025) which is a combination of sample data and kluzcky brothers data
#(ananlyte info from specific samples but geospatial data from kluzcky brothers) the process can
#skip right to the sf_data call
sf_data_noloc <- tf_2 %>%
  filter(str_detect(header,"ANALYTE_")) %>%
  mutate(header = gsub("ANALYTE_","",header)) %>%
  filter(!header == "GPS-Latitude" | !header == "GPS-Longitude")

sf_data_loc <- tf_2 %>%
  filter(str_detect(header,"ANALYTE_")) %>%
  mutate(header = gsub("ANALYTE_","",header)) %>%
  filter(header == "GPS-Latitude" | header == "GPS-Longitude") %>%
  pivot_wider(.,id_cols = c(TimeStamp,id),names_from = header)

sf_data <- sf_data_noloc %>%
  left_join(.,sf_data_loc, by=c("TimeStamp","id")) %>%
  st_as_sf(.,coords = c("GPS-Longitude","GPS-Latitude"),crs=4326)%>%
  filter(header=="H2S" & id=="MA02")
  st_cast("LINESTRING")

  kluz_sf <- samp_kluz_fin %>%
    st_as_sf(.,coords = c("GPS-Longitude","GPS-Latitude"),crs=4326)

kluz_sf_tf <- kluz_sf %>%
  st_transform(.,crs=st_crs(crs(naip_1)))

#getting the raster image together
naip_1<-rast("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/GMAP2 Data test/joliet_NAIPimagery/m_4108831_se_16_060_20190914.tif")
naip_2<-rast("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/GMAP2 Data test/joliet_NAIPimagery/m_4108839_ne_16_060_20190914.tif")


ext <- st_bbox(kluz_sf_tf)  #Setting extents (buffers) around monitoring data for imagery, 50 is arbitrary and can be changed as needed
ext[1] <- ext[1] - 50
ext[2] <- ext[2] - 50
ext[3] <- ext[3] + 50
ext[4] <- ext[4] + 50
ext
naip_mos <- mosaic(naip_1,naip_2)

writeRaster()
naip_mos_crop <- crop(naip_mos,ext)
plotRGB(naip_mos_crop)

naip_mos_crop_df<- as.data.frame (naip_mos_crop,xy=T) %>%
  rename(Red = 3,
         Green = 4,
         Blue = 5)

#subsetting to show which ananlytes a user would want to be displayed
kluz_sf_tf_ma01 <- kluz_sf_tf %>%
  filter(id=="MA02" & header=="H2S")

ggplot(data=kluz_sf_tf)+
  geom_raster(data = naip_mos_crop_df, aes(x = x, y = y),
              fill = rgb(r = naip_mos_crop_df$Red,
                         g = naip_mos_crop_df$Green,
                         b = naip_mos_crop_df$Blue,
                         maxColorValue = 255)) +
  geom_sf(aes(color=value),size=4)
  geom_spatraster(data = naip_mos_crop)

ggplot()

MA_map(x=kluz_sf,analyte = "CH4", extent="w",rast_path = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/GMAPR2/test_interim_raster_mosaic.tif",
                  campaign = "test",multi_rast = NULL,color_vec = c("blue","green","salmon","pink","turquoise"))


kluz_st_data <- samp_kluz_fin %>%
  filter(!header=="ws"|!header=="wd")

kluz_st_wswd <- samp_kluz_fin %>%
  filter(header=="ws"|header=="wd") %>%
  pivot_wider(.,id_cols = c(id,TimeStamp),names_from = header)

kluz_st_only <- kluz_st_data %>%
  left_join(.,kluz_st_wswd) %>%
  filter(id=="ST02") %>%
  filter(header=="H2S") %>%
  ungroup()

ggplot(data=kluz_st_only, aes(x=wd,y=value))+
  geom_col()

ST_map(kluz_st_only,rast_path = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/GMAPR2/test_interim_raster_mosaic.tif")

#####,##sf_data###,,

tspt <- ts_plot(tf_2, grp = c(1,2), analyte = c("H2S","CH4"),units = "TESTUNITS")
wind_legend<-readJPEG("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/Picture1.jpg")

wind_legend_grob<-rasterGrob(wind_legend,interpolate = T)
ggplot() +
  background_image(wind_legend)+
  theme_transparent()
  geom_image(wind_legend,aes(image="C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/wind_direction_key.png"))

print(wind_legend)
analyte = c("H2S","CH4")
grp = c(1,2)
input <- tf_2 %>%
  filter(str_detect(header,"ANALYTE_")) %>%
  mutate(header = gsub("ANALYTE_","",header)) %>%
  filter(!header == "ws" & !header == "wd")
input_ws_wd_lat_long <-  tf_2 %>%
  filter(str_detect(header,"ANALYTE_")) %>%
  mutate(header = gsub("ANALYTE_","",header)) %>%
  filter(header=="ws"|header=="wd"|header=="GPS-Latitude"|header=="GPS-Longitude") %>%
  pivot_wider(.,id_cols = TimeStamp,names_from = header)
header <- analyte
groupings <- tibble(grp, header)
print(groupings)
groupings_1<-groupings %>%
  mutate(header=strsplit(header,","))%>%
  unnest(header)
input_2<-input %>%
  left_join(.,groupings_1,by="header") %>%
  left_join(.,input_ws_wd_lat_long,by="TimeStamp")
input_3 <-input_2 %>%
  filter(!is.na(grp))

center<-t(matrix(c(40.93832578275309, -88.65307604150641)))
lat_long_samp<-cbind(input_3$`GPS-Longitude`,input_3$`GPS-Latitude`)

input_4<- input_3 %>%
  mutate(azmiuth = bearing(cbind(input_3$`GPS-Longitude`,input_3$`GPS-Latitude`),center))

wd_plot_prep <- input_3 %>%
  # filter(TimeStamp %in% plot_in_tran$TimeStamp) %>%
  group_by(TimeStamp)%>%
  distinct(TimeStamp,.keep_all = T)%>%
  mutate(wd_rad=-(wd)+90+180)%>%
  # ungroup()%>%
  mutate(direction_section = case_when(wd_rad>=0 & wd_rad <=45 ~ "N-NE",
                                       wd_rad>45 & wd_rad <=90 ~ "NE-E",
                                       wd_rad>90 & wd_rad <=135 ~ "E-SE",
                                       wd_rad>135 & wd_rad <=180 ~ "SE-S",
                                       wd_rad>180 & wd_rad <=225 ~ "S-SW",
                                       wd_rad>225 & wd_rad <=270 ~ "SW-W",
                                       wd_rad>270 & wd_rad <=315 ~ "W-NW",
                                       wd_rad>315 & wd_rad <=360 ~ "NE-E",
                                       .default = "NA"))%>%
  mutate(date_fact=seq_along(nrow(.)))
# filter(header==groupings_1$header)

max_plots <-max(input_3$grp)


#qual_sum test ####
p <- qual_sum(tf_2,mdl)

