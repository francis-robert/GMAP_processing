
mdl_df<-read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/2024_mdl_list.csv")
#rawdataprep_test#####
# test<-rawdataprep("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/GMAP2 Data test",
#                   time_zone = "America/Chicago")
test<-rawdataprep( "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/gmap_test_03_18_25",
                   time_zone = "America/Chicago")
#rawlist_2_df test####
df_test <- rawlist_2_df(test,"MA",campaign = "GMAPTEST_2")
df_test_st <-rawlist_2_df(test,"ST",campaign = "GMAPTEST_2")

#MA_ST_bind test ####
comb<-MA_ST_bind(df_test,df_test_st)

#setting on and off status of specific transects

comb_onoff <- onoff(comb,transect = c("250318_MA09","250318_MA10","250318_MA11"))

#time flag test####
# test_pic_flag <- test_pic_flag %>%
#   mutate(time_flag="NA")


tf<-time_flagging(comb_onoff,
                  timestart = "02/18/2025 09:45:00",
                  timestop = "03/18/2025 10:54:00",
                  timeqt = "TESTTESTTEST",
                  analyte = c("H2S","acrolein","ccl4"))

tf_2<-time_flagging(tf,
                    timestart = "03/18/2025 10:45:00",
                    timestop = "03/18/2025 11:01:00",
                    timeqt = "WHOOPER",
                    analyte = c("H2S","CH4","acetone"))
#doiong all the flagging before the splitting
samp_int_local<-read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/GMAPR2/trans_method.csv")
syft_mdl <- read.csv ("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/2025_mdl_syft.csv")


flagged <- tf_2 %>%
  groundspeed_flagging(.,flag="QX") %>%
  gps_precision_flagging(.,flag="GPS") %>%
  pic_flagging(., mdl_df, h2shs = -1000, ch4hs = 10) %>%
  syft_flagging(.,syft_mdl)

flow_check(flagged,flow_out = "both",flow_low = 3, flow_high = 3)

#splitting the data by instrument
split_comb<-splitsville(flagged)
#####getting the data subset to match the sampling interval####

# samp_interval_df<-data.frame("method"=c("picarro","method_btex","method_to15"),
#                    "res_time"=c(3,3,10))



# syft_test<-split_comb[["syft"]] %>%
#   filter(!is.na(value)) %>%
#   filter(value>0) %>%
#   filter(str_detect(header,"ANALYTE_")) %>%
#   mutate(header = gsub("ANALYTE_","",header)) %>%
#   left_join(.,samp_int_local,by="id") %>%
#   group_by(header)%>%
#   arrange(TimeStamp) %>%
#   mutate(time_grp=rleid(value)) %>%
#   group_by(header,time_grp) %>%
#   mutate(group_id=n()) %>%
#   unite("analyte_timegrp_idgrp",c("header","time_grp","group_id"),sep="_",remove = F) %>%
#   mutate("sec_div_cyl"=floor(group_id/cyl_time))%>%
#   # mutate(sec_div_cyl=case_when(sec_div_cyl==0~1,
#   #                              .default = sec_div_cyl)) %>%
#   distinct(analyte_timegrp_idgrp,.keep_all = T) %>%
#   ungroup() %>%
#   summarise(total=sum(sec_div_cyl))
#   # left_join(.,y,by="id",relationship = "many-to-many") %>%
#   ungroup() %>%
#   group_by(analyte_timegrp_idgrp) %>%
#
#   mutate(sec_div_cyl=case_when(sec_div_cyl==0~1,
#                                .default = sec_div_cyl))

# picarro_test <- split_comb[["picarro"]]
#
# x_zero<-subsamp_temporal_syft_zero(split_comb[["syft"]],samp_int_local)
#
# x_zero_2 <-subsamp_temporal_syft_zero(split_comb[["syft"]],samp_int_local)
# d<-x_zero %>%
#   distinct(header_grpid_grpnum,.keep_all = T)
#
# x<-subsamp_temporal_syft(split_comb[["syft"]],samp_int_local)
#
# y<-x%>%
#   bind_rows(x_zero) %>%
#   arrange(TimeStamp)
#
# d<-subsamp_temporal_pic(split_comb[["picarro"]])
# ungroup()
#   distinct(analyte_timegrp_idgrp,.keep_all = T)
#   ungroup()%>%
#   mutate(row_num=row_number())
#
# t<-x %>%
#   distinct(analyte_timegrp_idgrp,.keep_all = T) %>%
#   mutate(sec_div_cyl=case_when(sec_div_cyl==0~1,
#                                .default = sec_div_cyl))
# k<-syft_test%>%
#   anti_join(.,x,by="value")
# s<-subsamp_temporal_syft(split_comb[["syft"]],samp_int_local)
#
# s_ct3<-s %>%
#   filter(cyl_time==3)
# s_ct10<-s %>%
#   filter(cyl_time==10)
# d<-x %>%
#   # filter(header=="xyleth")
# # & value ==1.839155)
#   filter(group_id==3|group_id==4|group_id==2)

#picarro data test
pic_data<-data.frame(split_comb[["picarro"]])

pic_data_sub <- subsamp_temporal_pic(pic_data)

syft_data <- data.frame(split_comb[["syft"]])

syft_data_sub <- subsamp_temporal_syft(syft_data,samp_int_local)
syft_data_zeros <-subsamp_temporal_syft_zero(syft_data, samp_int_local)

syft_data_sub_all <- syft_data_sub %>%
  bind_rows(.,syft_data_zeros)
met_data<-data.frame(split_comb[["metgps"]])

# flow_check_test <- flow_check(met_data, flow_out = "both", flow_low = 3, flow_high = 7)


# met_qa_test <- met_data %>%
#   groundspeed_flagging(.,flag="IL") %>%
#   gps_precision_flagging(.,flag="QX")


# d<-groundspeed_flagging(met_data,flag="IL")
#
# x<-d %>%
#   filter(gs_flag=="IL")
# gps_flag <- gps_precision_flagging(met_data,flag="QX")
#
#
# syft_data_qa<- syft_flagging(x=syft_data_sub_all,y=syft_mdl)

# syft_data_zero_qa<- syft_flagging(x=syft_data_zeros,y=syft_mdl)
#####a bunch of messing with the syft data zero process####
# input_test <-syft_data %>%
#   ungroup() %>%
#   filter(value==0) %>%
#   filter(str_detect(header,"ANALYTE_")) %>%
#   mutate(header = gsub("ANALYTE_","",header)) %>%
#   left_join(.,samp_int_local,by="id",relationship = "many-to-many") %>%
#   group_by(id,header) %>%
#   mutate(group_id=cumsum(c(TRUE,diff(TimeStamp)>1))) %>%
  # group_by(header,group_id) %>%
  # group_by(group_id) %>%
  # ungroup()%>%
  # unite(header_grpid_grpnum,c("id","header","group_id"),sep="_",remove = F) %>%
  # group_by(header_grpid_grpnum) %>%
  # mutate(group_num=n()) %>%
  # mutate(sec_div_cyl=floor(group_num/cyl_time)) %>%
  # mutate(cyl_time=as.numeric(cyl_time))
  # unite(header_grpid_grpnum,c("id","header","group_id","group_num"),sep="_",remove = F) %>%

#
# input_test2 <-syft_data %>%
#   ungroup() %>%
#   filter(value==0) %>%
#   filter(str_detect(header,"ANALYTE_")) %>%
#   mutate(header = gsub("ANALYTE_","",header)) %>%
#   left_join(.,samp_int_local,by="id",relationship = "many-to-many") %>%
#   mutate(group_id=cumsum(c(TRUE,diff(TimeStamp)>1))) %>%
#   # group_by(header,group_id) %>%
#   group_by(group_id) %>%
#   mutate(group_num=n()) %>%
#   mutate(sec_div_cyl=floor(group_num/cyl_time)) %>%
#   mutate(cyl_time=as.numeric(cyl_time)) %>%
#   unite(header_grpid_grpnum,c("id","group_id","group_num"),sep="_",remove = F) %>%
#   group_by(header_grpid_grpnum)
#

# mapply(input_test,input_test2,FUN=function(v1,v2) all(input_test==input_test2) )
#
# identical(input_test,input_test2)
#
# all.equal(input_test,input_test2)
#
# f<-input_test2 %>%
#   anti_join(.,input_test, by=c("group_num"))

# input_1 <- input_test %>%
#   filter(group_num < cyl_time) %>%
#   slice_min(TimeStamp) %>%
#   ungroup()
# input_2 <- input_test %>%
#   filter(group_num >= cyl_time) %>%
#   slice(.,seq(0,n(), by = unique(cyl_time))) %>%
#   ungroup()
#
# input_3<-input_1 %>%
#   rbind(input_2)
# input_examp <-syft_data %>%
#   ungroup() %>%
#   filter(value==0) %>%
#   filter(str_detect(header,"ANALYTE_")) %>%
#   mutate(header = gsub("ANALYTE_","",header)) %>%
#   left_join(.,samp_int_local,by="id",relationship = "many-to-many") %>%
#   group_by(id,header) %>%
#   mutate(group_id=cumsum(c(TRUE,diff(TimeStamp)>1))) %>%
#   # group_by(header,group_id) %>%
#   # group_by(group_id) %>%
#   ungroup()%>%
#   unite(header_grpid_grpnum,c("id","header","group_id"),sep="_",remove = F) %>%
#   group_by(header_grpid_grpnum) %>%
#   mutate(group_num=n()) %>%
#   # unite(header_grpid_grpnum,c("id","header","group_num"),sep="_",remove=F) %>%
#   # group_by(header_grpid_grpnum) %>%
#   mutate(sec_div_cyl=floor(group_num/cyl_time)) %>%
#   mutate(sec_div_cyl=case_when(sec_div_cyl==0~1,
#   .default = sec_div_cyl)) %>%
#   ungroup()%>%
#   distinct(header_grpid_grpnum,.keep_all = T)%>%
#   ungroup() %>%
#   summarise(total=sum(sec_div_cyl))

##pic_flag test ####
# test_pic_flag <- pic_flagging(pic_data_sub, mdl, h2shs = -1000, ch4hs = 10)

# unique(test_pic_flag$mdl_flag)
# x <- test_pic_flag %>% filter(header == "ANALYTE_H2S")
# View(x)



#transect start and stop time (can use comb, time_pic_flag, or tf after run time flagging)

tran_time_minmax <- transect_time_minmax(flagged)

# d<-transect_max(comb_onoff)




#testing to see why there are nas for lat long

# tf_lat_long_test<- tf_2 %>%
#   filter(str_detect(header,"ANALYTE_")) %>%
#   mutate(header = gsub("ANALYTE_","",header)) %>%
#   filter(header=="GPS-Latitude" | header=="GPS-Longitude")
#transect_max test, trans time min max test####

tran_max<- transect_max(flagged)

tran_time <- transect_time_minmax(tf_2)


#time series test #####
tst<- ts_table(tf_2,rm_flagged = "no")

#output data & max csv test ####
output <- output_csv_data(x=test_pic_flag,y=met_qa_test, output_type = "picarro")

out_distinct<-output %>%
  distinct(.)

#testing the mapping functions
timeseries<-ts_plot(syft_data_sub_all,met_data,grp = c("A","B","Y"),
                    analyte=c("H2S,benzene","toluene,xyleth,acetone","acrolein,butadiene"),
                    time_labels="300 sec")

timeseries_single<-ts_plot(syft_data_sub_all,met_data,analyte = "benzene",grp = "A",
                    time_labels="300 sec",
                    multi_analyte = F,mdl = syft_mdl,proc="btex")
# n_arrow <-data.frame(readPNG ("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Pictures/north_arrow.png"))
#
#
# tf_3<-tf_2 %>%
#   bind_rows(tf_2[1:10,]) %>%
#   mutate(value=case_when(header=="ANALYTE_H2S" & value<=0~5000,
#                          .default = value))
break_pt <- read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/GMAPR2/analyte_breaks.csv")
MA_test_2<-MA_map(x= syft_data_sub_all,y=met_data,rast_path = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/landsat_gmap_test",
                z=break_pt,analyte = "propene",extent = "w", transect= "250318_MA07",campaign = "test_test",
                rast_type = "landsat",pt_size = 2,color_pal = "wed",zoom_scale = 100)

st_test<- ST_map(x= syft_data_sub_all,y=met_data,z=break_pt,
                 rast_path = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Downloads/m_4308757_se_16_060_20220623/m_4308757_se_16_060_20220623.tif",
                 analyte = "benzene",
                 transect="250318_ST02",campaign = "test_test_test",
                 ,zoom_scale = 200,color_pal = "o")

# color_breaks <- break_pt %>%
#   filter(analyte=="benzene")
# tf_2 |>
#   dplyr::summarise(n = dplyr::n(), .by = c(TimeStamp, header)) |>
#   dplyr::filter(n > 1L)
#time series plot test###

######
analyte = c("H2S,benzene","toluene,xyleth,acetone","acrolein,butadiene")
grp = c("A","B","Y")

header=analyte
groupings <- tibble(grp, header)
groupings_1<-groupings %>%
  mutate(header=strsplit(header,","))%>%
  unnest(header)

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
# header <- analyte
# groupings <- tibble(grp, header)
# print(groupings)
# groupings_1<-groupings %>%
#   mutate(header=strsplit(header,","))%>%
#   unnest(header)
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
  filter(id=="MA05")

input_multi <- input %>%
  unite("id_grp",c(id,grp)) %>%
  left_join(.,mean_wd,by="TimeStamp") %>%
  group_by(id_grp) %>%
  drop_na(wd_rad)

input_multi_list <- input_multi %>%
  # mutate(Time= as.POSIXct(Time, format = "%H:%M:%S")) %>%
  group_split(.,.keep = T)
ggplot(input_multi_list[[9]],aes(x=Time,y=ws,angle=wd_rad,radius = .5))+
  # geom_point(aes(x=Time,y=ws))+
  geom_spoke(arrow = arrow(ends = "first",length = unit(.05, 'inches')))
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
  select(TimeStamp,header,value) %>%
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
  filter(!header=="ANALYTE_GPS-Latitude" & !header=="ANALYTE_GPS-Longitude") %>%
  left_join(.,latlong_dash,by="TimeStamp") %>%
  mutate(crs = "EPSG:4326") %>%
  mutate(epa_region = 5) %>%
  mutate(ma_st = substr(id,start=1,stop=2)) %>%
  mutate(met_bool= 1) %>%
  filter(!header=="ANALYTE_ws" & !header=="ANALYTE_wd") %>%
  left_join(.,wdws_dash,by="TimeStamp") %>%
  mutate(naics="NA") %>%
  mutate(naics_industry="NA") %>%
  mutate(tags="NA") %>%
  mutate(operators = "Haile.Kate, Hamilton.Scott") %>%
  mutate(validated_bool=0) %>%
  mutate(report_bool=0) %>%
  mutate(links="NA") %>%
  mutate(QAPP="NA") %>%
  mutate(requestor="WDNR") %>%
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

write.csv(dash_ex_2,"GMAP_VAN_examp_march.csv")


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
  left_join(.,sf_data_loc, by=c("TimeStamp","id"))
  st_as_sf(.,coords = c("GPS-Longitude","GPS-Latitude"),crs=4326)
  filter(header=="H2S" & id=="MA02")

kluz_sf <- samp_kluz_fin %>%
  st_as_sf(.,coords = c("GPS-Longitude","GPS-Latitude"),crs=4326)

kluz_sf_tf <- kluz_sf %>%
  st_transform(.,crs=st_crs(crs(naip_1)))

#getting the raster image together
naip_1<-rast("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/GMAP2 Data test/joliet_NAIPimagery/m_4108831_se_16_060_20190914.tif")
naip_2<-rast("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/GMAP2 Data test/joliet_NAIPimagery/m_4108839_ne_16_060_20190914.tif")

landsat<- list.files(path="C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/landsat_gmap_test"
                     , pattern = c("B4|B3|B2"),full.names = T)

landsat_rast<-rast(landsat)

landsat_rast_rbg<-rast(c(red=landsat_rast[[grepl("B4",landsat_rast)]],
                    green=landsat_rast[[grepl("B3",landsat_rast)]],
                    blue=landsat_rast[[grepl("B2",landsat_rast)]]))
landsat_cl
plot(landsat_rast_rbg)
plotRGB(landsat_rast,scale=65535)
x<-RGB(landsat_rast)<-c(4,3,2)

data_nolocs <- tf_2 %>%
  filter(str_detect(header,"ANALYTE_")) %>%
  mutate(header = gsub("ANALYTE_","",header)) %>%
  filter(!header == "GPS-Latitude" | !header == "GPS-Longitude")

data_locs <- tf_2 %>%
  filter(str_detect(header,"ANALYTE_")) %>%
  mutate(header = gsub("ANALYTE_","",header)) %>%
  filter(header == "GPS-Latitude" | header == "GPS-Longitude") %>%
  pivot_wider(.,id_cols = c(TimeStamp,id),names_from = header)

data_comb <- data_nolocs %>%
  left_join(., data_locs, by=c("TimeStamp","id"))

data_sf <-data_comb %>%
  st_as_sf(.,coords = c("GPS-Longitude","GPS-Latitude"),crs=4326) %>%
  filter(grepl("MA",id))

raster_crs<-crs(landsat_rast)
data_sf_clip <-data_sf %>%
  st_transform(.,crs=st_crs(crs(raster_crs)))

ext<- data_sf_clip %>%
  st_bbox(.)

raster_clip <- crop(landsat_rast,ext)
as.data.frame (raster,xy=T)

plot(x)

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
  filter(header=="CH4") %>%
  ungroup() %>%
  mutate(value_bin=cut_number(value,n=5)) %>%
  rowwise() %>%
  mutate(wd_samp=sample(c(0:360),size=1,replace=TRUE)) %>%
  ungroup()%>%
  mutate(divs=cut_number(wd_samp,n=8)) %>%
  mutate(bins=case_when(wd_samp>=348.75 ~ "N",
                        wd_samp<=11.25  ~ "N",
                        wd_samp>11.25 & wd_samp<=33.75 ~"NNE",
                        wd_samp>33.75 & wd_samp<=56.25 ~"NE",
                        wd_samp>56.25 & wd_samp<=78.75 ~"ENE",
                        wd_samp>78.75 & wd_samp<=101.25 ~"E",
                        wd_samp>101.25 & wd_samp<=123.75 ~"ESE",
                        wd_samp>123.75 & wd_samp<=146.25 ~"SE",
                        wd_samp>146.25 & wd_samp<=168.75 ~"SSE",
                        wd_samp>168.75 & wd_samp<=191.25 ~"S",
                        wd_samp>191.25 & wd_samp<=213.75 ~"SSW",
                        wd_samp>213.75 & wd_samp<=236.25 ~"SW",
                        wd_samp>236.25 & wd_samp<=258.75 ~"WSW",
                        wd_samp>258.75 & wd_samp<=281.25 ~"W",
                        wd_samp>281.25 & wd_samp<=303.75 ~"WNW",
                        wd_samp>303.75 & wd_samp<=326.25 ~"NW",
                        wd_samp>326.25 & wd_samp<=348.75 ~"NNW",)) %>%
  mutate(divs=as.factor(divs)) %>%
  group_by(bins) %>%
  mutate(freq=(n())) %>%
  ungroup()%>%
  mutate(freq_perc=freq/sum(freq))%>%
  mutate(bins=factor(bins,levels=c("N","NNE","NE","ENE","E","ESE","SE","SSE",
                                   "S","SSW","SW","WSW","W","WNW","NW","NNW"))) %>%
  st_as_sf(.,coords = c("GPS-Longitude","GPS-Latitude"),crs=st_crs(4326))
kluz_st_only_sf<- st_as_sf(kluz_st_only,coords = c("GPS-Longitude","GPS-Latitude"),crs=st_crs(4326))

st_crs(kluz_st_only_sf)<-4326

kluz_st_only_sf_tsfm <- kluz_st_only_sf %>%
  st_transform(.,crs=st_crs(naip_1)) %>%
  mutate(long = st_coordinates(.)[,1]) %>%
  mutate(lat = st_coordinates(.)[,2])

wr_plot<-ggplot(data=kluz_st_only_sf, aes(x=bins,y=freq_perc,fill=divs))+
  geom_bar(stat="identity",position = "stack")+
  coord_polar(start = 2 * pi - pi/8)+
  theme(panel.grid.major = element_line(colour = NA),
        axis.line = element_line(colour = NA),
        panel.background = element_rect(fill= "transparent", colour = NA),
        plot.background = element_rect(fill= "transparent", colour = NA),
        text = element_blank(),
        axis.text = element_blank,
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")+
  labs(fill=paste0(kluz_st_only$header))

ggsave(wr_plot, file="wr_plot.png", bg= "transparent", device = NULL )

# wr_plot_img<- readPNG("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/GMAPR2/wr_plot.png")

wr_plot_img_df<-data.frame(x=404950.7,y=4595008,image=sample("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/GMAPR2/wr_plot.png",size = 1))

# wr_rast_grob<-rasterGrob(wr_plot_img,interpolate = T)

ggplot(data=kluz_st_only_sf_tsfm)+
  geom_sf(data = kluz_st_only_sf_tsfm,aes(color=divs),size=10,pch=15)+
  geom_raster(data = naip_mos_crop_df, aes(x = x, y = y),
              fill = rgb(r = naip_mos_crop_df$Red,
                         g = naip_mos_crop_df$Green,
                         b = naip_mos_crop_df$Blue,
                         maxColorValue = 255))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  geom_image(data=wr_plot_img_df,aes(x=x,y=y,image=image), size=.75)+
  labs(color=paste0(kluz_st_only_sf_tsfm$header))+
  theme(legend.key = element_rect(fill = NA, color = NA))
  # theme(plot.margin = margin(t = 0,  # Top margin
  #                      r = 100,  # Right margin
  #                      b = 0,  # Bottom margin
  #                      l = 0))

naip_plot
  ggplot(wr_plot_img_df, aes(x, y)) + geom_image(aes(image=image), size=1.5)
  annotation_custom(wr_rast_grob)
  geom_subplot(data=kluz_st_only, aes(x=bins,y=freq_perc,fill=divs))+
  geom_bar(stat="identity",position = "stack")+
  coord_polar(start = 2 * pi - pi/8)+
  theme(panel.background = element_blank())

d <- kluz_st_only %>%
  filter(bins=="SE")

sum(d$analyte_samp_norm)


test_raster<-rast("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/GMAPR2/test_interim_raster_mosaic.tif")

time_start<-Sys.time()
test_st_map<-ST_map(kluz_st_only,rast_path = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/GMAPR2/test_interim_raster_mosaic.tif",
       campaign="test",analyte="CH4",transect = "ST02",multi_rast = NULL)
time_end<-Sys.time()
test_st_map
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
# filter_clean<-syft_test %>%
#   filter(group_id==cyl_time | group_id==cyl_time+1 | group_id==cyl_time-1) %>%
#   slice_min(TimeStamp) %>%
#   mutate("sec_div_cyl"=floor(group_id/cyl_time))
#
# filter_dirty_below<-syft_test %>%
#   filter(!group_id==cyl_time & !group_id==cyl_time+1 & !group_id==cyl_time-1) %>%
#   filter(group_id < cyl_time) %>%
#   slice_min(TimeStamp) %>%
#   mutate("sec_div_cyl"=floor(group_id/cyl_time))
#
# filter_dirty_above<-syft_test %>%
#   filter(!group_id==cyl_time & !group_id==cyl_time+1 & !group_id==cyl_time-1) %>%
#   filter(group_id > cyl_time+1) %>%
#   mutate("sec_div_cyl"=floor(group_id/cyl_time))
#
# filter_dirty_above_1 <- filter_dirty_above %>%
#   filter(sec_div_cyl==1) %>%
#   slice_min(TimeStamp)
#
# filter_dirty_above_multi <- filter_dirty_above %>%
#   filter(!sec_div_cyl==1) %>%
#   arrange(TimeStamp) %>%
#   mutate(short_subset=group_id-sec_div_cyl) %>%
#   slice(.,seq(1, unique(short_subset))) %>%
#   slice(.,seq(1, n(), by = unique(cyl_time)))
#
# output <- filter_clean %>%
#   bind_rows(.,filter_dirty_below,filter_dirty_above_1,
#             filter_dirty_above_multi)
#
# ungroup() %>%
#   summarise(sum=sum(sec_div_cyl))
# syft_rt3<-syft_test %>%
#   left_join(.,samp_int_local,by="id") %>%
#   filter(cyl_time==3)
#
# syft_rt10 <- syft_test %>%
#   left_join(.,samp_int_local,by="id") %>%
#   filter(cyl_time==10)

#wd averaging figures ####

wind_data<- tf_2 %>%
  filter(header=="ANALYTE_wd")
time_test_wd<-wind_data %>%
  group_by(id) %>%
  mutate(time_interval=floor_date(TimeStamp,unit="hour")+minutes(floor(minute(TimeStamp)/1)*1))

wind_data_sub <- time_test_wd %>%
  filter(time_interval=="2025-03-18 09:30:00")
ggplot(wind_data_sub,aes(x=TimeStamp,y=value))+
  geom_point()
  geom_hline(aes(yintercept = 134.360848),lty="dashed")+
  geom_hline(aes(yintercept =  135.63958))
