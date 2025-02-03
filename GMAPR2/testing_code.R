
mdl<-read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/2024_mdl_list.csv")
#rawdataprep_test#####
test<-rawdataprep("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/GMAP2 Data test")


MA_temp <- read.table("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/GMAP2 Data test/Mapping/250203/250203_MA01.txt",skip=20, sep = "\t",
                      fill = TRUE, na.strings = "NaN")
header_MA_temp <- read.table("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/GMAP2 Data test/Mapping/250203/250203_MA01.txt",sep = "\t", skip = 15, nrows = 5,fill=T,header = F) %>%
  replace(is.na(.),"NA") %>%
  mutate(across(everything(),~str_replace_all(.,"_","-")))
  mutate(across(everything(),~str_replace_all(.," ","-")))
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


tf<-time_flagging(test_pic_flag, timestart = "09/11/2024 08:55:00", timestop = "09/11/2024 09:10:00", timeqt = "TESTTESTTEST",
              analyte = c("H2S","ws"))

tf_2<-time_flagging(tf,timestart = "09/11/2023 08:57:00", timestop = "09/11/2025 09:20:00", timeqt = "WHOOPER",
                    analyte = c("H2S","CH4","ws","wd"))

x<-tf_2 %>%
  filter(header=="ANALYTE_ws")
#transect_max test, trans time min max test####

tran_max<- transect_max(tf_2)


#time series test #####
tst<- ts_table(tf_2,rm_flagged = "no")

#output data & max csv test ####
output <- output_csv_data(tf_2,loc="off")

out_distinct<-output %>%
  distinct(.)



tf_2 |>
  dplyr::summarise(n = dplyr::n(), .by = c(TimeStamp, header)) |>
  dplyr::filter(n > 1L)
#time series plot test####

######
analyte

input <- tf_2 %>%
  filter(str_detect(header,"ANALYTE_")) %>%
  mutate(header = gsub("ANALYTE_","",header)) %>%
  filter(!header == "ws" & !header == "wd")
input_ws_wd_lat_long <- tf_2 %>%
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
# filter(header==groupings_1$header)
input_3 <-input_2 %>%
  filter(!is.na(grp))

time_test<-input_ws_wd_lat_long %>%
  group_by(id) %>%
  mutate(time_interval=floor_date(TimeStamp,unit="hour")+minutes(floor(minute(TimeStamp)/5)*5))

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
wd_plot<-ggplot(mean_wd_test,aes(x=TimeStamp,y=ws))+
  geom_text(aes(angle=wd_rad),label="→",size=7)+
  theme(axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),legend.position = "none")

analyte_plot<- input %>%
  filter(id=="MA01", header=="H2S")

y<-ggplot(analyte_plot, aes(x=TimeStamp,y=value,color=header))+
  geom_point()+
  scale_color_manual(values=c("blue3","darkorange","chartreuse3","firebrick2","blueviolet","orange4","violetred","honeydew4","gold2","turquoise2"),
                     drop=FALSE)+
  geom_hline(aes(yintercept=unique(mdl),linetype="MDL"))+
  geom_hline(aes(yintercept=unique(SQL),linetype="SQL"))+
  scale_linetype_manual("Critical Values",values=c("MDL"="dashed","SQL"="dotted"))

ggarrange(wd_plot,y, nrow = 2,ncol = 1, common.legend = T,legend = "bottom",align = "hv")

#####

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

