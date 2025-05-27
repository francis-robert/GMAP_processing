#test 5_14_2025
mdl_df<-read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/2025_mdl.csv")
samp_int_local<-read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/GMAPR2/transect_procedure_250515.csv")
test<-rawdataprep("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/test_5_15_25",
                  time_zone = "America/Chicago")

df_test <- rawlist_2_df(test,"MA",campaign = "GMAPTEST_5_15_25")
df_test_2 <- rawlist_2_df(test,"ST",campaign = "GMAPTEST_5_15_25")

comb_ma<-MA_ST_bind(df_test, samp_int_local)

comb_st<-MA_ST_bind(df_test_2, samp_int_local)

comb<-comb_ma %>%
  bind_rows(comb_st)

comb_onoff<-onoff(comb)

tran_time_minmax <- transect_time_minmax(comb_onoff)

tran_max<- transect_max(comb_onoff)

tf<-time_flagging(comb_onoff,
                 timestart = "05/15/2025 09:18:00",
                 timestop = "05/15/2025 10:27:00",
                 timeqt = "TEST1",
                 analyte = c("H2S","mtbe" ,"ccl4","hexane","propene"))

tf_2<-time_flagging(tf,
                    timestart = "05/15/2025 10:20:00",
                    timestop = "05/15/2025 11:00:00",
                    timeqt = "TEST2",
                    analyte = c("CH4","acetone","mek","xyleth","toluene" ))

flagged <- tf_2 %>%
  groundspeed_flagging(.,flag="QX") %>%
  gps_precision_flagging(.,flag="GPS") %>%
  pic_flagging(., mdl_df, h2shs = -1000, ch4hs = 10) %>%
  syft_flagging(.)

flow_check(flagged,flow_out = "both",flow_low = 3, flow_high = 10)

split_comb<- splitsville(flagged)
pic_data<-data.frame(split_comb[["picarro"]])
syft_data <- data.frame(split_comb[["syft"]])
met_data<-data.frame(split_comb[["metgps"]])

pic_data_met <- met_analyte_comb(met_data,pic_data)
syft_data_met <-met_analyte_comb(met_data,syft_data)

syft_method_split<- method_split_syft(syft_data_met,samp_int_local)

syft_pet<-syft_method_split$petroleum

syft_btex<-syft_method_split$btex
pic_data_sub <- subsamp_temporal_pic(pic_data_met)
syft_data_sub_pet <- subsamp_temporal_syft(syft_pet)

syft_data_sub_btex <- subsamp_temporal_syft(syft_btex)

syft_sub_all<-syft_data_sub_pet %>%
  bind_rows(syft_data_sub_btex)

x<-output_csv_data(pic_data_sub,output_type = ("picarro"),write=F)
y<-output_csv_data(syft_sub_all,output_type = "syft",write=F)

timeseries<-ts_plot(syft_sub_all,tran_time_minmax, mdl_df,
                    grp = c("A","B","Y","Z","BleepBloop"),
                    analyte=c("dbe,mek,pce","trimethyl,tce",
                              "chbr3,ch3br","H2S",
                              "heptane,hexane"))

timeseries_2<-ts_plot(syft_data_met,tran_time_minmax, mdl_df,
                      grp = c("A","B","Y","Z","BleepBloop"),
                      analyte=c("dbe,mek,pce","trimethyl,tce",
                                "chbr3,ch3br","H2S",
                                "heptane,hexane"))

timeseries_single<-ts_plot(syft_sub_all,tran_time_minmax,mdl_df,
                           analyte = "benzene",grp = "A",
                           multi_analyte = F,
                           time_labels="60",
                           time_labels_graph = "60 sec")

break_pt <- read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Downloads/analyte_breaks_250520.csv")


syft_sub_all_bad_locs<- syft_sub_all%>%
  filter(!`GPS-Latitude`==0 &! `GPS-Longitude`==0)

MA_test_2<-MA_map(x= syft_sub_all_bad_locs,
                  rast_path = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Downloads/landsat_2/gmap_5_15_landsat_2",
                  z=break_pt,
                  analyte = "pce",
                  extent = "s",
                  transect= "250515_MA03",
                  rast_type = "landsat",pt_size = 2,color_pal = "wed",zoom_scale = 500)

st_test<-ST_map(x=syft_sub_all_bad_locs,
                rast_path = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Downloads/naip_test/m_4108710_ne_16_030_20230710.jp2",
                z=break_pt,
                analyte = "pce",
                transect= "250515_ST02",
                color_pal = "g",
                campaign = "GMAP_5_15_25")

x<-terra::rast("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Downloads/naip_test/m_4108710_ne_16_030_20230710.jp2")

d<-as.data.frame(x,xy=T)
write.csv(syft_sub_all_bad_locs,"test_out.csv")
