#test 2


mdl_df<-read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/2025_mdl.csv")
samp_int_local<-read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/may_2_2025_test/trans_method_250507.csv")
test<-rawdataprep("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/may_2_2025_test",
                   time_zone = "America/Chicago")

df_test <- rawlist_2_df(test,"MA",campaign = "GMAPTEST_5_7_25")

comb<-MA_ST_bind(df_test, samp_int_local)

comb_onoff <- onoff(comb)

tran_time_minmax <- transect_time_minmax(comb_onoff)

tran_max<- transect_max(comb_onoff)

tf<-time_flagging(comb_onoff,
                  timestart = "05/07/2025 12:15:43",
                  timestop = "05/07/2025 12:27:19",
                  timeqt = "TEST1",
                  analyte = c("H2S","acrolein","ccl4","hexane"))

tf_2<-time_flagging(tf,
                    timestart = "05/07/2025 12:25:00",
                    timestop = "05/07/2025 13:38:56",
                    timeqt = "TEST2",
                    analyte = c("CH4","acetone","mek","pce"))

# syft_mdl <- read.csv ("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/2025_mdl_syft.csv")


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

pic_data_sub <- subsamp_temporal_pic(pic_data_met)
syft_data_sub <- subsamp_temporal_syft(syft_data_met,samp_int_local)

x<-output_csv_data(pic_data_sub,output_type = ("picarro"),write=T)
y<-output_csv_data(syft_data_sub,output_type = "syft",write=T)

us_states <- read_sf("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/GMAPR2/cb_2018_us_county_20m/cb_2018_us_county_20m.shp")
timeseries<-ts_plot(syft_data_sub,met_data,grp = c("A","B","Y","Z","BleepBloop"),
                    analyte=c("benzene,dbe,mek,pce","toluene,xyleth,trimethyl", "chbr3,ch3br","H2S","heptane,hexane"),
                    time_labels="60 sec")

timeseries_single<-ts_plot(syft_data_sub_all,met_data,analyte = "benzene",grp = "A",
                           multi_analyte = F,mdl = syft_mdl,proc="petroleum",time_labels="60 sec")

break_pt <- read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/GMAPR2/analyte_breaks.csv")

start<-Sys.time()
raster_comb(rast_path = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Downloads/m_4108824_ne_16_030_20230710",
            campaign = "test_test")
rast_comb_time <- Sys.time()-start
MA_test_2<-MA_map(x= syft_data_sub_all,y=met_data,rast_path = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Downloads/landsat_2",
                  z=break_pt, analyte = "benzene",extent = "s", transect= "250507_MA14",campaign = "test_test",
                  rast_type = "landsat",pt_size = 2,color_pal = "wed",zoom_scale = 500)


