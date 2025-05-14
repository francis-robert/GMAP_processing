#test 2

setwd("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/may_2_2025_test")

mdl<-read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/2024_mdl_list.csv")
#rawdataprep_test#####
# test<-rawdataprep("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/GMAP2 Data test",
#                   time_zone = "America/Chicago")
test<-rawdataprep("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/may_2_2025_test",
                   time_zone = "America/Chicago")
######
#trouble shooting the the issue where MA04 had not header
# files_list <- list.files("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/may_2_2025_test", full.names = TRUE, recursive = TRUE)
#
# MA_temp <- read.table(files_list[4],skip=20, sep = "/t",
#                       fill = TRUE, na.strings = "NaN")
# header_MA_temp <- read.table(files_list[4],sep = "/t", skip = 15, nrows = 5,fill=T,header = F) %>%
#   replace(is.na(.),"NA") %>%
#   mutate(across(everything(),~str_replace_all(.,"_","-"))) %>%
#   mutate(across(everything(),~str_replace_all(.," ","-"))) %>%
#   mutate(across(everything(),~sub("^$","BLANK",.))) %>%
#   pivot_longer(.,cols = 1:ncol(.)) %>%
#   mutate(name = as.numeric(gsub("V", "", name))) %>%
#   group_by(name) %>%
#   summarise(value = str_c(value, collapse="_")) %>%
#   pivot_wider(.)
#
# colnames(MA_temp) <- header_MA_temp
#
# MA_temp_3 <- MA_temp[,!duplicated(colnames(MA_temp))]
#
# MA_temp_4 <- MA_temp_3 %>%
#   mutate(across(everything(),as.character)) %>%
#   mutate(`Type_DeviceName_ResidenceTime-(s)_Units_TimeStamp` = gsub("//..*","",`Type_DeviceName_ResidenceTime-(s)_Units_TimeStamp`)) %>%
#   mutate(`Type_DeviceName_ResidenceTime-(s)_Units_TimeStamp`=if_else(str_detect(`Type_DeviceName_ResidenceTime-(s)_Units_TimeStamp`,":")==FALSE,paste0("01/01/1700 00:00:00"),`Type_DeviceName_ResidenceTime-(s)_Units_TimeStamp`)) %>%
#   mutate(`Type_DeviceName_ResidenceTime-(s)_Units_TimeStamp` = as.POSIXct(`Type_DeviceName_ResidenceTime-(s)_Units_TimeStamp`, tz="America/Chicago", tryFormats = c("%m/%d/%Y %H:%M:%S")))
#####
df_test <- rawlist_2_df(test,"MA",campaign = "GMAPTEST_5_7_25")

comb<-MA_ST_bind(df_test)

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


split_comb<-splitsville(tf_2)

samp_int_local<-read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/may_2_2025_test/trans_method_250507.csv")

pic_data<-data.frame(split_comb[["picarro"]])

pic_data_sub <- subsamp_temporal_pic(pic_data)

syft_data <- data.frame(split_comb[["syft"]])

syft_data_sub <- subsamp_temporal_syft(syft_data,samp_int_local)
syft_data_zeros <-subsamp_temporal_syft_zero(syft_data, samp_int_local)

syft_data_sub_all <- syft_data_sub %>%
  bind_rows(.,syft_data_zeros)
met_data<-data.frame(split_comb[["metgps"]])

flow_check_test <- flow_check(met_data, flow_out = "both", flow_low = 3, flow_high = 7)


met_qa_test <- met_data %>%
  groundspeed_flagging(.,flag="IL") %>%
  gps_precision_flagging(.,flag="QX")

syft_mdl <- read.csv ("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/2025_mdl_syft.csv")
syft_data_qa<- syft_flagging(x=syft_data_sub_all,y=syft_mdl)

test_pic_flag <- pic_flagging(pic_data_sub, mdl, h2shs = 10000, ch4hs = 15)

timeseries<-ts_plot(syft_data_sub_all,met_data,grp = c("A","B","Y","Z","BleepBloop"),
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
                  z=break_pt, analyte = "benzene",extent = "w", transect= "250507_MA10",campaign = "test_test",
                  rast_type = "landsat",pt_size = 2,color_pal = "wed",zoom_scale = 500)


#testing how to get the extent for the the syft data
ext_syft <- met_data %>%
  filter(header=="GPS-Latitude"|header=="GPS-Longitude") %>%
  pivot_wider(.,id_cols = TimeStamp,names_from = header) %>%
  st_as_sf(.,coords = c("GPS-Latitude","GPS-Longitude")) %>%
  st_bbox(.)
