#rawdataprep_test#####
test<-rawdataprep("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/Livingston Landfill/GMAP2 Data Livingston Landfill")

# files<-list.files("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/Livingston Landfill/GMAP2 Data Livingston Landfill",
#                   full.names = T,recursive = T)
#
#
# test_MA <- subset(files, grepl("ABC", files))
# test_MA_result <- lapply(test_MA, skip=19, FUN = read.table, sep = "\t",
#                          fill = TRUE)
# x<- read.table(test_MA[1],sep = "/t", skip = 19,fill=T)
# select_if(~!all(is.na(.)))
# y<-read.table(test_MA[5],sep = "/t", skip = 19,fill=T)%>%
#   select_if(~!all(is.na(.)))
#
# d<-read.table(test_MA[5],sep = "/t", skip = 14, nrows = 5,fill=T,header = F) %>%
#   mutate(across(everything(),~str_replace(.,"_","-"))) %>%
#   mutate(across(everything(),~str_replace(.," ","-"))) %>%
#   mutate(across(everything(),~sub("^$","BLANK",.))) %>%
#   pivot_longer(.,cols=1:ncol(.)) %>%
#   group_by(name) %>%
#   summarise(value = str_c(value, collapse="_")) %>%
#   pivot_wider(.)
# test_header_MA <- read.table(test_MA[1],sep = "/t", skip = 14, nrows = 5,fill=T,header = F)
#
# test_header_collapse<-test_header_MA %>%
#   mutate(across(everything(),~str_replace(.,"_","-"))) %>%
#   mutate(across(everything(),~str_replace(.," ","-"))) %>%
#   mutate(across(everything(),~sub("^$","BLANK",.))) %>%
#   pivot_longer(.,cols=1:ncol(.)) %>%
#   group_by(name) %>%
#   summarise(value = str_c(value, collapse="_")) %>%
#   pivot_wider(.)

#rawlist_2_df test####
df_test <- rawlist_2_df(test,"MA",campaign = "LivingstonLandfill",loc="off")
df_test_st <-rawlist_2_df(test,"ST",campaign = "LivingstonLandfill",loc="off")

#MA_ST_bind test ####
comb<-MA_ST_bind(df_test,df_test_st)

##pic_flag test ####
# x<-comb%>%
#   filter(header== "CH4" | header== "H2S")

# mdl <- read.csv("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/2024_mdl_list.csv")

test_pic_flag <- pic_flagging(comb, "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/new_van_example/2024_mdl_list.csv", h2shs = 6.5, ch4hs = 7)

#time flag test####
# test_pic_flag <- test_pic_flag %>%
#   mutate(time_flag="NA")

tf<-time_flagging(test_pic_flag, timestart = "09/11/2024 08:55:00", timestop = "09/11/2024 09:10:00", timeqt = "TESTTESTTEST",
              analyte = c("H2S"))

tf_2<-time_flagging(tf,timestart = "09/11/2024 08:53:00", timestop = "09/11/2024 09:20:00", timeqt = "WHOOPER",
                    analyte = c("H2S","CH4"))
#transect_max test, trans time min max test####

tran_max<- transect_max(tf_2)

tran_time_minmax <- transect_time_minmax(tf_2)

#time series test #####
tst<- ts_table(tf_2,use_flag = "no")

#output data & max csv test ####
output <- output_csv_data(tf_2,loc="off")

output_max <- output_csv_max(tf_2,loc="off")

#time series plot test####

tspt <- ts_plot(output)
