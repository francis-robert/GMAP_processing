library(devtools)
library(testthat)
library(dplyr)
library(tidyr)
library(stringr)

path <- "C:/Users/khaile/Environmental Protection Agency (EPA)/R5ARD Air Monitoring and Analysis Section - GMAP/GMAP/.2024 GMAP season/Cleveland Cliffs Cleveland OH/raw data working/GMAP2 Data Cleveland Cliffs"
##raw data prep ####
rawdataprep <- function(path){
  files_list <- list.files(path, full.names = TRUE, recursive = TRUE)
  output_df <- c()
  out_df_MA <- c()
  out_df_ST <- c()
  data_MA <- subset(files_list, grepl("_MA", files_list)) #this needs to be changed at some point in the future
  if(length(data_MA) == 0){
    out_df_MA <- list(-9999,-9999,-9999)
    print("No Mapping Transects")
  }else{for (i in seq_along(data_MA)){
    MA_temp <- read.table(data_MA[[i]],skip=19, sep = "\t",
                          fill = TRUE, na.strings = "NaN")
    header_MA_temp <- read.table(data_MA[[i]],sep = "\t", skip = 14, nrows = 5,fill=T,header = F) %>%
      replace(is.na(.),"NA") %>%
      mutate(across(everything(),~str_replace(.,"_","-"))) %>%
      mutate(across(everything(),~str_replace(.," ","-"))) %>%
      mutate(across(everything(),~sub("^$","BLANK",.))) %>%
      pivot_longer(.,cols = 1:ncol(.)) %>%
      mutate(name = as.numeric(gsub("V", "", name))) %>%
      group_by(name) %>%
      summarise(value = str_c(value, collapse="_")) %>%
      pivot_wider(.)
    # header_MA_temp_2 <-make.unique(as.character(header_MA_temp[1,]), sep = "@")
     colnames(MA_temp) <- header_MA_temp
    #   MA_temp_2 <- MA_temp %>%
    #     select(where(function(x) !all(is.na(x))))
    #   names(MA_temp_2) <- gsub("\\@.*", "", names(MA_temp_2))
      MA_temp_3 <- MA_temp[,!duplicated(colnames(MA_temp))]
     MA_temp_4 <- MA_temp_3 %>%
       mutate(across(everything(),as.character)) %>%
       pivot_longer(.,cols = 2:ncol(.))
    out_df_MA[[data_MA[[i]]]]<- MA_temp_4
  }
    print("Mapping Transect Present")
  }
  
  data_ST <- subset(files_list, grepl("_ST", files_list)) #this needs to be changed at some point in the future
  if(length(data_ST) == 0){
    out_df_ST <- list(-9999,-9999,-9999)
    print("No Stationary Transects")
  }else{for (i in seq_along(data_ST)){
    ST_temp <- read.table(data_ST[[i]],skip=31, sep = "\t",
                          fill = TRUE, na.strings = "NaN")
    header_ST_temp <- read.table(data_ST[[i]],sep = "\t", skip = 26, nrows = 5,fill=T,header = F) %>%
      replace(is.na(.),"NA") %>%
      mutate(across(everything(),~str_replace(.,"_","-"))) %>%
      mutate(across(everything(),~str_replace(.," ","-"))) %>%
      mutate(across(everything(),~sub("^$","BLANK",.))) %>%
      pivot_longer(.,cols = 1:ncol(.)) %>%
      mutate(name = as.numeric(gsub("V", "", name))) %>%
      group_by(name) %>%
      summarise(value = str_c(value, collapse="_")) %>%
      pivot_wider(.)
     #header_ST_temp_2 <-make.unique(as.character(header_ST_temp[1,]), sep = "@")
     colnames(ST_temp) <- header_ST_temp
    #  ST_temp_2 <- ST_temp %>%
    #    select(where(function(x) !all(is.na(x))))
    #  names(ST_temp_2) <- gsub("\\@.*", "", names(ST_temp_2))
      ST_temp_3 <- ST_temp[,!duplicated(colnames(ST_temp))]
     ST_temp_4 <- ST_temp_3 %>%
       mutate(across(everything(),as.character)) %>%
       pivot_longer(.,cols = 2:ncol(.))
    out_df_ST[[data_ST[[i]]]]<- ST_temp_4
  }
  }
  print("Stationary Transects Present")
  output_df[["MA"]] <- out_df_MA
  output_df[["ST"]] <- out_df_ST
  return(output_df)
}

test <- rawdataprep("C:/Users/khaile/Environmental Protection Agency (EPA)/R5ARD Air Monitoring and Analysis Section - GMAP/GMAP/.2024 GMAP season/Cleveland Cliffs Cleveland OH/raw data working/GMAP2 Data Cleveland Cliffs")

#rawlist_2_df test####
x<- test
transect = "MA"
campaign = "ClevelandCliffs"
loc="off"
rawlist_2_df <- function(x, transect = c("MA","ST"),campaign = " ", loc = c("on", "off")){
  output_notime <- test[[transect]] %>%
    bind_rows( ,.id = "id") %>%
    mutate(id = gsub(paste0(".*","_",transect,sep=""),transect,id)) %>%
    mutate(id = gsub("\\..*","",id)) %>%
    rename_with(~ str_extract(.x, "TimeStamp") ,
                matches("TimeStamp")) %>%
    mutate(TimeStamp = gsub("\\..*","",TimeStamp)) %>%
    filter(TimeStamp == "") %>%
    mutate(TimeStamp = as.Date(TimeStamp))%>%
    mutate(TimeStamp = first(as.Date("01/01/0001 00:00:01",format = "%m/%d/%Y %H:%M:%S")) + seconds(1+(row_number()-1)))%>%
    mutate(TimeStamp = as.POSIXct(TimeStamp, tryFormats = c("%m/%d/%Y %H:%M:%S")))
  output <- test[[transect]] %>%
    bind_rows( ,.id = "id") %>%
    mutate(id = gsub(paste0(".*","_",transect,sep=""),transect,id)) %>%
    mutate(id = gsub("\\..*","",id)) %>%
    rename_with(~ str_extract(.x, "TimeStamp") ,
                matches("TimeStamp")) %>%
    mutate(TimeStamp = gsub("\\..*","",TimeStamp)) %>%
   # mutate(TimeStamp = if_else(TimeStamp == "",paste0("01/01/0001 00:00:01"),TimeStamp)) %>%
    mutate(TimeStamp = as.POSIXct(TimeStamp, tryFormats = c("%m/%d/%Y %H:%M:%S"))) %>%
    bind_rows(.,output_notime) %>%
    separate(TimeStamp, c("Date", "Time"), sep=" ", remove = FALSE) %>%
    mutate(campaign = campaign) %>%
    mutate(loc_samp = loc)
}


df_test <- rawlist_2_df(test,"MA",campaign = "ClevelandCliffs",loc="off")
df_test_st <-rawlist_2_df(test,"ST",campaign = "ClevelandCliffs",loc="off")

#MA_ST_bind test ####

MA_ST_bind <- function(x,y){
  output <- x %>%
    bind_rows(., y) %>%
    separate(name,c("type", "instrument", "residence_time_sec", "units", "header"), sep = "_") %>%
    mutate(header = case_when (str_detect(instrument, c("Picarro|Syft")) ~ paste0("ANALYTE_",header),
                               .default = header)) %>%
    mutate(value = as.numeric(value))
}

comb<-MA_ST_bind(df_test,df_test_st)


##pic flagging ####
pic_flagging <- function(x, y, h2shs=NULL,ch4hs=NULL){
  mdl <- read.csv(y, header = T) %>%
    mutate(analyte = paste0("ANALYTE_",analyte))
  
  data <- x %>%
    left_join(.,mdl, by=c("header" = "analyte")) %>%
    mutate(mdl_flag = "NA") %>%
    group_by(instrument, header) %>%
    #rowwise() %>%
    mutate(mdl_flag = case_when(instrument == "Picarro-G2204" & header == "ANALYTE_CH4" & value < (-abs(mdl)) ~ "ND",
                                instrument == "Picarro-G2204" & header == "ANALYTE_CH4" & value >= (-abs(mdl)) & value <= (abs(mdl)) ~ "MD",
                                instrument == "Picarro-G2204" & header == "ANALYTE_CH4" & value > (abs(mdl)) & value <= (abs(3 * mdl)) ~ "PQ",
                                instrument == "Picarro-G2204" & header == "ANALYTE_CH4" & value > ch4hs  ~ "EH",
                                instrument == "Picarro-G2204" & header == "ANALYTE_H2S" & value < (-abs(mdl)) ~ "ND",
                                instrument == "Picarro-G2204" & header == "ANALYTE_H2S" & value >= (-abs(mdl)) & value <= (abs(mdl)) ~ "MD",
                                instrument == "Picarro-G2204" & header == "ANALYTE_H2S" & value > (abs(mdl)) & value <= (abs(3 * mdl)) ~ "PQ",
                                instrument == "Picarro-G2204" & header == "ANALYTE_H2S" & value > h2shs  ~ "EH",
                                .default = "NA")) %>%
        mutate(qa_flag = case_when(instrument == "Picarro-G2204" & TimeStamp == as.POSIXct("0001-01-01 00:00:01", format = "%Y-%m-%d %H:%M:%S")~"DA",
                               .default = "NA")) %>%
    unite("mdl_qa_flag", c("mdl_flag", "qa_flag"), sep = ",") %>%
    mutate(mdl_qa_flag = gsub("NA,","",mdl_qa_flag)) %>%
    mutate(mdl_qa_flag = gsub(",NA","",mdl_qa_flag)) %>%
    mutate(mdl_qa_flag = gsub("NA","",mdl_qa_flag)) %>%
    mutate(value = replace(value, str_detect(mdl_qa_flag, "AV|BJ|AM|AN|AT|AZ|BA|BN|QX|DA"),NA)) %>%
    ungroup()
}

test_pic_flag <- pic_flagging(comb, "C:/Users/khaile/Environmental Protection Agency (EPA)/R5ARD Air Monitoring and Analysis Section - GMAP/GMAP/.2024 GMAP season/IL WI Landfills/2024_mdl_list.csv", h2shs = 10110, ch4hs = 15)

unique(test_pic_flag$mdl_qa_flag)
x <- test_pic_flag %>% filter(header == "ANALYTE_CH4")
View(x)
y <- test_pic_flag %>% filter(header == "ANALYTE_H2S")
View(y)

#transect start and stop time (can use comb, time_pic_flag, or tf after run time flagging)
transect_time_minmax <-function (x){
  output <- x %>%
    filter(TimeStamp > as.POSIXct("0001-01-01 00:00:01", format = "%Y-%m-%d %H:%M:%S"))%>%
    group_by(campaign, id) %>%
    summarise(min = min(TimeStamp),
              max = max(TimeStamp))
  return(output)
}

tran_time_minmax <- transect_time_minmax(comb)
View(tran_time_minmax)

#time flag test####

time_flagging <- function(x, timestart = " ", timestop = " ", timeqt = " ",
                          analyte = " ", procedure = " ") {
  analyte_vec <- paste0("ANALYTE_",unique(analyte))
  print(analyte_vec)
  timestart <- as.POSIXct(timestart,tryFormats = c("%m/%d/%Y %H:%M:%S"))
  print(timestart)
  timestop <- as.POSIXct(timestop,tryFormats = c("%m/%d/%Y %H:%M:%S"))
  
  if (all(!str_detect(colnames(x),"time_flag"))){
    inter_df <- x %>%
      mutate(time_flag = "NA") %>%
      mutate(rn = row_number())
    print("Time Flagging Column Added")
  }else{
    inter_df <- x %>%
      mutate(rn = row_number())
    print("Time Flagging Column Already Present")}
  df_change <- inter_df %>%
    filter(., str_detect(header, paste(analyte_vec, collapse="|"))) %>%
    filter(TimeStamp >= timestart & TimeStamp <= timestop) %>%
    mutate(time_flag = paste(time_flag,timeqt,sep = ","))
  
  df_no_change <- subset(inter_df, !(rn %in% df_change$rn))
  
  
  output <- df_change %>%
    bind_rows(.,df_no_change) %>%
    select(-rn)%>%
    mutate(time_flag = gsub("NA,","",time_flag))
  
  return(output)
}


Data_flagged<-time_flagging(test_pic_flag, timestart = "10/08/2024 06:54:00", timestop = "10/10/2024 17:15:00", timeqt = "2",
                            analyte = c("H2S","CH4"))


View(Data_flagged)

#transect_max test, trans time min max test####
transect_max <- function (x){
  output <- c()
  output_short <- x %>%
    filter(.,str_detect(instrument, c("Picarro|Syft"))) %>%
    group_by(campaign, instrument, id, header) %>%
    slice_max(value, n=1) %>%
    slice_max(TimeStamp, n=1) %>%
    mutate(header = gsub("ANALYTE_","",header)) %>%
    pivot_wider(.,id_cols = id,names_from = header)
  output_long <- x %>%
    filter(.,str_detect(instrument, c("Picarro|Syft"))) %>%
    group_by(campaign, instrument, id, header) %>%
    slice_max(value, n=1) %>%
    slice_max(TimeStamp, n=1) %>%
    mutate(header = gsub("ANALYTE_","",header)) 
  output[["short"]] = output_short
  output[["long"]] = output_long
  write.csv(output[["short"]],paste0(unique(output_long$campaign),"_transect_max_",unique(output_long$loc_samp),".csv"))
  
  return(output)
}

tran_max<- transect_max(Data_flagged)


#time series test #####
ts_table <- function(x,rm_flagged = c("yes", "no")){
  if(rm_flagged=="yes"){output <- x %>%
    filter(.,str_detect(instrument, c("Picarro|Syft")))%>%
    filter(mdl_flag == "NA" & time_flag == "NA")
  }else{output <- x %>%
    filter(.,str_detect(instrument, c("Picarro|Syft")))
  }
}


tst<- ts_table(Data_flagged,rm_flagged = "no")

#output data & max csv test ####

output_csv_data <- function (x, loc= c("on", "off")){
  output_flag_data <- x %>%
    ungroup() %>%
    unite("flag",c(mdl_qa_flag, time_flag), sep = ",", remove = FALSE) %>%
    mutate(flag = gsub("NA,","",flag)) %>%
    mutate(flag = gsub("NA","",flag)) %>%
    mutate(flag = gsub(", NA","",flag)) %>%
    filter(.,str_detect(instrument, c("Picarro|Syft"))) %>%
    group_by(instrument) %>%
    mutate(header = paste0(header, "_flag")) %>%
    mutate(value = flag) %>%
    ungroup () %>%
    select(TimeStamp, header, value) %>%
    pivot_wider(.,id_cols = TimeStamp, names_from = header)
  output_inter <- x %>%
    ungroup() %>%
    filter(!instrument == "AliCat-FP-25" & !header== "GPS-Time") %>%
    filter(loc_samp == loc) %>%
    select(TimeStamp, header, value) %>%
    pivot_wider(.,id_cols = TimeStamp, names_from = header)
  output_inter_2 <- x %>%
    ungroup() %>%
    filter(!instrument == "AliCat-FP-25" & !header== "GPS-Time") %>%
    filter(loc_samp == loc) %>%
    select(-header, -value,-mdl_qa_flag,-time_flag)
  output_fin <- output_inter_2 %>%
    left_join(.,output_inter,by="TimeStamp") %>%
    left_join(.,output_flag_data,by="TimeStamp") %>%
    filter(.,str_detect(instrument, c("Picarro|Syft")))
  #write.csv(output_fin,paste0(unique(output_fin$campaign),"_",unique(output_fin$loc_samp),".csv"))
  return(output_fin)
}


output <- output_csv_data(Data_flagged,loc="off")


output_max <- output_csv_max(Data_flagged,loc="off")

#time series plot test####

ts_plot <- function (x) {
  data <- x %>%
    mutate(header = gsub("ANALYTE_","", header))
  return(data)
}


tspt <- ts_plot(output)


#qual_sum test ####

qual_sum <- function(x,y) {
  mdl_sql <- read.csv(y) %>%
    mutate(analyte = paste0("ANALYTE_",analyte))
  data <- x %>%
    left_join(.,mdl_sql,by = c("header" = "analyte")) %>%
    mutate(gtr_sql = if_else(value > SQL,1,0)) %>%
    mutate(gtr_il = if_else(value > IL,1,0)) %>%
    mutate(flagged = if_else(!mdl_flag == "NA"|!time_flag == "NA",1,0)) %>%
    filter(.,str_detect(header,"ANALYTE_")) %>%
    group_by(header,id) %>%
    mutate(sum_tran = 1) %>%
    summarise(across(c(gtr_sql,gtr_il,flagged,sum_tran),sum)) %>%
    mutate(gtr_sql_perc = (gtr_sql/sum_tran)*100) %>%
    mutate(gtr_il_perc = (gtr_il/sum_tran)*100) %>%
    mutate(flagged_perc = (flagged/sum_tran)*100) %>%
    mutate(header = gsub("ANALYTE_","",header)) %>%
    rename("analyte" = "header")
}




p <- qual_sum(Data_flagged,"C:/Users/khaile/Environmental Protection Agency (EPA)/R5ARD Air Monitoring and Analysis Section - GMAP/GMAP/.2024 GMAP season/IL WI Landfills/2024_mdl_list.csv")




## pollution roses ####

x <- read.csv("C:/Users/khaile/Environmental Protection Agency (EPA)/R5ARD Air Monitoring and Analysis Section - GMAP/GMAP/.2024 GMAP season/IL WI Landfills/Orchard Ridge Landfill/flagged data/OrchardRidgeLandfill_off.csv")

x <- x %>%
  rename("")

ST01 <- x %>%
  filter(id=="ST01")
ST02 <- x %>%
  filter(id == "ST02")
ST03 <- x %>%
  filter(id == "ST03")


openair::polarPlot(mydata=ST01,pollutant="CH4..ppm.",x="Wind.Speed",wd="Wind.Direction",statistic = "max")

openair::polarPlot(mydata=ST01,pollutant="CH4..ppm.",x="Wind.Speed",wd="Wind.Direction",statistic = "mean")

openair::pollutionRose(ST01,pollutant="CH4..ppm.",ws="Wind.Speed",wd="Wind.Direction",angle=5,breaks=c(-0.01,5,25,40,50),cols=c("lightskyblue3","deepskyblue3","yellow2","orange2","red2"),statistic="abs.count")


openair::pollutionRose(ST02,pollutant="CH4..ppm.",ws="Wind.Speed",wd="Wind.Direction",angle=5,breaks=c(-0.01,5,25,40,50),cols=c("lightskyblue3","deepskyblue3","yellow2","orange2","red2"),statistic="abs.count")

openair::pollutionRose(ST03,pollutant="CH4..ppm.",ws="Wind.Speed",wd="Wind.Direction",angle=5,breaks=c(-0.01,5,25,40,50),cols=c("lightskyblue3","deepskyblue3","yellow2","orange2","red2"),statistic="abs.count")



