
kluz_data<-rawdataprep("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Downloads/OneDrive_2025-02-03/GMAP2 Data Kalunzy Bros")


#rawlist_2_df test####
df_test_kluz <- rawlist_2_df(kluz_data,"MA",campaign = "GMAPTEST_kluz",loc="off")
df_test_st_kluzt <-rawlist_2_df(kluz_data,"ST",campaign = "GMAPTEST_kluz",loc="off")

#MA_ST_bind test ####
comb_kluz<-MA_ST_bind(df_test_kluz,df_test_st_kluzt)

#getting the sample data ready for the kluz spatial location

samp_counts <- tf_2 %>%
  group_by(id,TimeStamp)%>%
  distinct(.,id,TimeStamp ,.keep_all = T)%>%
  ungroup() %>%
  group_by(id) %>%
  summarise(n())

kluz_loc <- comb_kluz %>%
  select(TimeStamp,id,header,value) %>%
  filter(str_detect(header,"ANALYTE_")) %>%
  mutate(header = gsub("ANALYTE_","",header))%>%
  filter(header=="GPS-Latitude"|header=="GPS-Longitude") %>%
  distinct(.,TimeStamp,id,header, .keep_all = T)%>%
  pivot_wider(.,id_cols = c(TimeStamp,id),names_from = header)

kluz_count <- kluz_loc %>%
  group_by(id) %>%
  summarise(n())

kluz_ma01_samp <- kluz_loc %>%
  filter(id=='MA01') %>%
  mutate(number = 1:nrow(.)) %>%
  filter(number %in% sample(unique(number),40)) %>%
  mutate(number_2 = 1:nrow(.)) %>%
  select(id,`GPS-Latitude`,`GPS-Longitude`,number_2)

kluz_ma02_samp <- kluz_loc %>%
  filter(id=='MA02') %>%
  mutate(number = 1:nrow(.)) %>%
  filter(number %in% sample(unique(number),101)) %>%
  mutate(number_2 = 1:nrow(.)) %>%
  select(id,`GPS-Latitude`,`GPS-Longitude`,number_2)


kluz_st01_samp <- kluz_loc %>%
  filter(id=='ST01') %>%
  mutate(number = 1:nrow(.)) %>%
  filter(number %in% sample(unique(number),1)) %>%
  bind_rows(replicate(36,.,simplify=F)) %>%
  mutate(number_2 = 1:nrow(.)) %>%
  select(id,`GPS-Latitude`,`GPS-Longitude`,number_2)

kluz_st02_samp <- kluz_loc %>%
  filter(id=='MA05') %>%
  mutate(id="ST02") %>%
  mutate(number = 1:nrow(.)) %>%
  filter(number %in% sample(unique(number),1)) %>%
  bind_rows(replicate(78,.,simplify=F)) %>%
  mutate(number_2 = 1:nrow(.)) %>%
  select(id,`GPS-Latitude`,`GPS-Longitude`,number_2)

kluz_samp_all <- rbind(kluz_ma01_samp,kluz_ma02_samp,kluz_st01_samp,kluz_st02_samp)

tf_samp <- tf_2 %>%
  filter(str_detect(header,"ANALYTE_")) %>%
  mutate(header = gsub("ANALYTE_","",header))%>%
  filter(!header=="GPS-Latitude"|!header=="GPS-Longitude")

samp_kluz_locs<-  tf_2 %>%
  filter(str_detect(header,"ANALYTE_")) %>%
  mutate(header = gsub("ANALYTE_","",header))%>%
  filter(!header=="GPS-Latitude"|!header=="GPS-Longitude") %>%
  distinct(.,id,TimeStamp,) %>%
  group_by(id) %>%
  mutate(number_2=row_number()) %>%
  left_join(.,kluz_samp_all,by=c("id","number_2")) %>%
  select(-number_2)

samp_kluz_fin<- tf_samp %>%
  left_join(.,samp_kluz_locs,by=c("id","TimeStamp"))
