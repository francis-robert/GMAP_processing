met_analyte_comb <- function(x,y){
  met <- x %>%
    select(TimeStamp,header,value) %>%
    filter(header=="ws"|header=="wd"|header=="GPS-Latitude"|header=="GPS-Longitude") %>%
    pivot_wider(.,id_cols = TimeStamp,names_from = header)

  ws_flag <- x %>%
    filter(header=="ws") %>%
    select(TimeStamp,gs_flag)
  gps_flag_df <- x %>%
    filter(header=="GPS-Latitude") %>%
    select(TimeStamp,gps_flag)

  met_comb <- met %>%
    left_join(.,ws_flag,by="TimeStamp") %>%
    left_join(.,gps_flag_df,by="TimeStamp")

  # analyte_df <- y %>%
  #   select(TimeStamp,header,value)%>%
  #   pivot_wider(.,names_from = header)


   output <- y %>%
     select(TimeStamp,header,value,time_flag,mdl_flag,analyte_procedure,instrument) %>%
     unite(time_mdl_flag,c("time_flag","mdl_flag")) %>%
     # pivot_wider(.,id_cols = c(TimeStamp,instrument),names_from = analyte_procedure,
     #             values_from =c(value,time_mdl_flag)) %>%
     left_join(.,met_comb,by="TimeStamp")
  }
