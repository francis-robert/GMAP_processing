output_csv_data <- function (x, write=T,output_type = c("syft","picarro","met")){

  if(write==T){
  out <- x %>%
    select(id,campaign,Date,Time,TimeStamp,`GPS-Latitude`,`GPS-Longitude`,time_gps_flag,ws,time_gs_flag,
           wd,header,value,time_mdl_flag,analyte_procedure,cyl_time,sec_div_cyl) %>%
    #mutate(wd_flag = gs_flag) %>%
    #separate(time_mdl_flag,c("time_flag","mdl_flag")) %>%
    rename("wind_flag"="time_gs_flag", "analyte"="header","analyte_flag"="time_mdl_flag","gps_flag"="time_gps_flag") %>%
    mutate(units = if_else(analyte=="CH4","ppm","ppb"))

  write.csv(out, paste0(unique(x$campaign),"_",output_type,".csv"))
  }else{
    print("No CSV output will be created")
    out <- x %>%
      select(id,campaign,Date,Time,TimeStamp,`GPS-Latitude`,`GPS-Longitude`,time_gps_flag,ws,time_gs_flag,
             wd,header,value,time_mdl_flag,analyte_procedure,cyl_time,sec_div_cyl) %>%
      #mutate(wd_flag = gs_flag) %>%
      #separate(time_mdl_flag,c("time_flag","mdl_flag")) %>%
      rename("wind_flag"="time_gs_flag", "analyte"="header","analyte_flag"="time_mdl_flag","gps_flag"="time_gps_flag") %>%
      mutate(units = if_else(analyte=="CH4","ppm","ppb"))
    return(out)
  }
}

