gps_zero_id <- function(x){
  gps_check <- x %>%
    filter(header =="GPS-Latitude" | header == "GPS-Longitude" | header ==  "GPS-DoP") %>%
    pivot_wider(.,id_cols = c(id,TimeStamp), names_from = header, values_from = value) %>%
    filter(`GPS-Latitude` == 0 | `GPS-Longitude` == 0)
 
  
  
   if (nrow(gps_check)>0){
    print("THESE DATA HAVE LAT/LONG of ZERO, unless you are actively in the atlantic ocean please remove these data before mapping")
     return(gps_check)
  }else{
    print("data have no zero lat/longs")
  }
  }
  