ts_table <- function(x,rm_flagged = c("yes", "no")){
  if(rm_flagged=="yes"){output <- x %>%
    filter(.,str_detect(instrument, c("Picarro|Syft")))%>%
    filter(mdl_flag == "NA" & time_flag == "NA")
  }else{output <- x %>%
      filter(.,str_detect(instrument, c("Picarro|Syft")))
  }
  }
