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
      mutate(across(everything(),~str_replace(.,"_","-"))) %>%
      mutate(across(everything(),~str_replace(.," ","-"))) %>%
      mutate(across(everything(),~sub("^$","BLANK",.))) %>%
      pivot_longer(.,cols = 1:ncol(.)) %>%
      mutate(name = as.numeric(gsub("V", "", name))) %>%
      group_by(name) %>%
      summarise(value = str_c(value, collapse="_")) %>%
      pivot_wider(.)
    header_MA_temp_2 <-make.unique(as.character(header_MA_temp[1,]), sep = "@")
    colnames(MA_temp) <- header_MA_temp_2
    MA_temp_2 <- MA_temp %>%
      select(where(function(x) !all(is.na(x))))
    names(MA_temp_2) <- gsub("\\@.*", "", names(MA_temp_2))
    MA_temp_3 <- MA_temp_2[,!duplicated(colnames(MA_temp_2))]
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
      mutate(across(everything(),~str_replace(.,"_","-"))) %>%
      mutate(across(everything(),~str_replace(.," ","-"))) %>%
      mutate(across(everything(),~sub("^$","BLANK",.))) %>%
      pivot_longer(.,cols = 1:ncol(.)) %>%
      mutate(name = as.numeric(gsub("V", "", name))) %>%
      group_by(name) %>%
      summarise(value = str_c(value, collapse="_")) %>%
      pivot_wider(.)
    header_ST_temp_2 <-make.unique(as.character(header_ST_temp[1,]), sep = "@")
    colnames(ST_temp) <- header_ST_temp_2
    ST_temp_2 <- ST_temp %>%
      select(where(function(x) !all(is.na(x))))
    names(ST_temp_2) <- gsub("\\@.*", "", names(ST_temp_2))
    ST_temp_3 <- ST_temp_2[,!duplicated(colnames(ST_temp_2))]
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
