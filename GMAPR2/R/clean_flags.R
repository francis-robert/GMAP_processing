clean_flags <- function(x) {
  #remove NA from flag columns (3)
  input <- x %>%
    mutate(across(contains("_flag"),~gsub("_NA","",.))) %>%
    mutate(across(contains("_flag"),~gsub("NA_","",.)))
  
  #override MDL/QA flags if a null flag is present
  data_clean <- input %>%
    mutate(across(contains("_flag"), ~if_else(str_detect(.,"AV|BJ|AM|AN|AT|AZ|BA|BN") == TRUE,
                                              gsub("_.*", "\\1", .),.)))
  
  
 
}
