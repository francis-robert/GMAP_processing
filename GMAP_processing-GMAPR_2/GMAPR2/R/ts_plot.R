ts_plot <- function (x) {
  data <- x %>%
    mutate(header = gsub("ANALYTE_","", header))
return(data)
}
