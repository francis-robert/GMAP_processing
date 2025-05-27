onoff <- function(x,transect = c(" ")){
  transect_vec <- unique(transect)

  on <- x %>%
    filter(id %in% transect_vec) %>%
    mutate(onoff = "on")

  off <-x %>%
    filter(!id %in% transect_vec) %>%
    mutate(onoff = "off")

  output <- on %>%
    bind_rows(off)

  return (output)
}
