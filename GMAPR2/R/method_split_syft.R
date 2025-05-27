method_split_syft <-function(x,y){
  out <-c()
  if(length(unique(y$transect_procedure)>1)){
    for (i in unique(y$transect_procedure)){
      y_sub <- y %>%
        filter(transect_procedure==i)
      out_mult_non_zero<- x %>%
        filter(!value==0) %>%
        ungroup() %>%
        left_join(.,y,by="id") %>%
        filter(id %in% y_sub$id)

      out_mult_zero<- x %>%
        filter(value==0) %>%
        ungroup() %>%
        left_join(.,y,by="id") %>%
        filter(id %in% y_sub$id)

      out_mult <-out_mult_non_zero %>%
        bind_rows(out_mult_zero)
  out[[i]]<- out_mult}
  }else{
    print("single cycle time for all transects this function is not needed")}
  return(out)
}
