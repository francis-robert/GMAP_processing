raster_comb <- function(rast_path, campaign=""){
  raster_list <- list.files(path=rast_path, pattern = c(".tif|.TIF"),full.names = T)
  raster_raw <-lapply(raster_list,rast)
  raster <- do.call(mosaic,raster_raw)
  print("Mosaiced Raster has been generated! Change rast_path
        to utilize the novel generated raster.")
  writeRaster(raster_raw,paste0(campaign,"_interim_raster_mosaic.tif"))
}
