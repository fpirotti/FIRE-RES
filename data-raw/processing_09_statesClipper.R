library(ggplot2)
library(sf)
library(terra)
library(foreach)
library(parallel)
library(doParallel)
library(sf)
library(raster)
## data processing ----

#source("data-raw/processing_00_constants.R")


states.sf <- sf::read_sf("/archivio/shared/geodati/vettoriali/confini/EU/EUstates.shp")

states <- terra::vect("/archivio/shared/geodati/vettoriali/confini/EU/EUstates.shp")

layers <- c("/archivio/shared/geodati/raster/FIRE/fireres/elevation.tif"
            ,"/archivio/shared/geodati/raster/FIRE/fireres/slope.tif"
            ,"/archivio/shared/geodati/raster/FIRE/fireres/aspect.tif"
            ,"/archivio/shared/geodati/raster/FIRE/fireres/fuelModelClasses_ScottBurgan.tif"
            ,"/archivio/shared/geodati/raster/FIRE/fireres/fuelModelClasses_Aragonese.tif"
            ,"/archivio/shared/geodati/raster/FIRE/fireres/canopyBaseHeight.tif"
            ,"/archivio/shared/geodati/raster/FIRE/fireres/canopyCover.tif"
            ,"/archivio/shared/geodati/raster/FIRE/fireres/canopyHeight.tif"
            ,"/archivio/shared/geodati/raster/FIRE/fireres/canopyBulkDensity.tif")
#treeH <- terra::rast("/archivio/shared/geodati/raster/FIRE/fireres/canopyHeight.tif")



for(i in 1:length(states) ){
  state=states[i,]
  # if(state$NAME_FIX!="Bulgaria"){
  #   next
  # }
  if( terra::expanse(state) < 5e7) {
    message(state$NAME_FIX, " troppo piccola ==============")
    next
  }
  message("Buffering ", state$NAME_FIX)


  state.b <- sf::st_transform(sf::st_as_sf(state), 3035)

  state.c <-  sf::st_buffer( state.b$geometry , 10000)
  state.d <-  sf::st_buffer(state.c, -9000)
  sf::write_sf(sf::st_transform(state.d, 4326), "tempvect.gpkg")
  state.b <- terra::vect("tempvect.gpkg")

  dirpath <- file.path("/archivio/shared/R/FIRE-RES/output/statesOut", state$NAME_FIX)
  if(!dir.exists(dirpath)){
    dir.create(dirpath)
  }
  fname <- file.path(dirpath, "fuelModelClasses_ScottBurgan.qml")
  a1<- file.copy("/archivio/shared/geodati/raster/FIRE/fireres/fuelModelClasses_ScottBurgan.qml",
            fname, overwrite = TRUE)
  fname <- file.path(dirpath, "fuelModelClasses_ScottBurgan.clr")
  a2<-file.copy("/archivio/shared/geodati/raster/FIRE/fireres/fuelModelClasses_ScottBurgan.clr",
            fname, overwrite = TRUE)
  fname <- file.path(dirpath, "fuelModelClasses_Aragonese.qml")
  a3<-file.copy("/archivio/shared/geodati/raster/FIRE/fireres/fuelModelClasses_Aragonese.qml",
            fname, overwrite = TRUE)
  fname <- file.path(dirpath, "fuelModelClasses_Aragonese.clr")
  a4<-file.copy("/archivio/shared/geodati/raster/FIRE/fireres/fuelModelClasses_Aragonese.clr",
            fname, overwrite = TRUE)

  for(l in layers){
    fname <- file.path(dirpath, basename(l))
    if(file.exists(fname)) next
    message("Doing ",  basename(l))
    ext <- terra::mask(terra::crop(terra::rast(l), state.b), state.b)
    writeRaster(ext, fname,overwrite=TRUE)
  }

}
