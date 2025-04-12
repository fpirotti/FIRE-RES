library(ggplot2)
library(sf)
library(terra)
library(foreach)
library(parallel)
library(doParallel)
## data processing ----

source("data-raw/processing_00_constants.R")


tiles <- sf::read_sf("output/allEU/biomassMapTiles_3035/allFilledTiles4326.gpkg" )
canopyHeight <- terra::rast("data-raw/tileData/forestCanopyHeight.vrt")
canopyCover <- terra::rast("data-raw/tileData/forestCover.vrt")
vegType <- terra::rast("data-raw/tileData/vegType.vrt")


st_crs(tiles)<-3035


cs <- makeCluster(12)
registerDoParallel(cs)

nr<-nrow(tiles)

res<- NA

res <- foreach(i = 1:nr, .packages = c("terra"), .inorder = TRUE ) %do% {

   tile <- tiles$code[[i]]
   # if(i%%100==0){
   #    message(i)
   # }
   if(file.exists(sprintf("output/allEU/biomassFoliageMapTiles2_3035/bmffr_%s.tif", tile))){
      return("exists")
   }
   input.raster <- terra::rast( sprintf("output/allEU/biomassMapTiles_3035/bm_%s.tif", tile) )

   # plot(input.raster)
   input.raster.out <- terra::rast(  input.raster  )
   vegType.t <- terra::project( vegType,  input.raster )

   vegType.tt <- vegType.t[[-1]]
   vegType.tt2 <- round(vegType.tt)
   # terra::writeRaster(vegType.tt2, sprintf("output/allEU/biomassFoliageMapTiles_3035/abmffr_%s.tif", tile), overwrite=TRUE )



   ##calculate weights for each species
   dt<-vegType.tt2[]
   biomass.values <- input.raster[]
   dt[dt==0]<-NA
   rs <- rowSums(dt, na.rm=TRUE)
   rs[rs==0]<-NA
   # dt[210,]

   # totSpeciesFraction
   # rs[[210]]
   dt2 <- t(dt/rs)

   browser()
   # dt2[,210]
   dt3 <- t( dt2*foliage.percentage*0.01  )*biomass.values[,1]

   dt4 <- rowSums(dt3, na.rm=TRUE)
   dt4[is.na(biomass.values[,1])] <- NaN

   input.raster.out[] <-  dt4
   #return(input.raster.out)
   # input.raster.out2<-terra::rast(sprintf("output/allEU/biomassFoliageMapTiles_3035/bmffr_%s.tif", tile))
   terra::writeRaster(input.raster.out, sprintf("output/allEU/biomassFoliageMapTiles2_3035/bmffr_%s.tif", tile), overwrite=TRUE )
   return("ok")

}

stopCluster(cs)
