library(ggplot2)
library(sf)
library(terra)
library(foreach)
library(doParallel)
## data processing ----

source("data-raw/processing_00_constants.R")

da <- read.csv("canopy_base_height.csv", sep="\t")
da2 <- da[order(da$Species),]
tiles <- sf::read_sf("output/allEU/biomassMapTiles_3035/allFilledTiles4326.gpkg" )
canopyHeight <- terra::rast("data-raw/tileData/forestCanopyHeight.vrt")
canopyCover <- terra::rast("data-raw/tileData/forestCover.vrt")
vegType <- terra::rast("data-raw/tileData/vegType.vrt")


st_crs(tiles)<-3035


cs <- makeCluster(10)
registerDoParallel(cs)

nr<-nrow(tiles)

res.cbh <- foreach(i = 1:nr ) %do% {

   tile <- tiles$code[[i]]
   if(i%%100==0){
      message(i)
   }
   # if(file.exists(sprintf("output/allEU/canopyBaseHeight_3035/cbh_%s.tif", tile))){
   #    return("exists")
   # }

   input.raster <- terra::rast( sprintf("output/allEU/biomassMapTiles_3035/bm_%s.tif", tile) )

   input.raster.out <- terra::rast(  input.raster  )
   vegType.t <- terra::project( vegType,  input.raster )
   vegType.tt <- vegType.t[[-1]]
   vegType.tt2 <- round(vegType.tt)

   canopyHeight.t <- terra::project( canopyHeight,  input.raster )

   dt<-vegType.tt2[]
   biomass.values <- input.raster[]
   cbh.values <- canopyHeight.t[]
   dt[dt==0]<-NA
   rs <- rowSums(dt, na.rm=TRUE)
   rs[rs==0]<-NA

   cbh.values.2 <- matrix(rep(cbh.values[,1], nrow(da2)), ncol=nrow(da2) )
   nrow(cbh.values.2)
   dt2 <- (dt/rs)
   keeplessthan2m <- cbh.values[,1]<1.3

   dt3 <- (( da2$Intercept  + t(cbh.values.2)*da2$Slope)  )
   dt3[dt3<0] <- 0
   # head(t(dt3))
   # t(dt2)

   dt4 <- rowSums(t(dt3)*dt2, na.rm=TRUE)
   dt4[is.na(biomass.values[,1])] <- NaN
   dt4[keeplessthan2m] <- NaN

   input.raster.out[] <-  dt4

   aa <- terra::writeRaster(input.raster.out, sprintf("output/allEU/canopyBaseHeight_3035/cbh_%s.tif", tile), overwrite=TRUE )
   # plot(aa)
   return(aa)

}

stopCluster(cs)
