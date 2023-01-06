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
lc_esa <-  terra::rast("data-raw/tileData/LULC_WC_10m.vrt")
lc_hcl <-  terra::rast("data-raw/tileData/lcv_landcover.vrt")

st_crs(tiles)<-3035


cs <- makeCluster(14)
registerDoParallel(cs)
nrow(tiles)

vt<-terra::rast(vegType)

res <- foreach(i = 1:4) %do% {

   tile <- tiles$code[[i]]
   input.raster <- terra::rast( sprintf("output/allEU/biomassMapTiles_3035/bm_%s.tif", tile) )
   vegType.t <- terra::resample( vegType, input.raster)
   canopyHeight.t <- terra::resample( canopyHeight, input.raster)

   lc_esa.t <-  terra::resample( lc_esa, input.raster)
   lc_hcl.t <-  terra::resample( lc_hcl, input.raster)

   plot(lc_esa.t)
   plot(lc_hcl.t)

  plot(output.raster)
  if(file.exists(sprintf("output/allEU/fractionLeafBiomassMapTiles_3035/bmfr_%s.tif", tile))){
    return("exists")
  }
  final.tr2 <- terra::rast(sprintf("output/allEU/fractionLeafBiomassMapTiles_3035/bmfr_%s.tif", tile) )


}

