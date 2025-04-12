library(ggplot2)
library(sf)
library(terra)
library(foreach)
library(parallel)
library(doParallel)
## data processing ----

source("data-raw/processing_00_constants.R")


# tiles <- sf::read_sf("data-raw/tiles/adm_tiling.system_30km_m_1m_s0..0m_2020_eumap_epsg3035_v0.1.gpkg")
#
# st_crs(tiles)<-3035
#
# AGB_2018 <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI-BIOMASS_L4_AGB_2018_v3_int16_europe.tif")
# AGB_2018_e <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI_BIOMASS_L4_AGB_StdDev_2018_v3_int16_europe.tif")
#

#
# metrics<- list()

#h2o.clusterStatus()
# grid.coords <- sf::st_coordinates( ( sf::st_centroid(tiles$geom) ) )
#
# rowl <- sort(unique(grid.coords[,1]) )
# coll <- sort(unique(grid.coords[,2]) )
#
# code <- paste( substr(grid.coords[,1], 1,3) , substr(grid.coords[,2], 1,3), sep="_")
# code2 <- unique(code)
# tiles$code <- code

biomass.rasters <- list.files("output/allEU/biomassMap_v03", pattern="\\.tif$", full.names = TRUE)
biomass.raster.final  <- terra::rast("output/allEU/biomassMap_v03.tif")

# for(i in biomass.rasters){
#   tr <- terra::rast(i)
#   terra::time(tr)<-c(rep(as.Date("2020-01-01"),2), rep(as.Date("2018-01-01"),2))
#
#   terra::writeRaster(tr,
#                      gsub("_v02","", i),
#                      datatype = "INT2S")
# }

biomass.rasters.extents <- Map(function(x){ (sf::st_as_sf(terra::as.polygons(terra::ext(terra::rast(x)), crs="EPSG:4326")) ) }, biomass.rasters)

extents.rasters <- sf::st_sf(dplyr::bind_rows(biomass.rasters.extents), crs=4326)
extents.rasters$paths <- names(biomass.rasters.extents)

# extents.rasters.30325 <- sf::st_transform( extents.rasters$geometry, crs=3035 )

tile.overlap<- ( (sf::st_intersects(extents.rasters,extents.rasters) ) )


for(nrast  in 1:length(biomass.rasters)){
  print("ddddd")
  mainrast <- terra::rast(biomass.rasters[[nrast]])

  print(nrast)
  overlaps <- setdiff(tile.overlap[[nrast]], nrast)
  # if(length(overlaps))
  xym <- terra::xyFromCell(mainrast, terra::cells(mainrast) )
  for(i in overlaps){
    slaverast <- terra::rast(biomass.rasters[[ overlaps[[i]] ]])
    xy <- terra::xyFromCell(slaverast, terra::cells(slaverast) )
    matchy <- (xy[,2] %in% xym[,2])
    matchx <- (xy[,1] %in% xym[,1])
    matchall <- which(matchx & matchy)
    matchys <- (xym[,2] %in% xy[,2])
    matchxs <- (xym[,1] %in% xy[,1])
    matchalls <- which(matchxs & matchys)
    print(length(matchall))
    slavecells <- terra::cellFromXY(slaverast, xy[matchall,])
    mastercells <- terra::cellFromXY(mainrast, xym[matchalls,])

    print("111")
    supermastercells <- terra::cellFromXY(biomass.raster.final, xym[matchalls,])
    biomass.raster.final$biomassMap_v03_1[supermastercells]
    vals.slave <- slaverast[slavecells][[1]]
    vals.master <- mainrast[mastercells][[1]]
    print("ddddd")
    browser()
    print("ddddd")
    biomass.raster.final[supermastercells] <- (vals.slave+vals.master)/2
  }
}


names(tile.overlap)<- tiles$code

# rn <- which(Map(length, sss)==0)
#
# sf::write_sf(tiles[rn,], "strange_tiles.gpkg")

cs <- makeCluster(14)
registerDoParallel(cs)
# res <-  foreach(tilen = 1:nrow(tiles) ) %do% {
tiles$filled <- 0
totJumped <- 0
for(tilen in 1:nrow(tiles) ) {


  tilet <- tiles[tilen, ]
  tile<-tilet$code

  if(file.exists(sprintf("output/allEU/biomassMapTiles_3035/bm_%s.tif", tile))){
    tiles$filled[[tilen]] <- 1
    next
  }

  message(tilen, " su ", nrow(tiles))
  # message("faccio ", tile)

  exts <- sf::st_bbox(tilet$geom)
  exts.4326 <- sf::st_transform(tilet$geom, 4326)
  exts <- exts[c(1,3,2,4) ]
  output.raster <- terra::rast(ext= exts , res=100, crs="EPSG:3035")

 rn <-  tile.overlap[[tile]]
 if(length(rn)==0){
     message("Nessun raster trovato x ", tile , " - la salto!")
   totJumped <- totJumped + 1
   next
   # return("length of overlapping is zero")
 }
 rasters2dump <- extents.rasters$paths[ rn ]

 output.raster.list<-list()
 for(rr in rasters2dump){
   tr<-terra::rast(rr)
   output.raster.list[[rr]] <- terra::resample( tr$agb_2020, output.raster)
 }
 if(length(rasters2dump) > 1) {
   final.tr <- terra::rast(output.raster.list)
   final.tr2 <-  terra::app(final.tr, function(x){mean(x, na.rm=TRUE)})
 } else {
   final.tr2 <-  output.raster.list[[rr]]
 }
  terra::writeRaster(final.tr2,
                     sprintf("output/allEU/biomassMapTiles_3035/bm_%s.tif", tile),
                     overwrite=TRUE,
                     datatype = "INT2S")

   # return(tilen)
}

tf <- tiles[tiles$filled==1,]
tf$filled<-NULL

cs <- makeCluster(14)
registerDoParallel(cs)

res <- foreach(i = 1:nrow(tf)) %dopar% {
  tile <- tf$code[[i]]
  if(file.exists(sprintf("output/allEU/biomassMapTiles_3035/bm_%s.tif", tile))){
    return("exists")
  }
  final.tr2 <- terra::rast(sprintf("output/allEU/biomassMapTilesOld_3035/bm_%s.tif", tile) )
  if(min(final.tr2[], na.rm=TRUE) < 0 ){
    final.tr3 <- terra::clamp(final.tr2, lower=0, upper=Inf, values=TRUE)
    terra::writeRaster(final.tr3,
                       sprintf("output/allEU/biomassMapTiles_3035/bm_%s.tif", tile),
                       overwrite=TRUE,
                       datatype = "INT2S")
    return("written")
  } else {
    file.copy(sprintf("output/allEU/biomassMapTilesOld_3035/bm_%s.tif", tile),
              sprintf("output/allEU/biomassMapTiles_3035/bm_%s.tif", tile))
    return("copied")
  }

}

sf::write_sf( sf::st_transform(tf, crs=4326),
                   sprintf("output/allEU/biomassMapTiles_3035/allFilledTiles4326.json"),
                   overwrite=TRUE,
              driver="GeoJSON")
