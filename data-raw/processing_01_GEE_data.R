library(rgee)
library("inborutils")
library("parallel")
library("terra")
ee_Initialize(quiet = T)
ee_Initialize(user = 'ndef', drive = TRUE )
# ee_install_upgrade()
tiles <- sf::read_sf("data-raw/tiles/bigTiles.gpkg")
AGB_2018 <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI_BIOMASS_L4_AGB_100m_2018_v3_europe.tif")
AGB_2018_e <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI_BIOMASS_L4_AGB_100m_2018_v3_europe_stdError.tif")
template <- "users/cirgeo/FIRE-RES/template"
AGB_2018 <- terra::rast("data-raw/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0.nc")
##cut correctly without shifting the grid



doTrainingPoints <- function(){

}

templateCeda <- ee$Image(template)

getLandCoverMask <- function(in.tile){

   tile <- sf_as_ee(in.tile)
   bb<-tile$geometry()$bounds()

   worldCover_forest <- ee$Image(features$LULC_WC_10m)#$clip(bb)

   worldcover_mask = worldCover_forest$neq(0)$multiply(worldCover_forest$lt(50))
   red <- ee$Reducer$fixedHistogram(10,50,4)
   worldCover_forest_tmp <- worldCover_forest$mask(worldcover_mask)$reduceResolution(
       reducer= red,
       maxPixels= 128)$reproject( templateCeda$projection() )


   nn  <- ee$List(worldCover_forest_tmp$toDictionary()$get('Map_class_names'));
   nns<-nn$slice(0,4)$getInfo()
   nns[[1]]<-"Tree"
   worldCover_forest_100m <- worldCover_forest_tmp$arrayFlatten( c(ee$List(nns), ee$List(c('class', 'f') ) ) )$select(c('Tree_f','Shrubland_f','Grassland_f', 'Cropland_f'))$multiply(100);


   task_img <- ee_image_to_drive(
      folder = "FIRE-RES-DATA",
      description = paste0("world", in.tile$ID, collapse ="_"),
      image = worldCover_forest_100m$toInt16(),
      fileFormat = "GEO_TIFF",
      region = bb,
      fileNamePrefix = paste0("worldLandCoverFraction", in.tile$ID, collapse ="_"),
      maxPixels = as.integer(300000000)
   )
   task_img$start()

}

## features for prediction
 features <- list(
   forestCover = "UMD/hansen/global_forest_change_2021_v1_9",
   LULC = "COPERNICUS/Landcover/100m/Proba-V-C3/Global/2019",
   ALOSyearlyMosaic = "JAXA/ALOS/PALSAR/YEARLY/SAR/2020",
   canopy_height  = "users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1",
   sd_canopy_height  = "users/nlang/ETH_GlobalCanopyHeightSD_2020_10m_v1",
   # ndvi = "VITO/PROBAV/C1/S1_TOC_100M",
   srtm = "USGS/GMTED2010",
   LULC_WC_10m  = "ESA/WorldCover/v100/2020",
   bioclim  = "WORLDCLIM/V1/BIO",
   lcv_landcover_hcl_v1= "users/cirgeo/FIRE-RES/open/lcv_landcover_hcl_v1",
   veg_abies_alba_anv_v3= "users/cirgeo/FIRE-RES/open/veg_abies_alba_anv_v3",
   veg_castanea_sativa_anv_v3= "users/cirgeo/FIRE-RES/open/veg_castanea_sativa_anv_v3",
   veg_corylus_avellana_anv_v3= "users/cirgeo/FIRE-RES/open/veg_corylus_avellana_anv_v3",
   veg_fagus_sylvatica_anv_v3= "users/cirgeo/FIRE-RES/open/veg_fagus_sylvatica_anv_v3",
   veg_olea_europaea_anv_v3= "users/cirgeo/FIRE-RES/open/veg_olea_europaea_anv_v3",
   veg_picea_abies_anv_v3= "users/cirgeo/FIRE-RES/open/veg_picea_abies_anv_v3",
   veg_pinus_halepensis_anv_v3= "users/cirgeo/FIRE-RES/open/veg_pinus_halepensis_anv_v3",
   veg_pinus_nigra_anv_v3= "users/cirgeo/FIRE-RES/open/veg_pinus_nigra_anv_v3",
   veg_pinus_pinea_anv_v3= "users/cirgeo/FIRE-RES/open/veg_pinus_pinea_anv_v3",
   veg_pinus_sylvestris_anv_v3= "users/cirgeo/FIRE-RES/open/veg_pinus_sylvestris_anv_v3",
   veg_prunus_avium_anv_v3= "users/cirgeo/FIRE-RES/open/veg_prunus_avium_anv_v3",
   veg_quercus_cerris_anv_v3= "users/cirgeo/FIRE-RES/open/veg_quercus_cerris_anv_v3",
   veg_quercus_ilex_anv_v3= "users/cirgeo/FIRE-RES/open/veg_quercus_ilex_anv_v3",
   veg_quercus_robur_anv_v3= "users/cirgeo/FIRE-RES/open/veg_quercus_robur_anv_v3",
   veg_quercus_suber_anv_v3= "users/cirgeo/FIRE-RES/open/veg_quercus_suber_anv_v3",
   veg_salix_caprea_anv_v3 = "users/cirgeo/FIRE-RES/open/veg_salix_caprea_anv_v3"
 )

 ## Collect CRS information
 features.crs <- list()

 ff<-features[[featureName]]
 feature <- ee$Image(ff)
 for(featureName in names(features) ){
   message(featureName)
   ff<-features[[featureName]]
   feature <- ee$Image(ff)
   # clipped <- feature$clip( bb );
   fi <- feature$getInfo()
   crs <- fi$bands[[1]]$crs
   if(nchar( crs)>300){
     crs<-"EPSG:3035"
   }
   features.crs[[featureName]] <-crs
 }


tilesToGetTrainingPoints<-c("B0", "E0", "F1")
for(tilen in 1:nrow(tiles)){
  tt<-  tiles[tilen,]
  print(tt$ID)
   if( is.element(tt$ID, tilesToGetTrainingPoints) ){
      print("do traing")
   }
  next
  in.tile<-tt
  bb<-tile$geometry()$bounds()

  for(featureName in names(features) ){
    message(featureName)
    ff<-features[[featureName]]
    feature <- ee$Image(ff)
    clipped <- feature$clip( bb );
    message(feature$projection()$crs()$getInfo())

  }


  task_img <- ee_image_to_drive(
    folder = "FIRE-RES-DATA",
    image = clipped,
    fileFormat = "GEO_TIFF",
    region = bb,
    fileNamePrefix = paste(tt$ID, sep="_", ),
    maxPixels = as.integer(300000000)
    )
  task_img$start()


}


for(tilen in 1:nrow(tiles)){




 }
