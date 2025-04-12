library(rgee)
library(rgeeExtra)
library("inborutils")
library("parallel")
library("terra")

ee_Initialize(quiet = T)
ee_Initialize(user = 'cirgeo' )


proj3035_30m = ee$Projection('EPSG:3035')$atScale(30);

dir.exists(outputdir) dir.create(outputdir)

library(stars)

# 1. Read GeoTIFF file and create a output filename
tif <- "/archivio/shared/geodati/raster/DEMs/demTinitaly.tif"
x <- read_stars(tif)
assetId <- sprintf("%s/%s",ee_get_assethome(),'tinitaly')

# 2. From local to gcs
gs_uri <- local_to_gcs(
   x = tif,
   bucket = 'rgee_dev' # Insert your own bucket here!
)

# 3. Create an Image Manifest
manifest <- ee_utils_create_manifest_image(gs_uri, assetId)

# 4. From GCS to Earth Engine
gcs_to_ee_image(
   manifest = manifest,
   overwrite = TRUE
)

# ee_install_upgrade()
tiles <- sf::read_sf("data-raw/tiles/bigTiles.gpkg")
AGB_2018 <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI-BIOMASS_L4_AGB_2018_v3_int16_europe.tif")
AGB_2018_e <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI_BIOMASS_L4_AGB_StdDev_2018_v3_int16_europe.tif")

source("processing_01_rgeeExtraFunctions.R")
template <- "users/cirgeo/FIRE-RES/template"


## features for prediction ----
features <- list(
   forestCover = "UMD/hansen/global_forest_change_2021_v1_9",
   LULC = "COPERNICUS/Landcover/100m/Proba-V-C3/Global/2019",
   ALOSyearlyMosaic = "JAXA/ALOS/PALSAR/YEARLY/SAR/2020",
   S1collection = "COPERNICUS/S1_GRD",
   canopy_height  = "users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1",
   sd_canopy_height  = "users/nlang/ETH_GlobalCanopyHeightSD_2020_10m_v1",
   # ndvi = "COPERNICUS/S2_SR_HARMONIZED",
   srtm = "USGS/GMTED2010",
   alosDSM ="JAXA/ALOS/AW3D30/V3_2",
   LULC_WC_10m  = "ESA/WorldCover/v100/2020",
   bioclim  = "WORLDCLIM/V1/BIO",
   lcv_landcover= "users/cirgeo/FIRE-RES/open/lcv_landcover_2020_v02",
   vegetationSpecies = list(
      abies_alba= "users/cirgeo/FIRE-RES/open/veg_abies_alba_anv_v3",
      castanea_sativa= "users/cirgeo/FIRE-RES/open/veg_castanea_sativa_anv_v3",
      corylus_avellana= "users/cirgeo/FIRE-RES/open/veg_corylus_avellana_anv_v3",
      fagus_sylvatica= "users/cirgeo/FIRE-RES/open/veg_fagus_sylvatica_anv_v3",
      olea_europaea= "users/cirgeo/FIRE-RES/open/veg_olea_europaea_anv_v3",
      picea_abies= "users/cirgeo/FIRE-RES/open/veg_picea_abies_anv_v3",
      pinus_halepensis= "users/cirgeo/FIRE-RES/open/veg_pinus_halepensis_anv_v3",
      pinus_nigra= "users/cirgeo/FIRE-RES/open/veg_pinus_nigra_anv_v3",
      pinus_pinea= "users/cirgeo/FIRE-RES/open/veg_pinus_pinea_anv_v3",
      pinus_sylvestris= "users/cirgeo/FIRE-RES/open/veg_pinus_sylvestris_anv_v3",
      prunus_avium= "users/cirgeo/FIRE-RES/open/veg_prunus_avium_anv_v3",
      quercus_cerris= "users/cirgeo/FIRE-RES/open/veg_quercus_cerris_anv_v3",
      quercus_ilex= "users/cirgeo/FIRE-RES/open/veg_quercus_ilex_anv_v3",
      quercus_robur= "users/cirgeo/FIRE-RES/open/veg_quercus_robur_anv_v3",
      quercus_suber= "users/cirgeo/FIRE-RES/open/veg_quercus_suber_anv_v3",
      salix_caprea = "users/cirgeo/FIRE-RES/open/veg_salix_caprea_anv_v3"
   )
)


##cut correctly without shifting the grid

ceda_biomass_eu <- ee$Image("users/cirgeo/FIRE-RES/ESACCI-BIOMASS_L4_AGB_2018_v3_int16_europe")
worldCover_forest <- ee$Image(features$LULC_WC_10m)
createTrainingPoints <- function(in.tile){

   tile <- sf_as_ee(in.tile)
   bb<-tile$geometry()$bounds()

   lulc_mask <-  worldCover_forest$lt(50)$Or(worldCover_forest$gt(80))
   sample1   = worldCover_forest$select('Map')$toInt()$mask(lulc_mask)$clip(bb)$stratifiedSample( numPoints=as.integer(3000) ,
                                                                                        geometries=TRUE   )
   sample2 = ceda_biomass_eu$divide(10)$toInt()$mask(lulc_mask)$clip(bb)$stratifiedSample(numPoints=as.integer(6000), geometries=TRUE)
   sample = sample1$merge( sample2 )

   task_img_container[[in.tile$ID]][["samples"]] = ee_table_to_drive(
      folder = "FIRE-RES-DATA",
      description = paste("samples", in.tile$ID, sep ="_"),
      collection = sample,
      fileFormat = "CSV",
      fileNamePrefix = paste("samples", in.tile$ID, sep ="_")
   )

task_img_container[[in.tile$ID]][["samples"]]$start()
}
## template CEDA biomass map ----
templateCeda <- ee$Image(template)
worldCover_forest <- ee$Image(features$LULC_WC_10m)
OpenDataHubLUCAS <- ee$Image(features$lcv_landcover)
s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
alos <- ee$Image(features$ALOSyearlyMosaic)
s1 <-  ee$ImageCollection(features$S1collection)$filterDate('2020-06-01', '2020-09-01')

task_img_container<- list()





getRADAR <- function(in.tile){

   tile <- sf_as_ee(in.tile)
   bb<-tile$geometry()$bounds()
   #$clip(bb)

   redu <- ee$Reducer$mean()
   redum <- ee$Reducer$median()

   ## convert to sigma naught
   alos2 <- alos$updateMask(alos$gt(0))$log10()$multiply(1000)$reduceResolution(redu)$reproject( templateCeda$projection() )$select(c('HH', 'HV' ))$rename(c('ALOS_HH', 'ALOS_HV' ) )
   s1_t2 <- s1$filterBounds(bb)$filter(ee$Filter$eq('instrumentMode', 'IW'))$filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VV'))$filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VH'));
   projname <- s1_t2$first()$select('VH')$projection()
   # image <- s1_t2$first()
   s1_t <-  s1_t2$map( terrainCorrection )$reduce(redum)$multiply(100)$setDefaultProjection(projname,NULL, 20)$reduceResolution(redu, TRUE)$reproject( templateCeda$projection() )
# projname$getInfo()

# dd$getInfo()
   radar <- alos2$addBands(s1_t)
   task_img_container[[in.tile$ID]] <<- list(

      RADAR = ee_image_to_drive(
         folder = "FIRE-RES-DATA",
         description = paste("radar", in.tile$ID, sep ="_"),
         image = radar$toInt16(),
         fileFormat = "GEO_TIFF",
         region = bb,
         fileNamePrefix = paste("radar", in.tile$ID, sep ="_"),
         maxPixels = as.integer(300000000)
      )
   )

   task_img_container[[in.tile$ID]][["RADAR"]]$start()

}


getLandCoverMask <- function(in.tile){

   tile <- sf_as_ee(in.tile)
   bb<-tile$geometry()$bounds()
#$clip(bb)

   worldcover_mask = worldCover_forest$neq(0)$multiply(worldCover_forest$lt(50))

   red1 <- ee$Reducer$fixedHistogram(10,50,4)
   red2 <- ee$Reducer$mode(NULL,NULL,12)

   worldCover_forest_tmp <- worldCover_forest$mask(worldcover_mask)$reduceResolution(
       reducer= red1,
       maxPixels= 128)$reproject( templateCeda$projection() )

   # OpenDataHubLUCAS$projection()$getInfo()
   OpenDataHubLUCAS_tmp <- templateCeda$gt(0)$multiply(
      OpenDataHubLUCAS$clip(bb)  )$reproject( templateCeda$projection() )

   message("Non si allinea")
   next
   OpenDataHubLUCAS_tmp = OpenDataHubLUCAS$reproject( templateCeda$projection() )$multiply( ceda_biomass_eu$clip(bb) )
   # $reduceResolution(
   #    reducer= red2,
   #    maxPixels= 12)

   nn  <- ee$List(worldCover_forest_tmp$toDictionary()$get('Map_class_names'));
   nns<-nn$slice(0,4)$getInfo()
   nns[[1]]<-"Tree"
   worldCover_forest_100m <- worldCover_forest_tmp$arrayFlatten( c(ee$List(nns), ee$List(c('class', 'f') ) ) )$select(c('Tree_f','Shrubland_f','Grassland_f', 'Cropland_f'))$multiply(100);


   task_img_container[[in.tile$ID]] <<- list(
      # LULC_WC_10m = ee_image_to_drive(
      #    folder = "FIRE-RES-DATA",
      #    description = paste("LULC_WC_10m", in.tile$ID, sep ="_"),
      #    image = worldCover_forest_100m$toInt16(),
      #    fileFormat = "GEO_TIFF",
      #    region = bb,
      #    fileNamePrefix = paste("LULC_WC_10m", in.tile$ID, sep ="_"),
      #    maxPixels = as.integer(300000000)
      # ),
      lcv_landcover = ee_image_to_drive(
         folder = "FIRE-RES-DATA",
         description = paste("lcv_landcover", in.tile$ID, sep ="_"),
         image = OpenDataHubLUCAS_tmp$toByte(),
         fileFormat = "GEO_TIFF",
         region = bb,
         fileNamePrefix = paste("lcv_landcover", in.tile$ID, sep ="_"),
         maxPixels = as.integer(300000000)
      )
   )

   # task_img_container[[in.tile$ID]][["LULC_WC_10m"]]$start()
   task_img_container[[in.tile$ID]][["lcv_landcover"]]$start()

}



eraClimate <- ee$Image("ECMWF/ERA5/MONTHLY")
getERAclimate <- function(in.tile){

   tile <- sf_as_ee(in.tile)
   bb<-tile$geometry()$bounds()


   task_img_container[[in.tile$ID]][[ "ERAclimate" ]] <<- ee_image_to_drive(
      folder = "FIRE-RES-DATA",
      description = paste("ERAclimate", in.tile$ID, sep ="_"),
      image = eraClimate$toByte(),
      fileFormat = "GEO_TIFF",
      region = bb,
      fileNamePrefix = paste("ERAclimate", in.tile$ID, sep ="_"),
      maxPixels = as.integer(300000000)
   )

   task_img_container[[in.tile$ID]][[ "ERAclimate" ]]$start()
}

bioclim <- ee$Image(features$bioclim)
srtm <- ee$Image(features$srtm)
forestCover <- ee$Image(features$forestCover)
canopyHeight <- ee$Image(features$canopy_height)
getFeatures <- function(in.tile){

   worldcover_mask <-  worldCover_forest$lt(50)$Or(worldCover_forest$gt(80))
   tile <- sf_as_ee(in.tile)
   bb<-tile$geometry()$bounds()

   # forestCanopyHeight = canopyHeight$reduceResolution(
   #    reducer=  ee$Reducer$mean(),
   #    maxPixels= 256)$reproject( templateCeda$projection() )
   #
   # lossyear <- forestCover$select("lossyear")$reduceResolution(
   #    reducer=  ee$Reducer$max() )$reproject( templateCeda$projection() )
   #
   fc <- forestCover$select("treecover2000")$reduceResolution(
                                           reducer=  ee$Reducer$mean(),
                                           maxPixels= 20)$reproject( templateCeda$projection() )

   task_img_container[[in.tile$ID]] <<- list(
      # bioclim = ee_image_to_drive(
      #    folder = "FIRE-RES-DATA",
      #    description = paste("bioclim", in.tile$ID, sep ="_"),
      #    image = bioclim$toInt16(),
      #    fileFormat = "GEO_TIFF",
      #    region = bb,
      #    fileNamePrefix = paste("bioclim", in.tile$ID, sep ="_"),
      #    maxPixels = as.integer(300000000)
      # )  ,
      forestCover = ee_image_to_drive(
         folder = "FIRE-RES-DATA",
         description = paste("forestCover", in.tile$ID, sep ="_"),
         image = fc$toByte(),
         fileFormat = "GEO_TIFF",
         region = bb,
         fileNamePrefix = paste("forestCover", in.tile$ID, sep ="_"),
         maxPixels = as.integer(300000000)
      )
      # lossyear = ee_image_to_drive(
      #    folder = "FIRE-RES-DATA",
      #    description = paste("lossyear", in.tile$ID, sep ="_"),
      #    image = lossyear$toByte(),
      #    fileFormat = "GEO_TIFF",
      #    region = bb,
      #    fileNamePrefix = paste("lossyear", in.tile$ID, sep ="_"),
      #    maxPixels = as.integer(300000000)
      # ),
      # forestCanopyHeight = ee_image_to_drive(
      #    folder = "FIRE-RES-DATA",
      #    description = paste("forestCanopyHeight", in.tile$ID, sep ="_"),
      #    image = forestCanopyHeight$toByte(),
      #    fileFormat = "GEO_TIFF",
      #    region = bb,
      #    fileNamePrefix = paste("forestCanopyHeight", in.tile$ID, sep ="_"),
      #    maxPixels = as.integer(300000000)
      # ),
      # elevation = ee_image_to_drive(
      #    folder = "FIRE-RES-DATA",
      #    description = paste("ALOSelevation", in.tile$ID, sep ="_"),
      #    image = ee$ImageCollection("JAXA/ALOS/AW3D30/V3_2")$select('DSM')$filterBounds(bb)$mosaic()$reproject( templateCeda$projection() )$toInt16()$rename("ALOSelevation"),
      #    fileFormat = "GEO_TIFF",
      #    region = bb,
      #
      #    fileNamePrefix = paste("ALOSelevation", in.tile$ID, sep ="_"),
      #    maxPixels = as.integer(300000000)
      # )
      # LCmask = ee_image_to_drive(
      #    folder = "FIRE-RES-DATA",
      #    description = paste("LCmask", in.tile$ID, sep ="_"),
      #    image = worldcover_mask$reproject( templateCeda$projection() )$toByte(),
      #    fileFormat = "GEO_TIFF",
      #    region = bb,
      #
      #    fileNamePrefix = paste("LCmask", in.tile$ID, sep ="_"),
      #    maxPixels = as.integer(300000000)
      # )
   )

   task_img_container[[in.tile$ID]][["forestCover"]]$start()
   # task_img_container[[in.tile$ID]][["forestCanopyHeight"]]$start()
   # ID2 <- in.tile$ID
   # # for(ID2 in names(task_img_container ) ){
   # for(nnn in names(task_img_container[[ID2]]) ){
   #    message("Checking ", nnn)
   #    task.status <- task_img_container[[ID2]][[nnn]]$status()
   #    # message("STATUS ", task.status$state )
   #    # next
   #    if( task.status$state!="RUNNING" && task.status$state!="COMPLETED" ) {
   #       task_img_container[[ID2]][[nnn]]$start()
   #    }
   #  }
 # }


}



getVegetationType <- function(in.tile){

   # worldcover_mask <-  worldCover_forest$lt(50)$Or(worldCover_forest$gt(80))
   tile <- sf_as_ee(in.tile)
   bb<-tile$geometry()$bounds()

   eeimages <- c()
   eeimagesTot <- c()
   for(specie in names(features$vegetationSpecies)){
      classID <- length(eeimages)+1
      tmpimage <- ee$Image(features$vegetationSpecies[[specie]])
      eeimagesTot <- c(eeimagesTot, tmpimage )

      tmpimage2 <-tmpimage$addBands(ee$Image$constant(ee$Number(classID))$rename('VegClass')$byte())$reproject( tmpimage$projection() )
      eeimages <- c(eeimages, tmpimage2 )
   }

   eeimageCollection <- ee$ImageCollection(eeimages)
   array = eeimageCollection$toArray();
   axes = list( image=0, band=1 )
   sort = array$arraySlice(axes$band, 0, 1);  # select bands from index 0 to 1 (NDVI)
   sorted = array$arraySort(sort);
   lengthArr = sorted$arrayLength(axes$image)
   valuesMax = sorted$arraySlice(axes$image, lengthArr$subtract(1), lengthArr)

   vmax = valuesMax$arrayProject(list(axes$band) )$arrayFlatten( list( c('b1', 'VegClass') ) )$reproject( templateCeda$projection() )
   vmax$getInfo()
   eeimageFinal <- ee$ImageCollection(c(vmax$select("VegClass"), eeimagesTot) )$toBands()$rename( c("VegHighestProb", names(features$vegetationSpecies) ) )$reproject( templateCeda$projection() )

   task_img_container[[in.tile$ID]][[ "vegType" ]] <<- ee_image_to_drive(
      folder = "FIRE-RES-DATA",
      description = paste("vegType", in.tile$ID, sep ="_"),
      image = eeimageFinal$toByte(),
      fileFormat = "GEO_TIFF",
      region = bb,
      fileNamePrefix = paste("vegType", in.tile$ID, sep ="_"),
      maxPixels = as.integer(300000000)
   )

   task_img_container[[in.tile$ID]][[ "vegType" ]]$start()


}

landsat_evi <- ee$ImageCollection("LANDSAT/LC08/C01/T1_8DAY_EVI")
landsat_ndvi <- ee$ImageCollection("LANDSAT/LC08/C01/T1_8DAY_NDVI")
getVegIndexes <- function(in.tile){

   worldcover_mask <-  worldCover_forest$lt(50)$Or(worldCover_forest$gt(80))
   tile <- sf_as_ee(in.tile)
   bb<-tile$geometry()$bounds()
   # bb<-polygon$bounds()
   maskAndClipAll <- function(image) {
      image = image$clip(bb)
      cp = image$select('MSK_CLDPRB')
      sp = image$select('MSK_SNWPRB')
      mask = cp$eq(0)$multiply(sp$eq(0))$multiply(worldcover_mask);
      image$updateMask(mask);
   }

   landsat_ndvit <- landsat_ndvi$filterBounds(bb)$filterDate('2020-01-01', '2022-10-10')$reduce(ee$Reducer$percentile(list(95) ) )$updateMask(worldcover_mask)$multiply(10000)$reproject( templateCeda$projection() )
    landsat_evit <-  landsat_evi$filterBounds(bb)$filterDate('2020-01-01', '2022-10-10')$reduce(ee$Reducer$percentile( list(95) ) )$updateMask(worldcover_mask)$multiply(10000)$reproject( templateCeda$projection() )

   # s2_tmp <- s2$filterBounds(bb)$filterDate('2020-01-01', '2022-10-10')$filter( ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE',20));
   # s2_tmp <- s2_tmp$map(maskAndClipAll)
   # s2_tmp_prj <- s2_tmp$first()$select('B8')$projection();
   #
   # ndvi <- s2_tmp$map( function(image){
   #       image$normalizedDifference( c('B8', 'B4'))$rename('NDVI')$multiply(1000)
   #    })$reduce( ee$Reducer$max() )$reproject(  s2_tmp_prj   )$reduceResolution(
   #                                      reducer=  ee$Reducer$mean(),
   #                                      maxPixels= 90)$reproject( templateCeda$projection() )
   # evi <- s2_tmp$map( function(image){
   #    eviband =  image$expression(  '2.5 * ((NIR-RED) / ((NIR + (6 * RED) - (7.5* BLUE) ) +1))',
   #                                  list(NIR=image$select("B8"),
   #                                       RED=image$select("B4"),
   #                                       BLUE=image$select("B1") ) )$rename('EVI')
   #
   #    eviband$updateMask(eviband$gt(-5)$And(eviband$lt(10)) )$multiply(1000)
   #
   #
   # })$reduce( ee$Reducer$max() )$reproject(  s2_tmp_prj   )$reduceResolution(
   #    reducer=  ee$Reducer$mean(),
   #    maxPixels= 90)$reproject( templateCeda$projection() )


   task_img_container[[in.tile$ID]] <<- list(
      evi95perc = ee_image_to_drive(
         folder = "FIRE-RES-DATA",
         description = paste("EVI", in.tile$ID, sep ="_"),
         image = landsat_evit$toInt16(),
         fileFormat = "GEO_TIFF",
         region = bb,
         fileNamePrefix = paste("EVI", in.tile$ID, sep ="_"),
         maxPixels = as.integer(300000000)
      ) ,
      ndvi95perc = ee_image_to_drive(
         folder = "FIRE-RES-DATA",
         description = paste("NDVI", in.tile$ID, sep ="_"),
         image = landsat_ndvit$toInt16(),
         fileFormat = "GEO_TIFF",
         region = bb,
         fileNamePrefix = paste("NDVI", in.tile$ID, sep ="_"),
         maxPixels = as.integer(300000000)
      )
   )

   # task_img_container[[in.tile$ID]][["LULC_WC_10m"]]$start()
   task_img_container[[in.tile$ID]][["ndvi95perc"]]$start()
   task_img_container[[in.tile$ID]][["evi95perc"]]$start()

}

## download from google drive ----

ttnames<-c()
for(file in files$name){

   message("=====", file)
   nm<-strsplit(file, "_")[[1]]
   fname<-NULL

   fname <- file.path(getwd(), "data-raw/tileData", gsub("radar", "",nm[[1]]) , paste("radar", ".tif") )

   if(nm[[1]]=="NDVI") {
      fname <- file.path(getwd(), "output", "featureRasters", nm[[2]], "NDVI_max" )
   }
   if(nm[[1]]=="EVI") {
      fname <- file.path(getwd(), "output", "featureRasters", nm[[2]], "EVI_mean" )
   }
   # raster::extension(fname)<-"tif"

   message( fname)
   ttnames<-c(ttnames, fname)
   if(!file.exists(fname)){
       googledrive::drive_download(file, path = fname)
   }
   #
}

vrt<-terra::vrt(ttnames,filename="/archivio/R/shared/FIRE-RES/data-raw/tileData/RADAR.vrt", overwrite=TRUE)

## functions for foliage fraction ----
 fff <- list(
    piceaAbies = function(total.biomass, height){
       if(height < 5){
          return(total.biomass*0.06)
       }
       if(height > 10){
          return(total.biomass*0.03)
       }
       return(total.biomass*0.04)
    },
    abiesAlba = function(total.biomass, height){
       if(height < 5){
          return(total.biomass*0.06)
       }
       else if(height >= 5 && height <= 10 ){
          return(total.biomass*0.04)
       }
       else if(height > 10){
          return(total.biomass*0.03)
       }


       return(total.biomass*0.04)
    }
 )


## Check Tasks and download -----
checkTasks <- function(taskcontainer){

   task_img_container <- taskcontainer

   for(i in names(task_img_container) ){

      for(i2 in names(task_img_container[[i]]) ) {

         tt<-task_img_container[[i]][[i2]]$status()


         fname <- file.path(getwd(), "output", "featureRasters", i, i2 )

         dirname <- dirname(fname)
         raster::extension(fname)<-"tif"
         if(!dir.exists(dirname)){
            message("Creating directory ", dirname)
            dir.create(dirname, recursive=TRUE)
         }


         if(!file.exists(fname)){

            if(tt$state!="COMPLETED") {
               message(".....Not completed: ", i, " -- ", i2, "... skipping")
               next
            } else {
               if(tt$state=="COMPLETED"){
                  message("....COMPLETED: ", i, " -- ", i2, " ... DOWNLOADING")
                  img <- ee_drive_to_local(task = task_img_container[[i]][[i2]],
                                           dsn= fname  )
               } else {
                  message(tt$state, " state!!!---: ", i, " -- ", i2, "... skipping")
               }
            }

         }
         else {
            message("File ",  fname, " already downloaded... skipping ")
         }
      }
   }
}


## Collect CRS information ----
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

## RUN FUNCTIONS ----
tilesToGetTrainingPoints<-c("B0",  "F1", "E4")
 bb<- ee$Geometry$Rectangle( c(9.0, 45.84,
                                9.1,  45.94  ) )$bounds()

 for(tilen in 1:nrow(tiles)){


   tt<-  tiles[tilen,]
   if(is.element(tt$ID, c("E0", "B0", "B1")) ){
      getFeatures(tt)
   }

   # if(is.element(tt$ID, tilesToGetTrainingPoints)){
     # getVegIndexes(tt)
       # getFeatures(tt)
     # getVegetationType(tt)
     # createTrainingPoints(tt)
    # getRADAR(tt)
     print(tt$ID)
   # }

}

aa<- list.files("data-raw/tileData/A1/", pattern = "\\.tif", full.names = T, recursive = T)
wd <- getwd()
setwd("data-raw/tileData")
for(i in aa){
   basename<-basename(i)
   bb<- list.files(
                   pattern = paste0(basename, "$"),
                   full.names = F, recursive = T)
   raster::extension(basename)<-"vrt"
   ovr.filename=paste0(basename, ".ovr")
   # if(file.exists(filename)){
   #    next
   # }
   terra::vrt(bb, filename=basename, overwrite=TRUE)
   tx  <- readLines(basename)
   tx2  <- gsub(pattern = 'SourceFilename relativeToVRT="0"',
                replace = 'SourceFilename relativeToVRT="1"', x = tx)
   writeLines(tx2, con=basename)
   if(!file.exists(ovr.filename)) gdalUtils::gdaladdo(ovr.filename, levels = c(2,4,8,16,32,64,128,256), r="nearest", ro=TRUE,verbose=TRUE)
}

setwd(wd)
  Map(function(fn){
     file.remove(fn)
     #file.rename(fn, "landcover_hcl.tif")
     },
 list.files("data-raw/tileData", pattern = "landcover_hcl.tif", full.names = T, recursive = T)
 )


