library(rgee)
ee_Initialize(quiet = T)
ee_Initialize(user = 'ndef', drive = TRUE )
# ee_install_upgrade()
tiles <- sf::read_sf("data-raw/tiles/bigTiles.gpkg")
AGB_2018 <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI_BIOMASS_L4_AGB_100m_2018_v3_europe.tif")
AGB_2018_e <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI_BIOMASS_L4_AGB_100m_2018_v3_europe_stdError.tif")

## features for prediction
 features <- list(
   forestCover = "UMD/hansen/global_forest_change_2021_v1_9",
   LULC = "COPERNICUS/Landcover/100m/Proba-V-C3/Global/2019",
   ALOSyearlyMosaic = "JAXA/ALOS/PALSAR/YEARLY/SAR/2020",
   canopy_height  = "users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1",
   # canopy_height_e  = "users/nlang/ETH_GlobalCanopyHeightSD_2020_10m_v1",
   # ndvi = "VITO/PROBAV/C1/S1_TOC_100M",
   srtm = "USGS/GMTED2010",
   LULC_WC_10m  = "ESA/WorldCover/v100/2020",
   bioclim  = "WORLDCLIM/V1/BIO"
 )

 features.crs <- list()
 for(featureName in names(features) ){
   message(featureName)
   ff<-features[[featureName]]
   feature <- ee$Image(ff)
   clipped <- feature$clip( bb );
   features.crs[[featureName]] <- feature$projection()$crs()$getInfo()
 }

for(tilen in 1:nrow(tiles)){
  tt<-  tiles[tilen,]
  tile <- sf_as_ee(tt)
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
