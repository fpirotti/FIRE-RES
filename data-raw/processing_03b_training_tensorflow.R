library(tensorflow)
library(keras)
library(tidyverse)


## data processing ----

if(!dir.exists("data-raw/tileData") ){
  stop("You are missing the folder with data - please read/run DATA.R to download missing data!")
}


tiles <- sf::read_sf("data-raw/tiles/bigTiles.gpkg")
AGB_2018 <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI-BIOMASS_L4_AGB_2018_v3_int16_europe.tif")
AGB_2018_e <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI_BIOMASS_L4_AGB_StdDev_2018_v3_int16_europe.tif")

#res <- readRDS("output/trainingTestingMatrixCoords.rds")

source("data-raw/processing_00_constants.R")


metrics<-list()
varimp <-list()

if(file.exists("output/models_v02/metrics_v02.rds")) metrics<-readRDS("output/models_v02/metrics_v02.rds")

for(tile in tiles$ID) {

  if( is.element(tile, nonfare ) ){
    message("No do for tile ",tile)
    next
  }
  if( is.element(tile, names(classification.lut) ) ){
    message("No data for this tile (",tile,"), will use model from ", classification.lut[[tile]])
    next
  }

  path = file.path("output/models_keras",
                   sprintf("%s_biomass_prediction_model_tile", tile) )

  if(file.exists(path)){
    message("Model exists, skipping ", tile)
    # next
  }

  message("doing ", tile)

  features <- list.files(paste0("data-raw/tileData/", tile), pattern = "\\.tif$", full.names = TRUE )
  if(length(features)!=11){
    message("problema tile ", tile)
    next
  }

  firstext <- terra::ext(terra::rast(features[[1]]))
  for(ntmppp in features) {
    te<- terra::ext(terra::rast(ntmppp))
    if(te!=firstext && basename(ntmppp)!="bioclim.tif"){
      message(tile, " not equal ", ntmppp)
    }
  }

  ly2 <- grep("landcover", features)
  res.current <- res[[tile]]
  vv.tmp2 <-  terra::extract(terra::rast(features[[ly2]]), res.current)
  res.final1 <- res.current[ which(!is.na(vv.tmp2[,2])), ]

  # table(vv.tmp2[,2])

  ly <- grep("loss", features)
  vv.tmp <-  terra::extract(terra::rast(features[[ly]]), res.final1)
  res.final <-res.final1[ vv.tmp$ID[ vv.tmp$lossyear < 17 ], ]
  message("Tile ", tile, " - Keeping post loss&Null-for-LC = ", as.integer( nrow(res.final)/nrow(res[[tile]])*100) , "%")

  vv <-  terra::extract(AGB_2018,res.final, ID=FALSE)
  # vv$ID<-NULL
  features <- features[-ly]
  for(feat in features){
    tmp <- terra::rast(feat)

      vv.temp <- terra::extract(tmp,res.final, ID=FALSE)
      # vv.temp$ID<-NULL
      if( length(names(vv.temp))==1 ){
        bn <- basename(feat)
        raster::extension(bn)<-""
        names(vv.temp)<-bn
      }
      vv<-cbind(vv, vv.temp)

  }


  vv$lcv_landcover <-as.factor(vv$lcv_landcover)
  vv$VegHighestProb<-as.factor(vv$VegHighestProb)

  df <- vv

  y <- "agb"

  browser()

  model = keras_model_sequential() %>%
    layer_dense(units=64, activation="relu", input_shape=3) %>%
    layer_dense(units=32, activation = "relu") %>%
    layer_dense(units=1, activation="linear")

  model %>% compile(
    loss = "mse",
    optimizer =  "adam",
    metrics = list("mean_absolute_error")
  )

  model %>% summary()

  # perf <- h2o.performance(aml2@leader, test)
  perf <- h2o.performance(aml2, test)
  metrics[[tile]] <- perf@metrics
  saveRDS(metrics, "output/models_v02_TF/metrics_v02.rds")
  saveRDS(varimp, "output/models_v02/varimp_v02.rds")
}

h2o.removeAll()
h2o.shutdown(prompt = FALSE)



metrics.clean <- lapply(metrics, function(x){ list(RMSE=x[["RMSE"]], r2=x[["r2"]]) } )
metrics.dt<-data.table::rbindlist(metrics.clean,  idcol = "tile")
writexl::write_xlsx(list(accuracy=metrics.dt), "output/models_v02/BiomassModelAccuracyMetrics.xlsx")
metrics.dt$Type <- "49 features"

metrics <- list(
  v02_50feat  = readRDS("output/models_v02/metrics_v02.rds"),
  v02_50feat_notOk = readRDS("output/models_v02_notOk/metrics_v02.rds"),
  v02_46feat = readRDS("output/models_v02noalos/metrics_v02.rds"),
  v01_28feat = readRDS("output/validation/metrics_v1.rds")
)

out.metric <- foreach(i = metrics) %do% {
  metrics.old.clean <- lapply(i, function(x){ list(RMSE=x[["RMSE"]], r2=x[["r2"]]) } )
  metrics.dt.old<-data.table::rbindlist(metrics.old.clean,  idcol = "tile")

}

names(out.metric)<-names(metrics)
final.metric <-  data.table::rbindlist(out.metric, idcol="Type")

 ggplot(final.metric) +
   geom_col( aes(x=tile, y=RMSE, fill=Type), position = "dodge" ) +
   # scale_fill_grey() +
   theme_bw()

