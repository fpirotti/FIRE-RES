library(ggplot2)
library(data.table)
library(foreach)
library(h2o)
library(doParallel);


## data processing ----

if(!dir.exists("data-raw/tileData") ){
  stop("You are missing the folder with data - please read/run DATA.R to download missing data!")
}


tiles <- sf::read_sf("data-raw/tiles/bigTiles.gpkg")
AGB_2018 <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI-BIOMASS_L4_AGB_2018_v3_int16_europe.tif")
AGB_2018_e <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI_BIOMASS_L4_AGB_StdDev_2018_v3_int16_europe.tif")

res <- readRDS("output/trainingTestingMatrixCoords.rds")

source("data-raw/processing_00_constants.R")

library(h2o)
h2o.init()
h2o.no_progress()

metrics<-list()
varimp <-list()

if(file.exists("output/models_v02/metrics_v02.rds")) metrics<-readRDS("output/models_v02/metrics_v02.rds")

tile <- "D2"
for(tile in tiles$ID) {

  if( is.element(tile, nonfare ) ){
    message("No do for tile ",tile)
    next
  }
  if( is.element(tile, names(classification.lut) ) ){
    message("No data for this tile (",tile,"), will use model from ", classification.lut[[tile]])
    next
  }

  path = file.path("output/models_v02_mojo",
                   sprintf("%s_biomass_prediction_model_tile", tile) )

  if(file.exists(path)){
    message("Model exists, skipping ", tile)
    next
  }

  aml2 <- h2o.loadModel(
    path = file.path("output/models_v02",
                     sprintf("%s_biomass_prediction_model_tile", tile)
    ) )

  mojo_destination <- h2o.save_mojo(aml2,
                                    path =  file.path("output/models_v02_mojo",
                                                      sprintf("%s_biomass_prediction_model_tile", tile)) )

  next
  message("doing ", tile)

  features <- list.files(paste0("data-raw/tileData/", tile), pattern = "\\.tif$", full.names = TRUE )
  if(length(features)!=11){
    message("problema tile ", tile)
    next
  }

  firstext <- ext(terra::rast(features[[1]]))
  for(ntmppp in features) {
    te<- ext(terra::rast(ntmppp))
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


  vv <-  terra::extract(AGB_2018,res.final)
  vv$ID<-NULL
  features <- features[-ly]
  for(feat in features){
    tmp <- terra::rast(feat)

      vv.temp <- terra::extract(tmp,res.final)
      vv.temp$ID<-NULL
      if( length(names(vv.temp))==1 ){
        bn <- basename(feat)
        raster::extension(bn)<-""
        names(vv.temp)<-bn
      }
      vv<-cbind(vv, vv.temp)

  }


  vv$lcv_landcover <-as.factor(vv$lcv_landcover)
  vv$VegHighestProb<-as.factor(vv$VegHighestProb)

  df <- h2o::as.h2o(vv)

  y <- "agb"
  splits <- h2o.splitFrame(df, ratios = 0.75, seed = 1)
  train <- splits[[1]]
  test <- splits[[2]]


  aml2 <- h2o.automl(y = y,
                    training_frame = train,
                    max_models = 24,
                    project_name = sprintf("%s_biomass_prediction_tile", tile))

  h2o.saveModel(object = aml2@leader, path = "output/models_v02",
               filename =  sprintf("%s_biomass_prediction_model_tile", tile), force = TRUE)
  imported_model <- h2o.import_mojo(mojo_destination)

  modid <- as.data.frame((aml2@leaderboard$model_id ))
  notstacked <- which( !grepl("Stacked", modid$model_id ) )
  varimp[[tile]]<-list()
  for(mod in notstacked){
    modn <- modid$model_id[[mod]]
    varimp[[tile]][[ modn ]] <- as.data.frame(h2o.varimp(h2o.getModel( modn ) ))
  }

  perf <- h2o.performance(aml2@leader, test)
  # pred <- h2o.predict(aml2, test)
  #
  metrics[[tile]] <- perf@metrics
  saveRDS(metrics, "output/models_v02/metrics_v02.rds")
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
  #v02_50feat_notOk = readRDS("output/models_v02_notOk/metrics_v02.rds"),
  v02_46feat = readRDS("output/models_v02noalos/metrics_v02.rds")
  #v01_28feat = readRDS("output/validation/metrics_v1.rds")
)

out.metric <- foreach(i = metrics) %do% {
  metrics.old.clean <- lapply(i, function(x){

    list(RMSE=x[["RMSE"]], r2=x[["r2"]], MAE=x[["MAE"]]) } )
  metrics.dt.old<-data.table::rbindlist(metrics.old.clean,  idcol = "tile")

}

names(out.metric)<-names(metrics)
final.metric <-  data.table::rbindlist(out.metric, idcol="Type")
ff =  final.metric %>% filter(tile != "B0" & tile != "B1")
ff1 = final.metric %>% filter(tile != "B0" & tile != "B1" & Type=="v02_50feat")
ff2 = final.metric %>% filter(tile != "B0" & tile != "B1" & Type=="v02_46feat")


ggplot(ff) +
   geom_col( aes(x=tile, y=RMSE, fill=Type), position = "dodge" ) +
   scale_fill_grey() +
   geom_hline( aes(yintercept =median(ff2$RMSE)), lwd=1 ) +
   geom_hline( aes(yintercept =median(ff1$RMSE)), lwd=1, col="grey" ) +
   theme_bw()

