library(ggplot2)
library(data.table)
library(foreach)
library(h2o)
library(terra)
library(doParallel);


## data processing ----
csvs <- list.files("data-raw/allDataEU", pattern = "*.csv$", full.names = TRUE)
feat <- list.files("data-raw/allDataEU", pattern = "*\\.tif$", full.names = TRUE)

# tiles <- stringr::str_extract(basename(csvs), "[A-F][0-9]")
# tiles1 <- stringr::str_extract(basename(feat1), "[A-F][0-9]")
# setdiff(tiles, tiles1)

readCSV <- function(csvfile){
  tile <- stringr::str_extract(basename(csvfile), "[A-F][0-9]")
  cs <- read.csv(csvfile )
  coordinates <- substring(cs[,3], 49,80)
  coordinates <- stringr::str_replace_all(coordinates, "[}\\]]", "")
  cc <- (stringr::str_split(coordinates, ",", simplify = TRUE))
  class(cc)<-"numeric"
  nas<-which(is.na(cc[,2]))
  if(length(nas)>0){
    cc <- (stringr::str_split(coordinates, ",", simplify = TRUE))
    return(list(cc[nas,], name=tile))
  }
  return(cc)
  # sfout<-sf::st_as_sf(as.data.frame(cc), coords=c(1,2), crs=4326)
  # sf::write_sf(sfout, sprintf("output/allEU/train_%s.gpkg", tile))
  # return(sfout)
}


# cs <- makeCluster(12)
# registerDoParallel(cs)
# res <-  foreach(csvfile = csvs) %dopar% {
#   readCSV(csvfile)
# }
# res
# stopCluster(cs)
# names(res) <-  tiles
# saveRDS(res, "trainingTestingMatrixCoords.rds")

 res <- readRDS("trainingTestingMatrixCoords.rds")
# reslist <- lapply(res, as.data.frame)
# resdt <- data.table::rbindlist(reslist,use.names =  TRUE, idcol = "tile")
# sfout<-sf::st_as_sf(as.data.frame(resdt), coords=c(2,3), crs=4326)
# sf::write_sf(sfout, sprintf("output/allEU/train_ALL2.gpkg"))

library(h2o)
h2o.init()
h2o.no_progress()

metrics<-readRDS("metrics.rds")
for(tile in tiles) {

  path = file.path("models/allEU",
                   sprintf("biomass_prediction_model_tile_%s", tile)
                  )

  if(file.exists(path)){
    message("skipping ", tile)
    next
    }

  message("doing ", tile)
  features <- list.files("data-raw/allDataEU", pattern = sprintf("%s\\.tif$", tile), full.names = TRUE )
  vv <- NULL
  if(length(features)!=3){
    message("WARNING ", tile)
    metrics[[tile]]<-NA;  next }
  for(feat in features){
    tmp <- terra::rast(feat)
    if(is.null(vv)) vv<-terra::extract(tmp,res[[tile]])
    else vv<-cbind(vv, terra::extract(tmp,res[[tile]]))
  }
  vv$discrete_classification<-as.factor(vv$discrete_classification)
  vv$forest_type<-as.factor(vv$forest_type)
  if(!is.null(vv$be75)){
    vv$elevation <- vv$be75
    vv$be75<-NULL
  }

  df <- h2o::as.h2o(vv)

  y <- "biomass"
  splits <- h2o.splitFrame(df, ratios = 0.8, seed = 1)
  train <- splits[[1]]
  test <- splits[[2]]


  aml2 <- h2o.automl(y = y,
                     training_frame = train,
                     max_models = 30,
                     project_name = sprintf("biomass_prediction_tile_%s", tile))

  h2o.saveModel(object = aml2@leader, path = "models/allEU",
                filename =  sprintf("biomass_prediction_model_tile_%s", tile), force = TRUE)

  perf <- h2o.performance(aml2@leader, test)
  metrics[[tile]] <- perf@metrics
  saveRDS(metrics, "metrics.rds")
}

metrics.clean <- lapply(metrics, function(x){ list(RMSE=x[["RMSE"]], r2=x[["r2"]]) } )
metrics.dt<-data.table::rbindlist(metrics.clean,  idcol = "tile")
writexl::write_xlsx(list(accuracy=metrics.dt), "output/allEU/BiomassModelAccuracyMetrics.xlsx")
barplot(names=metrics.dt$tile, height=metrics.dt$RMSE)

h2o.removeAll()
h2o.shutdown(prompt = FALSE)




