library(ggplot2)
library(data.table)
library(terra)
library(foreach)
library(parallel)
## data processing ----



doit <- function(chunkn){
  message("Doing chunk ", chunkn, " of ", nchunks)
  endel <- chunkn*chunk

  if(endel > nTotCells) endel <- nTotCells

  nn <- (( (chunkn-1) * chunk+1 ):endel)
  sub.cells <- nn # cells[nn]

  mask2 <- mask[nn]
  sub.cells.final<-sub.cells[mask2]
  sub.cells.final2mask<-sub.cells[!mask2]
  df1<- features1[sub.cells.final]
  df1$discrete_classification<-as.factor(df1$discrete_classification)
  df1$forest_type<-as.factor(df1$forest_type)

  df2<- features2[sub.cells.final]
  ## bioclim raster is 1 km x 1 km therefore not
  ## stackable with other variables with 100 m x 100 m pixel
  ## we therefore sample the values by XY
  bioclim <- features.bioclim[terra::cellFromXY(features.bioclim, terra::xyFromCell(features1, sub.cells.final) )]
  newdata <-  h2o::as.h2o( cbind(df1, df2, bioclim) )
  pred <- h2o.predict(bm, newdata)
  preds<-as.vector(pred)
  obs<-features2$biomass[sub.cells.final][,1]
  preds[preds<1]<-NA
  biomass.output[sub.cells.final]<<-preds
  biomass.outputSD[sub.cells.final]<<- (obs-preds)
  biomass.output[sub.cells.final2mask]<<-NA
  biomass.outputSD[sub.cells.final2mask]<<-NA
  return(length(sub.cells.final))
}



csvs <- list.files("data-raw/allDataEU", pattern = "*.csv$", full.names = TRUE)
feat1 <- list.files("data-raw/allDataEU", pattern = "*.tif$", full.names = TRUE)

tiles <- stringr::str_extract(basename(csvs), "[A-F][0-9]")

metrics<- list()

h2o.init()
#h2o.clusterStatus()

for(tile in tiles) {

  if(file.exists(sprintf("output/allEU/biomassMap/biomassFromML_%s.tif", tile))){
    message("esiste, ", tile)
    next
  }

  message("faccio ", tile)

  features <- list.files("data-raw/allDataEU",
                         pattern = sprintf("%s\\.tif$", tile), full.names = TRUE )

  if(length(features)!=3){ message("problem, ", tile); metrics[[tile]]<-NA;  next }

  features.bioclim<-terra::rast(features[[1]])
  features1<-terra::rast(features[[2]])
  features2<-terra::rast(features[[3]])

  if(!is.element("elevation", names(features2))){
    ww<-which(names(features2) == "be75" )
    names(features2)[[ww]] <- "elevation"
    message("Renamed elevation column")
  }

  # create mask using new WorldCover from ESA, keep only areas with fraction o
  # of grass/shrub/trees above 0%
  ff<-features1[[c("Trees_f", "Shrubland_f", "Grassland_f")]]
  ff2<- terra::app(ff, sum)
  ff3<- terra::values(ff2)
  mask<- ff3[,1]!=0

  ## prepare output
  biomass.output <- terra::rast(features2$biomass)
  biomass.outputSD <- terra::rast(features2$biomass)


  cells <- 1:terra::ncell(features2$biomass) ## terra::cells(features2$biomass)
  # cells2 <- terra::cells(features2$biomass)
  chunk <- 10000000
  nTotCells <- length(cells)

  nchunks<-ceiling(nTotCells/chunk)

  bm<-h2o.loadModel(
    path = file.path("models/allEU",
                     sprintf("biomass_prediction_model_tile_%s", tile)
                     ) )


  res <- foreach( i = 1:nchunks) %do% doit(i)

  terra::writeRaster(biomass.output,
                     sprintf("output/allEU/biomassMap/biomassFromML_%s.tif", tile),
                     overwrite=TRUE)

  terra::writeRaster(biomass.outputSD,
                     sprintf("output/allEU/biomassMap/biomassFromML_SD_%s.tif", tile),
                     overwrite=TRUE)
}


res<-numeric(nTotCells)
res <- foreach( i = 1:nchunks) %do% doit(i)

terra::writeRaster(biomass.output, "biomassFromML.tif", overwrite=TRUE)
terra::writeRaster(biomass.outputSD, "biomassFromML_SD.tif", overwrite=TRUE)
h2o.shutdown()
