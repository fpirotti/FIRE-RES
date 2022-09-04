library(ggplot2)
library(data.table)
library(terra)
library(foreach)
library(parallel)
## data processing ----

if(!file.exists("data-raw/testing.csv") ||
   !file.exists("data-raw/features_100m.tif")||
   !file.exists("data-raw/features_bioclim.tif") ){
  stop("Please read/run DATA.R to download missing data")
}
h2o.init()
#h2o.clusterStatus()
bm<-h2o.loadModel("models/bm_spain/StackedEnsemble_AllModels_1_AutoML_1_20220802_191956")

## reading bioclim data
features.bioclim<-terra::rast("data-raw/features_bioclim.tif")
names(features.bioclim)
features.rest<-terra::rast("data-raw/features_100m.tif")

### mask pixels with 0  ----
# features.rest.masked <- terra::mask(features.rest, features.rest$biomass, maskvalues=0)
### create blank output  ----
biomass.output <- terra::rast(features.rest$biomass)
biomass.outputSD <- terra::rast(features.rest$biomass)

# plot(features.rest.masked$biomass)
ncells <- terra::ncell(features.rest$biomass)/1000000

if(ncells > 2^31/1000000/2){
  message("Too many values! Have to chunck")
}

cells <- terra::cells(features.rest$biomass)
chunk <- 10000000
nTotCells <- length(cells)
nchunks<-ceiling(nTotCells/chunk)
### split to keep only 10 M lines of the table

doit <- function(chunkn){
  message("Doing chunk ", chunkn, " of ", nchunks)
  endel <- chunkn*chunk

  if(endel > nTotCells) endel <- nTotCells

  nn <- (( (chunkn-1) * chunk+1 ):endel)
  sub.cells <- cells[nn]
  mask <- features.rest$discrete_classification[sub.cells][,1]
  sub.cells.final<-sub.cells[which(mask!=0)]
  sub.cells.final2mask<-sub.cells[which(mask==0)]
  df<- features.rest[sub.cells.final]
  df$discrete_classification<-as.factor(df$discrete_classification)
  df$forest_type<-as.factor(df$forest_type)
  ## bioclim raster is 1 km x 1 km therefore not
  ## stackable with other variables with 100 m x 100 m pixel
  ## we therefore sample the values by XY
  bioclim <- features.bioclim[terra::cellFromXY(features.bioclim, terra::xyFromCell(features.rest, sub.cells.final) )]
  newdata <-  h2o::as.h2o( cbind(df, bioclim) )
  pred <- h2o.predict(bm, newdata)
  preds<-as.vector(pred)
  obs<-features.rest$biomass[sub.cells.final][,1]
  preds[preds<1]<-NA
  biomass.output[sub.cells.final]<<-preds
  biomass.outputSD[sub.cells.final]<<-obs-preds
  biomass.output[sub.cells.final2mask]<<-NA
  biomass.outputSD[sub.cells.final2mask]<<-NA
  return(obs-preds )
}
res<-numeric(nTotCells)
res <- foreach( i = 1:nchunks) %do% doit(i)

terra::writeRaster(biomass.output, "biomassFromML.tif", overwrite=TRUE)
terra::writeRaster(biomass.outputSD, "biomassFromML_SD.tif", overwrite=TRUE)
h2o.shutdown()
