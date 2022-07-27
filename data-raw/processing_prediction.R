library(ggplot2)
library(terra)
## data processing ----

if(!file.exists("data-raw/samples4biomassML.csv")){
  stop("Please read/run DATA.R to download missing data")
}

train.csv <- read.csv("data-raw/samples4biomassML.csv" )
train.csv$.geo<-NULL
train.csv$system.index<-NULL
names(train.csv)

## reading bioclim data
features.bioclim<-terra::rast("data-raw/features_bioclim.tif")
names(features.bioclim)
features.rest<-terra::rast("data-raw/features_100m.tif")
## remove pixels with 0 values by assigning to NA
features.rest.masked <- terra::mask(features.rest, features.rest$biomass, maskvalues=0)
biomass.output <- terra::rast(features.rest.masked$biomass)
# plot(features.rest.masked$biomass)

cells <- terra::cells(features.rest.masked)

##

h2o.init()
#h2o.clusterStatus()
bm<-h2o.loadModel("models/bm_spain/StackedEnsemble_AllModels_6_AutoML_3_20220726_193310")

##


h2o.shutdown()
