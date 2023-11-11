library(ggplot2)
library(sf)
library(terra)
library(foreach)
library(parallel)
library(doParallel)
## data processing ----

source("data-raw/processing_00_constants.R")


biomass <- terra::rast("output/final/biomass2020fireres.tif")
states <- terra::vect("/archivio/shared/geodati/vettoriali/confini/EU/EUstates.shp")
cuts <- seq(0, 300, 10)
tables<-list()
for(i in 1:length(states) ){
  state=states[i,]
  ext <- terra::extract(biomass, state)
  cc <- cut(ext$biomass2020fireres, cuts)
  tables[[state$NAME_FIX]] <- table(cc)
}
saveRDS(tables, file = "processing_08_tables.rds")

