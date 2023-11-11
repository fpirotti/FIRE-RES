library(ggplot2)
library(data.table)
library(terra)
library(foreach)
library(parallel)
library(doParallel)
library(h2o)
## data processing ----

source("data-raw/processing_00_constants.R")

doit <- function(chunkn){
  message("Doing chunk ", chunkn, " of a total of ", nchunks ," chunks...")
  ## numero cella finale del chunk
  endel <- chunkn*chunk
  ## se numero cella finale del chunk è maggiore
  ## del numero totale di celle, allora il numero cella finale
  ## sarà uguale al numero totale di celle
  if(endel > nTotCells) endel <- nTotCells

  ## vettore con ID celle
  sub.cells <- (( (chunkn-1) * chunk+1 ):endel)

  ## assegno a df1 i valori del raster grande originale
  df1<- tot.raster[sub.cells]
  ## qui sotto applico un modello di AI
  newdata <-  h2o::as.h2o(  df1  )
  preds.h2o <- h2o.predict("oggetto con mio modello di AI tipo MOJO o altri standard", newdata)
  ## converto i valori in un vettore
  preds<-as.vector(preds.h2o)
  ## la funzione ritorna i valori modificati del pezzo di raster
  return(preds)
}

h2o.init()
## il raster gigante
tot.raster <- terra::rast("ilmio raster")

## numero di celle per ogni pezzo di raster
chunk <- 5000000
## numero di celle in totale nel raster
nTotCells <- terra::ncell(final.biomass.output) #length(cells)
## numero di pezzi
nchunks<-ceiling(nTotCells/chunk)

## LOOP
## se invece di fare %do% si fa %dopar% si può rendere parallelo
## ma attenzione che Terra ha problemi con il parallel
res <- foreach( i = 1:nchunks ) %do% doit(i)
## res è un oggetto di tipo list, lo fondo in un unico vettore
df <- unname( unlist( res ) )

## salvo un raster singola banda in file temporaneo,
terra::writeRaster(tot.raster[[1]],
                   "tmp.tif",
                   overwrite=TRUE,
                   datatype = "INT2S")
## lo rileggo...
final.biomass.output <- terra::rast("tmp.tif")
## assegno i nuovi valori
final.biomass.output[] <- df

terra::writeRaster(final.biomass.output,
                   "mio output.tif",
                   overwrite=TRUE,
                   datatype = "INT2S")



