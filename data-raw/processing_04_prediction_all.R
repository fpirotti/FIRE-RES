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
  message("Doing chunk ", chunkn, " of ", nchunks)
  endel <- chunkn*chunk

  if(endel > nTotCells) endel <- nTotCells

  nn <- (( (chunkn-1) * chunk+1 ):endel)
  sub.cells <- nn # cells[nn]

  mask2 <- mask[nn]
  sub.cells.final<-sub.cells[mask2]
  # sub.cells.final2mask<-sub.cells[!mask2]

  ############################


  # AGB_2018_values <-  biomass.output[sub.cells.final]
  # AGB_2018_values_sd <-  biomass.outputSD[sub.cells.final]
  #


 # fc <- tot.raster$forestCover * ((lossyear.map < 10) + (lossyear.map==21))

  df1<- tot.raster[sub.cells.final]

  if(nrow(df1)==0){ return(NA) }

  lostForest <- (lossyear.map$lossyear[sub.cells.final][[1]] > 0) &  (lossyear.map$lossyear[sub.cells.final][[1]] < 21)
  ff <- (1 - ( lossyear.map$lossyear[sub.cells.final][[1]][lostForest] / 20  ))
  ##we assume that the forest cover will return to full cover after 20 years
  df1$forestCover[lostForest] <- df1$forestCover[lostForest] * ff
  df1$forestCanopyHeight[lostForest] <- df1$forestCanopyHeight[lostForest] * ff
  df1$VegHighestProb<-as.factor(df1$VegHighestProb)
  df1$lcv_landcover<-as.factor(df1$lcv_landcover)

  bioclim.data <- bioclim[terra::cellFromXY(bioclim, terra::xyFromCell(tot.raster, sub.cells.final) )]

  newdata <-  h2o::as.h2o( cbind(df1,  bioclim.data ) )
  preds.h2o <- h2o.predict(bm, newdata)


  # exp <- h2o.residual_analysis_plot(bm, newdata[1:100,])

  preds<-as.vector(preds.h2o)

  preds[preds<0]<-0
  final.biomass.output[sub.cells.final]<<-preds

  nsamples<-5000
  if(length(sub.cells.final) < nsamples) nsamples <- length(sub.cells.final)

  idx.sample <- sample(1:length(sub.cells.final), nsamples)
  sub.cells.final.noloss.sub  <- sub.cells.final[idx.sample]
  areNoLoss <- (lossyear.map$lossyear[sub.cells.final.noloss.sub][[1]]==0)
  sub.cells.final.noloss.sub <- sub.cells.final.noloss.sub[areNoLoss]
  idx.sample.sub <- idx.sample[areNoLoss]

  df <- data.frame(d2018=biomass.output[sub.cells.final.noloss.sub]$agb,
                   d2020=preds[idx.sample.sub])


  return(df)
}


tiles <- sf::read_sf("data-raw/tiles/bigTiles.gpkg")
AGB_2018 <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI-BIOMASS_L4_AGB_2018_v3_int16_europe.tif")
AGB_2018_e <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI_BIOMASS_L4_AGB_StdDev_2018_v3_int16_europe.tif")

metrics<- list()

#h2o.clusterStatus()

h2o.init()
for(tilen in 1:nrow(tiles) ) {

# foreach(tilen = 1:nrow(tiles) ) %do% {


  tilet <- tiles[tilen, ]
  tile<-tilet$ID
  if(is.element(tile, nonfare)) {
    next
  }

    # if(tile!="D2") next

  message("faccio ", tile)

  if(file.exists(sprintf("output/allEU/biomassMap_v03/biomassFromML_%s.tif", tile))){
    message("esiste, ", tile)
    next
  }

  features <- list.files(file.path("data-raw/tileData", tile ),
                         pattern =  "\\.tif$" , full.names = TRUE )


  if(length(features)!=11){
    lc <- terra::rast("/archivio/home/pirotti/Downloads/lcv_landcover.hcl_lucas.corine.eml_f_30m_0..0cm_2020_eumap_epsg3035_v0.2.tif")

    message(" PROBLEMA CON  ---", tile)
    # TTT<-terra::resample(lc, terra::rast(features[[5]]), method="mode", filename=file.path("data-raw/tileData", tile, "/lcv_landcover.tif" ), datatype="INT1U" )
    stop()
    # return(TTT)
  }

  # return(NA)
  # features <- list.files(file.path("data-raw/tileData", tile ),
  #                        pattern =  "\\.tif$" , full.names = TRUE )

  if(length(features)!=11){
    message("problem, ", tile);
    metrics[[tile]]<-NA;
    next
  }

  ## remove LossYear --------
  ly <- grep("loss", features)
  lossyear.map <- terra::rast( features[ly] )
  features <- features[-ly]
  ## READ feature RASTERS ----
  tmp <- list()
  bioclim <- NULL

  for(feat in features){

    vv.temp <- terra::rast(feat)
    bn <- basename(feat)
    raster::extension(bn)<-""
    if( length(names(vv.temp))==1 ){
      names(vv.temp)<-bn
    }
    # message(bn)
    if(bn=="lossYear") next
    if(bn!="bioclim") {
      tmp[[bn]]  <- vv.temp
    } else {
      bioclim <-vv.temp
    }
  }


  tot.raster <- terra::rast( unname(unlist( tmp )) )

  ## MASK ESA WORLD COVER ----
  # create mask using new WorldCover from ESA, keep only areas with fraction o
  # of grass/shrub/trees above 0%

  ff2<- terra::app(tmp$LULC_WC_10m, sum, cores=11)
  ff3<- terra::values(ff2)
  mask1<- ff3[,1]!=0


  ## MASK 3O m LULC corine land COVER ----
  ff3.hcl<- terra::values(tmp$lcv_landcover)
  mask.landcover <-  (ff3.hcl[,1] > 9  )  & (ff3.hcl[,1] < 37  )
  mask <- mask1 & mask.landcover

  mask[ is.na(mask[])] <- FALSE

  ## prepare output ---
  ### grab 2018 biomass values ----
  message("Cropping 2018 raster")
  biomass.output <- terra::crop(AGB_2018, tmp$lcv_landcover )
  biomass.outputSD <- terra::crop(AGB_2018_e, tmp$lcv_landcover )

  final.biomass.output <- biomass.output
  terra::values(final.biomass.output) <- NA

  terra::writeRaster(final.biomass.output,
                     "tmp.tif",
                     overwrite=TRUE,
                     datatype = "INT2S")

  ### prepare 4 layers in raster of biomass values ----
  final.biomass.output <- terra::rast("tmp.tif")
  final.biomass.output.se <- terra::rast("tmp.tif")


  chunk <- 5000000
  nTotCells <- terra::ncell(final.biomass.output) #length(cells)

  nchunks<-ceiling(nTotCells/chunk)

  ## LOAD MODEL ----
  if(is.element(tile, names(classification.lut) )){
    bm<-h2o.loadModel(
      path = file.path("output/models_v02",
                       sprintf("%s_biomass_prediction_model_tile", classification.lut[[tile]])
      ) )

  } else {
    bm<-h2o.loadModel(
      path = file.path("output/models_v02",
                       sprintf("%s_biomass_prediction_model_tile", tile)
      ) )

  }


  res <- foreach( i = 1:nchunks ) %do% doit(i)

  res2 <- lapply(res, function(x){ if(is.null(nrow(x)) ){ return(NULL)} else return(x) })
  df <- data.table::rbindlist(res2)

  model <- lm(d2018 ~ d2020, data=df)
  names(final.biomass.output)<-"d2020"
  preds.se <- predict(final.biomass.output, model, se.fit=TRUE)
  final.biomass.output.se <- final.biomass.output * preds.se$se.fit

  preds.se$residual.scale[] <- rnorm(ncell(preds.se), mean=1, sd=preds.se$se.fit[])

  preds.se.final <- preds.se$se.fit *  final.biomass.output

  final.biomass.output2 <- c(final.biomass.output, preds.se.final )

   # hh<-terra::spatSample(final.biomass.output2, 100000, cells=T)
   # hh <- na.omit(hh)
   # plot(hh$d2020, hh$se.fit , pch="." )
   # rnorm(200, mean=0, sd=1)

  # # ,
  # #                            biomass.output, biomass.outputSD)
  #
  # names(final.biomass.output2) <- c("agb_2020", "agb_2020_se" )

  terra::writeRaster(final.biomass.output2,
                     sprintf("output/allEU/biomassMap_v03/biomassFromML_%s.tif", tile),
                     overwrite=TRUE,
                     datatype = "INT2S")


}

#
