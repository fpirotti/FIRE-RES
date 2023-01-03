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



  df1<- tot.raster[sub.cells.final]
  df1$VegHighestProb<-as.factor(df1$VegHighestProb)
  df1$lcv_landcover<-as.factor(df1$lcv_landcover)

  bioclim.data <- bioclim[terra::cellFromXY(bioclim, terra::xyFromCell(tot.raster, sub.cells.final) )]

  newdata <-  h2o::as.h2o( cbind(df1,  bioclim.data) )
  preds <- h2o.predict(bm, newdata)

  # exp <- h2o.residual_analysis_plot(bm, newdata[1:100,])

  preds<-as.vector(preds)

  ## remove
  # lossyear <- which(lossyear.map[sub.cells.final] < 17 )
  # sub.cells.final.noloss <- sub.cells.final[ lossyear ]
  final.biomass.output[sub.cells.final]<<-preds

  # for(boot in 1:10){
  #   sub.cells.final.noloss.sub  <- sample(sub.cells.final.noloss, 2000)
  #   obs <- final.biomass.output[sub.cells.final.noloss]
  #
  #   pred.y <- obs$agb_2020
  #
  #   ll<-lm(obs$agb_2020 ~  obs$agb_2018)
  #   fm1.prd <- predict(ll, interval = "prediction")
  #   confidence <- fm1.prd[,3] - fm1.prd[,2]
  #   confidence <- confidence/2
  #   smoothScatter( (obs$agb_2020 -  obs$agb_2018) /  obs$agb_2018 * 100 ~  obs$agb_2018,
  #                  ylim = c(-200,200))
  #   hist( (obs$agb_2020 -  obs$agb_2018) /  obs$agb_2018 * 100, xlim=c(-200,200), breaks=1000 )
  #   summary(ll)
  # }

  # preds[preds<1]<-NA

  # final.biomass.output$agb_2020_se[sub.cells.final] <<- preds - final.biomass.output$agb_2018[sub.cells.final]
  # gc()
  # final.biomass.output$agb_2020[sub.cells.final2mask]<<-NA
  # gc()
  # final.biomass.output$agb_2020_se[sub.cells.final2mask]<<-NA
  # gc()
  return(length(sub.cells.final))
}


tiles <- sf::read_sf("data-raw/tiles/bigTiles.gpkg")
AGB_2018 <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI-BIOMASS_L4_AGB_2018_v3_int16_europe.tif")
AGB_2018_e <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI_BIOMASS_L4_AGB_StdDev_2018_v3_int16_europe.tif")

metrics<- list()

h2o.init()
#h2o.clusterStatus()

for(tilen in 1:nrow(tiles) ) {
# foreach(tilen = 1:nrow(tiles) ) %do% {


  tilet <- tiles[tilen, ]
  tile<-tilet$ID
  if(is.element(tile, nonfare)) {
    next
  }
   message("faccio ", tile)

  if(file.exists(sprintf("output/allEU/biomassMap/biomassFromML_%s.tif", tile))){
    message("esiste, ", tile)
    next
  }

  features <- list.files(file.path("data-raw/tileData", tile ),
                         pattern =  "\\.tif$" , full.names = TRUE )


  if(length(features)!=11){
    lc <- terra::rast("/archivio/home/pirotti/Downloads/lcv_landcover.hcl_lucas.corine.eml_f_30m_0..0cm_2020_eumap_epsg3035_v0.2.tif")

    message("faccio ---", tile)
    # TTT<-terra::resample(lc, terra::rast(features[[5]]), method="mode", filename=file.path("data-raw/tileData", tile, "/lcv_landcover.tif" ), datatype="INT1U" )
    next
    # return(TTT)
  }

  # return(NA)
  features <- list.files(file.path("data-raw/tileData", tile ),
                         pattern =  "\\.tif$" , full.names = TRUE )

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

  ff2<- terra::app(tmp$LULC_WC_10m, sum)
  ff3<- terra::values(ff2)
  mask1<- ff3[,1]!=0


  ## MASK 3O m LULC corine land COVER ----
  ff3.hcl<- terra::values(tmp$lcv_landcover)
  remains <- ff3.hcl[,1][mask1]
  mask <- mask1 & (ff3.hcl[,1] > 9  )  & (ff3.hcl[,1] < 37  )

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



  res <- foreach( i = 1:nchunks) %do% doit(i)


  final.biomass.output2 <- c(final.biomass.output,
                             (final.biomass.output -  biomass.output),
                             biomass.output, biomass.outputSD)

  names(final.biomass.output2) <- c("agb_2020", "agb_2020_se",
                                    "agb_2018", "agb_2018_se")

  terra::writeRaster(final.biomass.output2,
                     sprintf("output/allEU/biomassMap/biomassFromML_%s.tif", tile),
                     overwrite=TRUE,
                     datatype = "INT2S")


}

#
h2o.shutdown()
