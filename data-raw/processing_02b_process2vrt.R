library(data.table)
library(foreach)
library(terra)
library(doParallel);


template <- list.files("data-raw/tileData/A1", full.names = T, recursive = T)

  toremove <- list()
  feat <- list()


wd <- file.path(getwd(), "data-raw/tileData")

foreach( i = template) %dopar%  {

  basename<-basename(i)
  template2 <- list.files(path = wd, pattern = paste0(basename, "$"), full.names = F, recursive = T)

  # feat[[basename(i)]] <- foreach( ii = template2) %dopar% {
  #   tt <- (terra::rast(ii))
  #   vv<-tt[[1]][terra::cells(tt)]
  #   mean(vv[[1]], na.rm = T )
  # }
#   names(feat[[basename(i)]]) <- basename(dirname(template2))
#
   raster::extension(basename)<-"vrt"
   ovr.filename=file.path(wd, paste0(basename, ".ovr"))

   terra::vrt(template2, filename=file.path(wd, basename), overwrite=TRUE)
   tx  <- readLines(file.path(wd, basename))
   tx2  <- gsub(pattern = 'SourceFilename relativeToVRT="0"',
               replace = 'SourceFilename relativeToVRT="1"', x = tx)
  writeLines(tx2, con=file.path(wd, basename))
  if(!file.exists(ovr.filename)) gdalUtils::gdaladdo(file.path(wd, basename), levels = c(2,4,8,16,32,64,128,256,512), r="nearest", ro=TRUE,verbose=TRUE)
}

setwd(wd)
