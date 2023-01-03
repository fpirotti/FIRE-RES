library(data.table)
library(foreach)
library(terra)
library(doParallel);


## data processing ----
csvs <- list.files("data-raw/samples", pattern = "*.csv$", full.names = TRUE)

tiles<-substr(csvs, 26,27)


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
}


cs <- makeCluster(12)
registerDoParallel(cs)
res <-  foreach(csvfile = csvs) %dopar% {
  as.data.frame(readCSV(csvfile))
}
stopCluster(cs)

names(res) <-  tiles
cc <-data.table::rbindlist(res, idcol = "tile")
sfout<-sf::st_as_sf( cc, coords=c(2,3), crs=4326)
sf::write_sf(sfout, sprintf("output/stratifiedSamples.gpkg"))

saveRDS(res, "output/trainingTestingMatrixCoords.rds")
saveRDS(sfout, "output/trainingTestingMatrixCoordsSF.rds")

res <- readRDS("trainingTestingMatrixCoords.rds")

