library(ggplot2)
library(sf)
library(terra)
library(foreach)
library(doParallel)
## data processing ----

source("data-raw/processing_00_constants.R")


dbhFromHeight <- function(H, Hmc){
   A =  -2.19715 + 7.5437 *log10(H) - 5.422006* log10(Hmc)
   A = 10^A
   Lh = (Hmc - 1.37)
   x = A * Lh
   DBH=1.36*x^0.017
   DBH
}


H = 30
Hmc = 15:28

plot(Hmc, dbhFromHeight(H,Hmc), ylab="DBH")
