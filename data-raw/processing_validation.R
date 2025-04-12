library(ggplot2)
library(data.table)
library(terra)
library(foreach)
library(parallel)
## data processing ----


features.rest<-terra::rast("data-raw/features_100m.tif")
biomass.output <- terra::rast( "biomassFromML.tif" )
biomass.outputSD<-terra::rast("biomassFromML_SD.tif" )

tc<-terra::cells(biomass.outputSD)
diffs<-biomass.outputSD[tc][[1]]
absDeltas<-abs(diffs)

largeDiffs<-(which(absDeltas>310))
largeDiffs.df<-as.data.frame(terra::xyFromCell(biomass.outputSD, tc[largeDiffs]))
largeDiffs.df$PredictedBiomassML <- biomass.output[ tc[largeDiffs] ][[1]]
largeDiffs.df$SantoroBiomass <- features.rest$biomass[ tc[largeDiffs] ][[1]]

sf::write_sf(sf::st_as_sf(largeDiffs.df, coords=c(1,2), crs=4326), "largeErrors.gpkg")

sd1<-sd(diffs)
png("man/images/DifferenceDistribution.png", width=1000, height=600, res=160)
hist(biomass.outputSD, breaks=600, xlim=c(-100, 100),
     main="CEDA vs AI model differences", xlab="CEDA Biomass - AI model (Mg/ha)")
abline(v=sd1, col="red" ,lwd=2)
abline(v=-sd1, col="red", lwd=2)
dev.off()


jpeg("man/images/compare.jpg", width=2000, height=1500, res=300)
terra::plot(biomass.output, col=viridis::turbo(12), main="ESA Biomass Climate Change Initiative 2018 (Mg/ha)" )
dev.off()

jpeg("man/images/compareAI.jpg", width=2000, height=1500, res=300)
terra::plot(biomass.output, col=viridis::turbo(12), main="AI-model Biomass 2020 (Mg/ha)" )
dev.off()

bosd<-terra::clamp(biomass.outputSD, lower=-200, upper=200, values=TRUE)
jpeg("man/images/diff.jpg", width=2000, height=1500, res=300)
terra::plot(bosd, col=viridis::turbo(12),
            range= c(-200,200), main="ESA(CEDA) - AI model (Mg/ha)" )
dev.off()

##


##


