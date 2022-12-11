library(rgee)
ee_Initialize()

# ee_install_upgrade()

AGB_2018 <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI_BIOMASS_L4_AGB_100m_2018_v3_europe.tif")
AGB_2018_e <- terra::rast("/archivio/shared/geodati/raster/AGB/ESA2018/ESACCI_BIOMASS_L4_AGB_100m_2018_v3_europe_stdError.tif")


forestCover <- ee$Image("UMD/hansen/global_forest_change_2021_v1_9")
LULC <- ee$Image("COPERNICUS/Landcover/100m/Proba-V-C3/Global/2019")
ALOSyearlyMosaic <- ee$Image("JAXA/ALOS/PALSAR/YEARLY/SAR/2020")
bioclim  <- ee$Image("WORLDCLIM/V1/BIO")
ALOS$getInfo()
