library(rgee)
library(rgeeExtra)
library("inborutils")
library("parallel")
library("terra")
ee_Initialize(quiet = T)
# ee_Initialize(user = 'ndef', drive = TRUE )
ee_Initialize(user = 'cirgeo',project = 'progetto-eu-h2020-cirgeo', drive = TRUE )

# ee_install_upgrade()

biomass <- ee$Image("projects/ee-fireres/assets/biomassV03tiles/biomassFromML_v03")
states <- ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/global/NUTS_2024_01M_all")

proj3035_30m = ee$Projection('EPSG:3035')$atScale(100);

# print(states$getInfo())

clipit = function(feat){
   name = feat$get("NAME_FIX");
   id = feat$get("ident");
   img=biomass$select(0)$clip(feat)$set("name", name )$updateMask( countries$select(0)$eq(ee$Number(id) ) )
   return(img)
}

countries =  ee$Image( states$reduceToImage(
  properties=list('ident'),
  reducer= ee$Reducer$first()
  ) );

newimg = ee$ImageCollection(states$map(clipit));

 sz = newimg$size()$getInfo();
 names = newimg$aggregate_array('name')$getInfo();
 name <- names[[1]]

id <- 1
task_img_container<-list()
for( id in 1:length(names)){

  img = ee$Image(newimg$toList(1, (id-1))$get(0));
  print(names[[id]])


  task_img_container[[names[[id]]]] <-
    ee_image_to_drive(
      image=img$toFloat(),
      folder= 'FIRE-RES-DATA',
      timePrefix=FALSE,
      description= names[[id]],
      scale=100,
      fileFormat= 'GEO_TIFF',
      fileNamePrefix = names[[id]],
      maxPixels= as.integer(900000000)
    )

  task_img_container[[ names[[id]] ]]$start()
  # task_img_container[[ names[[id]] ]]$status

}
CRS("+init=epsg:32632")
# for( task in  task_img_container){
#   task$cancel()
# }
