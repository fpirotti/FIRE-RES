library(rgee)
library(reticulate)
library(rgeeExtra)
library(bit64)
library(sf)
library("terra")
library(googledrive)
library(tidyverse)

 boundingBox = ee$Geometry$Rectangle(c(-10.989848835, 28.992632051,
                                         34.828811442, 76.912595540));


 # ee_Initialize(quiet = T)
# # ee_Initialize(user = 'ndef', drive = TRUE )
#  ee_Initialize(user = 'cirgeo'  )
#  ee_Authenticate()
# reticulate::py_run_string("import ee; ee.Initialize(  project='progetto-eu-h2020-cirgeo' )")


ee_Initialize(user = 'cirgeo',project = 'progetto-eu-h2020-cirgeo', drive = TRUE )

nuts <- ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/global/NUTS_2024_01M_all")
proj3035  = ee$Projection('EPSG:3035')$atScale(100);

# ee_install_upgrade()
items <- list(
  biomass='projects/progetto-eu-h2020-cirgeo/assets/fire-res/biomass',
  cbh='projects/progetto-eu-h2020-cirgeo/assets/fire-res/canopyBaseHeight',
  cbd='projects/progetto-eu-h2020-cirgeo/assets/fire-res/canopyBulkDensity',
  elevation='projects/progetto-eu-h2020-cirgeo/assets/fire-res/elevation',
  slope='projects/progetto-eu-h2020-cirgeo/assets/fire-res/slope',
  aspect='projects/progetto-eu-h2020-cirgeo/assets/fire-res/aspect',
  canopyHeight='projects/progetto-eu-h2020-cirgeo/assets/fire-res/canopyHeight',
  canopyCover='projects/progetto-eu-h2020-cirgeo/assets/fire-res/canopyCover',
  fuel='projects/progetto-eu-h2020-cirgeo/assets/fire-res/fuelModelScottBurgan_v2024_06_09'
)

# al <- rgee::ee_manage_assetlist("projects/progetto-eu-h2020-cirgeo/assets/fire-res")



rmse <- list(
  cbdrmse='projects/progetto-eu-h2020-cirgeo/assets/fire-res/canopyBaseHeight_RMSE',
  cbdrmse='projects/progetto-eu-h2020-cirgeo/assets/fire-res/canopyBulkDensity_RMSE'
)


task_img_container<-list()
task_img_container.status<-list()

goGEE <- function(id, crs.in='CRS:4326', force=F) {

  message( " ..... ", id, " ..... ")
  if(crs.in!='CRS:4326'){
    crs.in  = ee$Projection('EPSG:3035')$atScale(100)
  }
  if(!is.element(id, folders$name)){
    message(id, " folder not in drive, creating it!")
    idfold <- googledrive::drive_mkdir( id, path="FIRE-RES-DATA")
  } else {
    warning(id, " folder exists , please remove all folders from FIRE-RES-DATA... make sure you downloaded them first....")
    filesn <- googledrive::drive_ls( sprintf("FIRE-RES-DATA/%s", id))
    namesl <- c(namesl , filesn$name)
    rems <- c(rems , filesn$id)
    return()
  }


  feat <- states$filter(ee$Filter$eq("NUTS_ID", id))

  ff <- feat$geometry()$geometries()$map(
    rgee::ee_utils_pyfunc(
      function(k){   ee$Feature(ee$Geometry(k)) }
    )
  )

  ff2 <- ee$FeatureCollection(ff)#$filterBounds( tile$geometry()$bounds() )
  gi <- ff2$getInfo()
  if(length(gi$features)==0){
    message(desc , "NO FEATURES!!!! "  )
    task_img_container[[desc]] <<- paste(id, " NO FEATures ")
    return(NULL)
  }


  ff2 <- ee$FeatureCollection(ff)$filterBounds( boundingBox )
  gi <- ff2$getInfo()
  if(length(gi$features)==0){
    message(desc , "NO FEATURES AFTER BOUNDING BOX CLIP!!!! "   )
    task_img_container[[desc]] <<- paste(id, " NO FEATures after BBOX")
    return(NULL)
  }


  for( item in names(items)){

    img = ee$Image(items[[item]]);

    desc <- sprintf("%s_%s", id, item)


    if( !is.null(task_img_container[[ desc ]] ) ) {
      message(desc, " task exists skipping...")
      break
    }

    if(is.element(desc, files.name.noExtension) ) {
      message(desc, " file exists... remove before  redoing")
      task_img_container[[desc]] <<- paste(desc, " file exists... remove before  redoing")
      if(!force) break
    }


    message(desc , " doing " , length(gi$features), " features..." )
    task_img_container[[desc]] <<-
      rgee::ee_image_to_drive(
        image=img$toFloat()$clip(ff2),
        folder= sprintf('%s', id) ,
        timePrefix=FALSE,
        description=sprintf('%s_%s', id, item) , #"fuelModelScottBurgan",
        region =   ff2$geometry(), # img$geometry()$bounds()
        scale=100L,
        crs=crs.in,
        fileFormat= 'GEO_TIFF',
        maxPixels= 3207879571
      )
    task_img_container[[ desc ]]$start()
    # ee_monitoring( task_img_container[[ desc ]])

  }
}



  states <- nuts$filter(ee$Filter$eq("LEVL_CODE", 1) )$filter(ee$Filter$neq("CNTR_CODE", "TR") )$filter(ee$Filter$neq("CNTR_CODE", "UA") )
  keys = nuts$first()$propertyNames()
  values = keys$map(   ee_utils_pyfunc(  function(k){  states$aggregate_array(k)} ) )
  dict = ee$Dictionary$fromLists(keys, values)

  # keys$getInfo()
  nutsID <- dict$get("NUTS_ID")$getInfo()
  nutsNames2 <- dict$get("NUTS_NAME")$getInfo()

  folders = googledrive::drive_ls("FIRE-RES-DATA", type="folder",
                                recursive=F)


  root <- "/archivio/shared/R/FIRE-RES/NUTS"
  options(googledrive_quiet = TRUE)

  local.folders <-  list.dirs(root, full.names = T, recursive = F)
  local.folders.name <- tools::file_path_sans_ext(basename(local.folders))


  files <-  list.files(root, full.names = T, recursive = T)
  files.name <- tools::file_path_sans_ext(files)
  files.name.noExtension <- tools::file_path_sans_ext(basename(files))


  ################ remove - careful!! -----
  rems <- c()
  namesl <- c()
  info = list()

  for(id in nutsID ){

    if(!is.element(id, local.folders.name)){

      message(id, " folder not there, creating it!")
      dir.create(path = file.path(root, id)  )

      goGEE(id)
    } else {

      message(id, "... folder exists, counting files there !")
      nfiles = length(list.files(pattern = "\\.tif$", file.path(root, id)  ))

      if(nfiles==9){
        message(id, "... All good!")
        next
      }

      if (nfiles == 0){
        goGEE(id)
      } else {
        message(id, "... Check here only ", nfiles, " files.")
        info[[id]] <- "Check HERE"
      }
    }




  }

  # remres <- googledrive::drive_rm( as_id(rems) )






## download the files -----

filesn <- list()
missing <- c()

folders = googledrive::drive_ls("FIRE-RES-DATA", type="folder",
                                recursive=F)

for(fold in  folders$name) { #folders$name
  locfolder <- file.path(root, fold)
  message(fold)
  if(!dir.exists(locfolder)){
    dir.create(locfolder)

    message("Creating ", fold)
  }

  # nf <- list.files(locfolder)
  # if(length(nf)==length(names(items))){
  #   message("all good... continuing")
  #   next
  # }

  filesn[[fold]] <- googledrive::drive_ls( sprintf("FIRE-RES-DATA/%s", fold))
  if(nrow(filesn[[fold]])==0){

    message("Nothing here!")
    missing<-c(missing, fold)
    next
  }
  filesn[[fold]]$modtime <- NULL
  filesn[[fold]]$modtime <- apply(filesn[[fold]], 1,function(x) {
    x$drive_resource$modifiedTime
    })

  filesn[[fold]]$note <- ""
  filesn[[fold]]$drive_resource<-NULL

  filesn[[fold]] <- filesn[[fold]] %>%
     arrange(desc(modtime)) %>% distinct(name, .keep_all = TRUE)

  # if(length(nf)>0){
  #   filestoDownload <- which(!(filesn[[fold]]$name %in% nf))
  # }   else{
  filestoDownload <- 1:nrow(filesn[[fold]])
    # }

  # googledrive::drive_ls("fuelModelScottBurgan.tif")

  if(nrow(filesn[[fold]])==0){
    message("No files here")
    filesn[[fold]][[row, "note"]] <- "no files here"
    next
  }

  for (row in filestoDownload){

    nm <- filesn[[fold]][[row, "name"]]
    message(nm)


    locfile <- file.path(locfolder, nm)

    # if(!file.exists(locfile)){

      googledrive::drive_download( file =  googledrive::as_id(filesn[[fold]][[row, "id"]]),
                                   path = locfile, overwrite = T )
      filesn[[fold]][[row, "note"]] <- "Downloaded"

    # } else {

      # filesn[[fold]][[row, "note"]] <- "Exists"

    # }

  }

}

### zip inside folders
###
###
folders <-  list.dirs(root, full.names = T)
for(fold in folders){
  if(fold==root) next
  ff <- list.files(fold, full.names = T)
  if(length(ff)>0){
    # bn <- strsplit(ff[[1]], split = "_")[[1]]
    of <- file.path(fold, paste(sep="", basename(fold),".zip"))
    if(file.exists(of)) next
    zip(of, files = ff )

  }
}
# sort(files.name)
# files.name.noExtension <- unlist(lapply(strsplit(files.name, "-"), function(X){X[[1]]}) )
