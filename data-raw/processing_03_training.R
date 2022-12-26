library(ggplot2)
library(data.table)
library(foreach)
library(h2o)
 library(doParallel);


## data processing ----

if(!dir.exists("data-raw/tileData") ){
  stop("You are missing the folder with data - please read/run DATA.R to download missing data!")
}

test.csv <- read.csv("data-raw/testing.csv" )
train1.csv <- read.csv("data-raw/training.csv" )
train2.csv <- read.csv("data-raw/training2.csv" )


cs <- makeCluster(12)
registerDoParallel(cs)
coords<-numeric(nrow(train.csv)*2)
coords <- foreach(i=1:nrow(train.csv), .combine = c) %dopar% {
 cc<- jsonify::from_json(train.csv$.geo[[i]])
 cc$coordinates
 }
 coordinates <- t(matrix(coords, nrow=2))
 sf::write_sf(sf::st_as_sf(as.data.frame(coordinates), coords=c(1,2), crs=4326), "train.gpkg")
doParallel::stopImplicitCluster()

# coords.test<-numeric(nrow(test.csv)*2)
# coords.test <- foreach(i=1:nrow(test.csv), .combine = c) %dopar% {
#  cc<- jsonify::from_json(test.csv$.geo[[i]])
#  cc$coordinates
# }
# coordinates.test <- t(matrix(coords.test, nrow=2))

test.csv$system.index<-NULL
torm<-which(duplicated(test.csv$.geo))
test.csv$.geo<-NULL
test.csv<-data.table(na.omit(test.csv[-torm,]))
test.csv$forest_type<-as.factor(test.csv$forest_type)
test.csv$discrete_classification<-as.factor(test.csv$discrete_classification)

train.csv <- rbind(train1.csv, train2.csv)

torm<-which(duplicated(train.csv$.geo))
train.csv$.geo<-NULL
train.csv$system.index<-NULL
train.csv<-data.table(na.omit(train.csv[-torm,]))
train.csv$forest_type<-as.factor(train.csv$forest_type)
train.csv$discrete_classification<-as.factor(train.csv$discrete_classification)



train.csv.summary <- train.csv[ , .(Mean=mean(biomass), n=length(biomass)) , by=forest_type ]
### some data analysis

# ggplot(data=train.csv) + geom_hex(aes(biomass, canopy_height_sum))


features.bioclim<-terra::rast("data-raw/features_bioclim.tif")
names(features.rest)
features.rest<-terra::rast("data-raw/features_100m.tif")

train.from.image <- terra::extract(features.rest, coordinates)
train.from.image2 <- terra::extract(features.bioclim, coordinates)
train.from.image.final <- cbind(train.from.image, train.from.image2)
train.from.image.final$discrete_classification<- as.factor(train.from.image.final$discrete_classification)
train.from.image.final$forest_type<- as.factor(train.from.image.final$forest_type)

test.from.image <- terra::extract(features.rest, coordinates.test)
test.from.image2 <- terra::extract(features.bioclim, coordinates.test)
test.from.image.final <- cbind(test.from.image, test.from.image2)
test.from.image.final$discrete_classification<- as.factor(test.from.image.final$discrete_classification)
test.from.image.final$forest_type<- as.factor(test.from.image.final$forest_type)


library(h2o)
h2o.init()

df <- h2o::as.h2o(train.from.image.final)
test <- h2o::as.h2o(test.from.image.final)


h2o.describe(df)
y <- "biomass"



aml2 <- h2o.automl(y = y,
                   training_frame = df,
                  # max_runtime_secs = 60,
                   seed = 2,
                  max_models = 30,
                   project_name = "biomass_prediction_image")




# Get the best XGBoost model using default sort metric
xgb <- h2o.get_best_model(aml2, algorithm = "xgboost")


drf <- h2o.get_best_model(aml2, algorithm = "DRF")
png("man/images/variable_importance.png", width=1200, height=600, res=120)
par(mfrow=c(1,2))
h2o.varimp_plot(drf)
h2o.varimp_plot(xgb)
dev.off()

perf <- h2o.performance(aml2@leader)
h2o.saveModel(object = aml2@leader, path = "models/bm_spain", force = TRUE)
mod <- h2o.loadModel("models/bm_spain/StackedEnsemble_AllModels_1_AutoML_1_20220802_191956")
pred <- h2o.predict(mod, test)
perf <- h2o.performance(mod, test)
hist(as.vector(pred)-test.csv$biomass, breaks=100)
df<-data.frame(predicted=as.data.frame(pred)[,1],
               observed=as.data.frame(test)[,y])

png("man/images/variables.png", width=500, height=500)
  smoothScatter(df,
                xlab="Predicted (Mg/ha)", ylab="Observed (Mg/ha)",
                pch=21, main="Fit on Independent Test Data",
                sub=sprintf("R-squared=%.2f, RMSE=%.0f Mg/ha", perf@metrics$r2, perf@metrics$RMSE),
                col="white",
                bg="red",
                colramp = colorRampPalette(c( viridis::turbo(12))) )
dev.off()


jpeg("man/images/variables.jpg", width=1500, height=4500, res=200)
par(mar = c(4.1, 4.1, 1.1, 0.1))
par(mfrow=c(9,3))
for(i in names(train.csv)){
  if(i=="biomass") next
  smoothScatter(y=train.csv$biomass,
                x=train.csv[[i]],
                xlab=toupper(i), ylab="Biomass (Mg/ha)",
                pch=21,
                col="white",
                bg="red",
                colramp = colorRampPalette(c( viridis::turbo(12))) )
}
dev.off()


