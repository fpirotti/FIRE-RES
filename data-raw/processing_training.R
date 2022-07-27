library(ggplot2)
## data processing ----

if(!file.exists("data-raw/samples4biomassML.csv")){
  stop("Please read/run DATA.R to download missing data")
}

train.csv <- read.csv("data-raw/samples4biomassML.csv" )
train.csv$.geo<-NULL
train.csv$system.index<-NULL
train.csv<-na.omit(train.csv)
### some data analysis

ggplot(data=train.csv) + geom_hex(aes(biomass, canopy_height_sum))




library(h2o)
h2o.init()

df <- h2o::as.h2o(train.csv)
h2o.describe(df)
y <- "biomass"
splits <- h2o.splitFrame(df, ratios = 0.8, seed = 1)
train <- splits[[1]]
test <- splits[[2]]

aml2 <- h2o.automl(y = y,
                   training_frame = df,
                  # max_runtime_secs = 60,
                   seed = 1,
                   project_name = "biomass_prediction_full_data")



# pred <- h2o.predict(aml2, test)  # predict(aml, test) and h2o.predict(aml@leader, test) also work
# head(pred)


perf <- h2o.performance(aml2@leader, test)
perf


h2o.saveModel(object = aml2@leader, path = "models/bm_spain", force = TRUE)
pred <- h2o.predict(aml2, df)

