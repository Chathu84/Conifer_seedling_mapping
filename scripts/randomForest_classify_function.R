# Random Forest image classification
# Adapted from http://gis.stackexchange.com/a/57786/12899

#' Performs a random forest classification of the specified raster.
#'
#' @param rasterPath  The full path to the raster file to be classified.
#' @param samplesPath The full path to the .SHP file of the training samples.
#' @param samplesCol  The column in the training samples shape file that contains the class names.
#' @param classifiedPath The full path to the output classified image.
#'
#' @author Franz Alex Gaisie-Essilfie
#' 
#' 
#' 
#' 
#' 

wd = "D:/Carbon_dynamics/UAS-processing/Hm_box1/training"
setwd(wd)


randomForest.classify <- function(rasterPath, samplesPath,
                                  samplesCol="CLASS", classifiedPath=NA){
  # Install the required packages that aren't already installed
  toInstall <- setdiff(c("randomForest", "sp", "rgdal", "raster", "tcltk"),
                       rownames(installed.packages()))
  if (length(toInstall)) install.packages(toInstall)
  
  # Add required libraries
  require(randomForest)
  require(sp)
  require(rgdal)
  require(raster)
  
  # Ensure we have a valid path to the training samples
  if (is.na(samplesPath) || !file.exists(samplesPath)){
    shapeFileFilter <- matrix(c("All Files", "*.*", "Shape Files", "*.shp"), ncol=2, byrow=T)
    samplesPath <- tcltk::tk_choose.files(caption="Select training samples shape file",
                                          filters=shapeFileFilter, multi=F)
  }
  if (length(samplesPath) == 0 || !file.exists(samplesPath))
    stop("Could not locate training samples.")
  
  
  # Load the training samples
  samples <- readOGR(dirname(samplesPath),
                     tools::file_path_sans_ext(basename(samplesPath)))
  
  
  # Ensure we have a valid path to the unclassified image
  if (is.na(rasterPath) || !file.exists(rasterPath)){
    rasterFilter <- matrix(c("All Files", "*.*",
                             "TIFF Image", "*.tif",
                             "ERDAS IMAGINE", "*.img"), ncol=2, byrow=T)
    rasterPath <- tcltk::tk_choose.files(caption="Select unclassified image",
                                         filters=rasterFilter, multi=F)
  }
  if (length(rasterPath) == 0 || !file.exists(rasterPath))
    stop("Could not locate raster file for classification.")
  
  # Read the image to be classified
  multispectral <- stack(rasterPath)
  
  # Use sample points to extract raster signatures
  samples@data <- data.frame(samples@data, extract(multispectral, samples))
  
  # Create RandomForest model
  rf.mdl <- randomForest(x=samples@data[, names(multispectral)],
                         y=samples@data[, samplesCol],
                         ntree=501, proximity=T, importancce=T)
  
  
  # Ensure we have a valid path to the classified image
  if (is.na(classifiedPath) || file.exists(classifiedPath)){
    rasterFilter <- paste('{"All Files" {"*.*"}}',
                          '{"TIFF Image" {"*.tif"}} ',
                          '{"ERDAS IMAGINE" {"*.img"}}',
                          sep=" ")
    default <- file.path(dirname(rasterPath),
                         paste(tools::file_path_sans_ext(basename(rasterPath)),
                               "classified", tools::file_ext(rasterPath), sep="."))
    classifiedPath <- tcltk::tkgetSaveFile(title="Save classified image as",
                                           initialfile=default,
                                           filetypes=rasterFilter)
    if (nchar(classifiedPath <- as.character(classifiedPath)) == 0)
      stop("No raster file selected for saving classified image.")
  }
  
  # Predict classified raster
  predict(multispectral, rf.mdl, filename=classifiedPath, type="response",
          na.rm=T, overwrite=T, progress="text", datatype="INT2U")
}


# Packages for spatial data processing & visualization
library(rgdal)
library(tidyverse)
# library(gdalUtils)
library(raster)
library(sf)
library(sp)
# library(RStoolbox)
# library(getSpatialData)
# library(rasterVis)
library(mapview)

library(RColorBrewer)
library(plotly)
library(grDevices)

# Machine learning packages
library(caret)
library(randomForest)
library(ranger)
library(MLmetrics)
library(nnet)
library(NeuralNetTools)
library(LiblineaR)

# Packages for general data processing and parallel computation
library(data.table)
library(dplyr)
library(stringr)
library(doParallel)
library(snow)
library(parallel)



wd = "D:/Carbon_dynamics/UAS-processing/Hm_box1/training"
setwd(wd)


input =  stack(paste0(wd,"/","VI/Hm_box1/08-25-2022/","hm_box1_8-25-2022-6-9all_layers.tif"))


chm = stack("chm_resampled.tif")

input = stack(input,chm)

#writeRaster(input,"all_with_chm_test.tif")


training = read.csv(paste0(wd,"/","training_data_with_params.csv"))
training = na.omit(training[,-c(1:3)])

# Histograms of predictors
training %>% 
  select(-"Type") %>% 
  melt(measure.vars = names(.)) %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  geom_vline(xintercept = 0, color = "gray70") +
  facet_wrap(facets = vars(variable), ncol = 3)

#Split into train and test
set.seed(321)
# A stratified random split of the data
idx_train <- createDataPartition(training$Type,
                                 p = 0.7, # percentage of data as training
                                 list = FALSE)
dt_train <- training[idx_train,]
dt_test <- training[-idx_train,]

table(dt_train$Type)

#Fit models

n_folds <- 10
set.seed(321)
folds <- createFolds(1:nrow(dt_train), k = n_folds)
# Set the seed at each resampling iteration. Useful when running CV in parallel.
seeds <- vector(mode = "list", length = n_folds + 1) # +1 for the final model
for(i in 1:n_folds) seeds[[i]] <- sample.int(1000, n_folds)
seeds[n_folds + 1] <- sample.int(1000, 1) # seed for the final model


ctrl <- trainControl(summaryFunction = multiClassSummary,
                     method = "cv",
                     number = n_folds,
                     search = "grid",
                     classProbs = TRUE, # not implemented for SVM; will just get a warning
                     savePredictions = TRUE,
                     index = folds,
                     seeds = seeds)

#random forest

# Register a doParallel cluster, using 3/4 (75%) of total CPU-s

library(Rmpi)

cl <- makeCluster(3/4 * detectCores())

registerDoParallel(cl)

model_rf <- caret::train(Type ~ . , method = "rf", data = dt_train,
                         importance = TRUE, # passed to randomForest()
                         # run CV process in parallel;
                         # see https://stackoverflow.com/a/44774591/5193830
                         allowParallel = TRUE,
                         tuneGrid = data.frame(mtry = c(2, 3, 4, 5, 8)),
                         trControl = ctrl)
stopCluster(cl); remove(cl)
# Unregister the doParallel cluster so that we can use sequential operations
# if needed; details at https://stackoverflow.com/a/25110203/5193830
registerDoSEQ()
saveRDS(model_rf, file = "model_rf.rds")

#performance
model_rf$times$everything

plot(model_rf)

#confusion metrix

cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         as.factor(dt_test$Type))
cm_rf


model_rf$finalModel

# Predictor importance

caret::varImp(model_rf)$importance %>%
  as.matrix %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)


randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

randomForest::varImpPlot(model_rf$finalModel)

#############SVM############################################


# Grid of tuning parameters
svm_grid <- expand.grid(cost = c(0.2, 0.5, 1),
                        Loss = c("L1", "L2"))

# cl <- makeCluster(3/4 * detectCores())
# registerDoParallel(cl)
model_svm <- caret::train(Type ~ . , method = "svmLinear3", data = dt_train,
                          allowParallel = TRUE,
                          tuneGrid = svm_grid,
                          trControl = ctrl)


# stopCluster(cl); remove(cl)
registerDoSEQ()
# Warning message:
# In train.default(x, y, weights = w, ...) :
#   Class probabilities were requested for a model that does not implement them
# (see why above)
saveRDS(model_svm, file = "model_svm.rds")


# Model summary & confusion matrix

model_svm$times$everything # total computation time

plot(model_svm) # tuning results

# The confusion matrix using the test dataset
cm_svm <- confusionMatrix(data = predict(model_svm, newdata = dt_test),
                          as.factor(dt_test$Type))
cm_svm



##################Neural Network#####################################
# Grid of tuning parameters
nnet_grid <- expand.grid(size = c(5, 10, 15),
                         decay = c(0.001, 0.01, 0.1))

# cl <- makeCluster(3/4 * detectCores())
# registerDoParallel(cl)
model_nnet <- train(Type ~ ., method = 'nnet', data = dt_train,
                    importance = TRUE,
                    maxit = 1000, # set high enough so to be sure that it converges
                    allowParallel = TRUE,
                    tuneGrid = nnet_grid,
                    trControl = ctrl)


# stopCluster(cl); remove(cl)


registerDoSEQ()
saveRDS(model_nnet, file = "model_nnet.rds")


# Model summary & confusion matrix

model_nnet$times$everything # total computation time

plot(model_nnet) # tuning results


# The confusion matrix using the test dataset
cm_nnet <- confusionMatrix(data = predict(model_nnet, newdata = dt_test),
                           as.factor(dt_test$Type))
cm_nnet

cols <- grDevices::colorRampPalette(colors = brewer.pal(n = 9, name = "YlGnBu"))(10)

dev.new(height=4, width=4)
garson(model_nnet) +
  scale_y_continuous('Rel. Importance') + 
  scale_fill_gradientn(colours = cols) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
dev.new(height=4, width=4)
cols_rank_import <- cols[rank(garson(model_nnet, bar_plot = FALSE)$rel_imp)]
plotnet(model_nnet, circle_col = list(cols_rank_import, 'lightblue'))



###Compare models###################

# Create model_list
model_list <- list(rf = model_rf, svm = model_svm, nnet = model_nnet)
# Pass model_list to resamples()
resamples <- caret::resamples(model_list)

# All metrics with boxplots
bwplot(resamples)


#paired t test to compare model performances
t_tests <- resamples %>%
  diff(metric = "Accuracy") %>%
  summary
t_tests


###Visualize classifications############



brick_input = brick(input)


system.time({
  predict_rf <- raster::predict(object = brick_input, overwrite = TRUE,
                                model = model_rf, type = 'raw',filename="predict_rf.tif")
  predict_svm <- raster::predict(object = brick_input,overwrite = TRUE,
                                 model = model_svm, type = 'raw',filename="predict_svm.tif")
  predict_nnet <- raster::predict(object = brick_input,overwrite = TRUE,
                                  model = model_nnet, type = 'raw',filename="predict_nnet.tif")
})

pred_rf = raster(predict_rf)

writeRaster(pred_rf,"predict_rf.tif")

sync(viewRGB(brick(rst_crop_lst[1:3]), r = 3, g = 2, b = 1) +
       mapView(poly, zcol = "class", col.regions = cls_dt$hex),
     mapView(predict_rf, col.regions = cls_dt$hex), 
     mapView(predict_svm, col.regions = cls_dt$hex),
     mapView(predict_nnet, col.regions = cls_dt$hex))

dev.new(height=4, width=4)
mapView(predict_rf)

dev.new(height=4, width=4)
mapView(predict_svm)

dev.new(height=4, width=4)
mapView(predict_nnet)

plot(predict_rf)


input2 =  stack(paste0(wd,"/","testing/texture/no_date/","Heyman-10-05-22-8-1_all_layers.tif"))


chm2 = stack(paste0(wd,"/","testing/texture/no_date/","CHM_resampled.tif"))

input2 = stack(input2,chm2)

brick_input2 = brick(input2)

system.time({
  predict_rf2 <- raster::predict(object = brick_input2, overwrite = TRUE,
                                model = model_rf, type = 'raw',filename="predict_rf2.tif")
  predict_svm2 <- raster::predict(object = brick_input2,overwrite = TRUE,
                                 model = model_svm, type = 'raw',filename="predict_svm2.tif")
  predict_nnet2 <- raster::predict(object = brick_input2,overwrite = TRUE,
                                  model = model_nnet, type = 'raw',filename="predict_nnet2.tif")
})





###########################
#############################
###############################
####################################

training = na.omit(training[,-c(1:3,42)])

# Histograms of predictors
training %>% 
  select(-"Type") %>% 
  melt(measure.vars = names(.)) %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  geom_vline(xintercept = 0, color = "gray70") +
  facet_wrap(facets = vars(variable), ncol = 3)

#Split into train and test
set.seed(321)
# A stratified random split of the data
idx_train <- createDataPartition(training$Type,
                                 p = 0.7, # percentage of data as training
                                 list = FALSE)
dt_train <- training[idx_train,]
dt_test <- training[-idx_train,]

table(dt_train$Type)

#Fit models

n_folds <- 10
set.seed(321)
folds <- createFolds(1:nrow(dt_train), k = n_folds)
# Set the seed at each resampling iteration. Useful when running CV in parallel.
seeds <- vector(mode = "list", length = n_folds + 1) # +1 for the final model
for(i in 1:n_folds) seeds[[i]] <- sample.int(1000, n_folds)
seeds[n_folds + 1] <- sample.int(1000, 1) # seed for the final model


ctrl <- trainControl(summaryFunction = multiClassSummary,
                     method = "cv",
                     number = n_folds,
                     search = "grid",
                     classProbs = TRUE, # not implemented for SVM; will just get a warning
                     savePredictions = TRUE,
                     index = folds,
                     seeds = seeds)

#random forest

# Register a doParallel cluster, using 3/4 (75%) of total CPU-s

library(Rmpi)

cl <- makeCluster(3/4 * detectCores())

registerDoParallel(cl)

model_rf <- caret::train(Type ~ . , method = "rf", data = dt_train,
                         importance = TRUE, # passed to randomForest()
                         # run CV process in parallel;
                         # see https://stackoverflow.com/a/44774591/5193830
                         allowParallel = TRUE,
                         tuneGrid = data.frame(mtry = c(2, 3, 4, 5, 8)),
                         trControl = ctrl)
stopCluster(cl); remove(cl)
# Unregister the doParallel cluster so that we can use sequential operations
# if needed; details at https://stackoverflow.com/a/25110203/5193830
registerDoSEQ()
saveRDS(model_rf, file = "model_rf_no_h.rds")

#performance
model_rf$times$everything

plot(model_rf)

#confusion metrix

cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         as.factor(dt_test$Type))
cm_rf


model_rf$finalModel

# Predictor importance

caret::varImp(model_rf)$importance %>%
  as.matrix %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)


randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

randomForest::varImpPlot(model_rf$finalModel)

#############SVM############################################


# Grid of tuning parameters
svm_grid <- expand.grid(cost = c(0.2, 0.5, 1),
                        Loss = c("L1", "L2"))

# cl <- makeCluster(3/4 * detectCores())
# registerDoParallel(cl)
model_svm <- caret::train(Type ~ . , method = "svmLinear3", data = dt_train,
                          allowParallel = TRUE,
                          tuneGrid = svm_grid,
                          trControl = ctrl)


# stopCluster(cl); remove(cl)
registerDoSEQ()
# Warning message:
# In train.default(x, y, weights = w, ...) :
#   Class probabilities were requested for a model that does not implement them
# (see why above)
saveRDS(model_svm, file = "model_svm_no_h.rds")


# Model summary & confusion matrix

model_svm$times$everything # total computation time

plot(model_svm) # tuning results

# The confusion matrix using the test dataset
cm_svm <- confusionMatrix(data = predict(model_svm, newdata = dt_test),
                          as.factor(dt_test$Type))
cm_svm



##################Neural Network#####################################
# Grid of tuning parameters
nnet_grid <- expand.grid(size = c(5, 10, 15),
                         decay = c(0.001, 0.01, 0.1))

# cl <- makeCluster(3/4 * detectCores())
# registerDoParallel(cl)
model_nnet <- train(Type ~ ., method = 'nnet', data = dt_train,
                    importance = TRUE,
                    maxit = 1000, # set high enough so to be sure that it converges
                    allowParallel = TRUE,
                    tuneGrid = nnet_grid,
                    trControl = ctrl)


# stopCluster(cl); remove(cl)


registerDoSEQ()
saveRDS(model_nnet, file = "model_nnet_no_h.rds")


# Model summary & confusion matrix

model_nnet$times$everything # total computation time

plot(model_nnet) # tuning results


# The confusion matrix using the test dataset
cm_nnet <- confusionMatrix(data = predict(model_nnet, newdata = dt_test),
                           as.factor(dt_test$Type))
cm_nnet

cols <- grDevices::colorRampPalette(colors = brewer.pal(n = 9, name = "YlGnBu"))(10)

dev.new(height=4, width=4)
garson(model_nnet) +
  scale_y_continuous('Rel. Importance') + 
  scale_fill_gradientn(colours = cols) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dev.new(height=4, width=4)
cols_rank_import <- cols[rank(garson(model_nnet, bar_plot = FALSE)$rel_imp)]
plotnet(model_nnet, circle_col = list(cols_rank_import, 'lightblue'))



###Compare models###################

# Create model_list
model_list <- list(rf = model_rf, svm = model_svm, nnet = model_nnet)
# Pass model_list to resamples()
resamples <- caret::resamples(model_list)

# All metrics with boxplots
bwplot(resamples)


#paired t test to compare model performances
t_tests <- resamples %>%
  diff(metric = "Accuracy") %>%
  summary
t_tests


###Visualize classifications############



brick_input = brick(input)


system.time({
  predict_rf <- raster::predict(object = brick_input, overwrite = TRUE,
                                model = model_rf, type = 'raw',filename="predict_rf_no_h.tif")
  predict_svm <- raster::predict(object = brick_input,overwrite = TRUE,
                                 model = model_svm, type = 'raw',filename="predict_svm_no_h.tif")
  predict_nnet <- raster::predict(object = brick_input,overwrite = TRUE,
                                  model = model_nnet, type = 'raw',filename="predict_nnet_no_h.tif")
})

# pred_rf = raster(predict_rf)
# 
# writeRaster(pred_rf,"predict_rf.tif")

sync(viewRGB(brick(rst_crop_lst[1:3]), r = 3, g = 2, b = 1) +
       mapView(poly, zcol = "class", col.regions = cls_dt$hex),
     mapView(predict_rf, col.regions = cls_dt$hex), 
     mapView(predict_svm, col.regions = cls_dt$hex),
     mapView(predict_nnet, col.regions = cls_dt$hex))

dev.new(height=4, width=4)
mapView(predict_rf)

dev.new(height=4, width=4)
mapView(predict_svm)

dev.new(height=4, width=4)
mapView(predict_nnet)

plot(predict_rf)


input2 =  stack(paste0(wd,"/","testing/texture/no_date/","Heyman-10-05-22-8-1_all_layers.tif"))


chm2 = stack(paste0(wd,"/","testing/texture/no_date/","CHM_resampled.tif"))

input2 = stack(input2,chm2)

brick_input2 = brick(input2)

system.time({
  predict_rf2 <- raster::predict(object = brick_input2, overwrite = TRUE,
                                 model = model_rf, type = 'raw',filename="predict_rf2_no_h.tif")
  predict_svm2 <- raster::predict(object = brick_input2,overwrite = TRUE,
                                  model = model_svm, type = 'raw',filename="predict_svm2_no_h.tif")
  predict_nnet2 <- raster::predict(object = brick_input2,overwrite = TRUE,
                                   model = model_nnet, type = 'raw',filename="predict_nnet2_no_h.tif")
})


















mapView(predict_rf2)

writeRaster(predict_nnet,"predict_nnet.tif",overwrite=TRUE)



# Prepare colors for each class.

training$id <- as.integer(factor(training$Type))


cls_dt <- unique(training$Type) %>% 
  arrange(training$id) %>% 
  mutate(hex = c(Dead_wood  = "#ff7f00",
                 Ground = "#e41a1c",
                 Herbs       = "#4daf4a",
                 Live_trees      = "#984ea3",
                 Rocks        = "#377eb8",
                 Seedling = "#F8766D",
                 Shadows = "#C77CFF",
                 Shrubs = "#00BFC4"))




# poly <- rgdal::readOGR(dsn   = "./data/train_polys", 
#                        layer = "train_polys", 
#                        stringsAsFactors = FALSE)
# # Need to have a numeric id for each class - helps with rasterization later on.
# poly@data$id <- as.integer(factor(poly@data$class))
# setDT(poly@data)


# #setwd("C:/data/")   # set the working directory to the directory where the data is stored
# randomForest.classify("all_with_chm_test.tif",    # file name of the unclassified image
#                       "Training_all_classes.shp",  # point shape file of the locations of the training samples
#                       "Type",           # the column in the training samples shape file which contain the class names
#                       "classifiedImage.tif"   # file name of the classified image
# )
