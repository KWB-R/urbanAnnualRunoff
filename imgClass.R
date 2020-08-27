# automatic classification of roofs and streets:
buildClassMod(dataPath='Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP2_SUW_pollution_Beijing/_DataAnalysis/GIS/',
              image='tz.tif',
              groundTruth='groundtruth2.shp', # column name of surface type in groundTruth must be 'cover'
              spectrSigName='spectrSigTz.Rdata',
              modelName='rForestTz.Rdata',
              nCores=5)

predictSurfClass(dataPath='Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP2_SUW_pollution_Beijing/_DataAnalysis/GIS/',
                 modelName='rForestTz.Rdata',
                 image='tz.tif',
                 predName='tzClass.img')

# build image classification model
buildClassMod <- function(dataPath, image, groundTruth, spectrSigName, modelName,
                          nCores){
  
  library(raster)
  library(caret)
  library(doParallel)
  
  # set working directory
  setwd(dataPath)
  
  # load image and ground truth data
  img <- raster::brick(image)
  ground <- raster::shapefile(groundTruth)
  
  # extract spectral values of ground truth areas
  cat('\nextracting spectral signatures of ground truth areas...')
  ov <- raster::extract(x=img, y=ground)
  cat('\ndone\n')
  
  # save resulting list containing overlay (ov)
  if(!is.na(spectrSigName)) 
    save(ov, file=spectrSigName)
  
  # convert ov into data.frame for each surface type in ground truth data
  coverTypes <- unique(ground@data$cover)
  pos <- lapply(X=coverTypes, FUN=function(x) which(ground@data$cover == x))
  Xi <- lapply(X=pos, FUN=function(x) do.call(rbind, ov[x]))
  X <- data.frame(do.call(what=rbind, args=Xi), 
                  cover=rep(coverTypes, times=unlist(lapply(Xi, nrow))))
  X$cover <- as.factor(X$cover)
  
  # train random forest model, repeated cross-validation:
  
  # make training and test sets (stratified)
  set.seed(1)
  indexTrain <- caret::createDataPartition(y=X$cover,
                                           times=1,
                                           p=0.75)[[1]]
  Xtrain <- X[indexTrain, ]
  Xtest <- X[-indexTrain, ]
  
  # train model, parallel processing
  cl <- makePSOCKcluster(nCores)
  registerDoParallel(cl)
  train.control <- caret::trainControl(method="repeatedcv", 
                                       number=3, 
                                       repeats=2,
                                       index=caret::createFolds(factor(Xtrain$cover), 
                                                                returnTrain=T),
                                       search='grid',
                                       allowParallel=TRUE)
  cat('\ntraining model...')
  model <- caret::train(form=cover ~., 
                        data=Xtrain, 
                        method="rf",
                        ntree=300,
                        nodesize=10,
                        tuneLength=3,
                        trControl=train.control)
  
  stopCluster(cl)
  cat('\ndone\n')
  
  # check prediction quality on test set, focusing on roofs
  Ypred <- raster::predict(model$finalModel, Xtest, type='class')
  print(caret::confusionMatrix(data=Ypred, reference=Xtest$cover, mode='prec_recall'))
  
  # save model
  cat('\nsaving model...')
  save(model, file=modelName)
  cat('\ndone\n')
}

# predict
predictSurfClass <- function(dataPath, modelName, image, predName){
  
  library(raster)
  library(caret)
  
  # set working directory
  setwd(dataPath)
  
  # load model object
  cat('\nloading model...')
  load(modelName)
  cat('\ndone\n')
  
  # load image to be classified
  img <- raster::brick(image)
  
  # predict
  cat('\nmaking predictions...')
  pred <- raster::predict(img, model$finalModel, type='class')
  cat('\ndone\n')
  
  # write predictions as raster
  cat('\nwriting output...')
  raster::writeRaster(pred, filename=predName, overwrite=TRUE)
  cat('\ndone\n')
}

