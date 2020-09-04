# image segmentation with random forest based on spectral signatures

# build image classification model. overlay object is named 'ov'
buildClassMod <- function(rawdir, image, groundTruth, spectrSigName, modelName,
                          overlayExists, nCores, 
                          mtryGrd, ntreeGrd,
                          nfolds, nodesize, cvrepeats){
  
  library(raster)
  library(caret)
  library(doParallel)
  
  # set working directory
  setwd(rawdir)
  
  # load image and ground truth data
  cat('\nloading spatial data...')
  img <- raster::brick(image)
  ground <- raster::shapefile(groundTruth)
  cat('\ndone\n')
  
  if(overlayExists){
    
    cat('\nreading overlay object with spectral signatures of groundtruth areas...')
    load('spectrSigJinxi.Rdata')
    cat('\ndone\n')
    
  } else {
    
    # extract spectral values of ground truth areas
    cat('\nextracting spectral signatures of ground truth areas...')
    ov <- raster::extract(x=img, y=ground)
    cat('\ndone\n')
    
    # save resulting list containing overlay (ov)
    if(!is.na(spectrSigName)) 
      cat('\nsaving overlay object...')
      save(ov, file=spectrSigName)
    cat('\ndone\n')
  }
  
  # convert ov into data.frame for each surface type in ground truth data
  coverTypes <- unique(ground@data$cover)
  pos <- lapply(X=coverTypes, FUN=function(x) which(ground@data$cover == x))
  Xi <- lapply(X=pos, FUN=function(x) do.call(rbind, ov[x]))
  X <- data.frame(do.call(what=rbind, args=Xi), 
                  cover=rep(coverTypes, times=unlist(lapply(Xi, nrow))))
  X$cover <- as.factor(X$cover)
  
  # train random forest model, repeated cross-validation, grid search (mtry),
  # parallel processing
  set.seed(1)

  cl <- makePSOCKcluster(nCores)
  registerDoParallel(cl)
  
  train.control <- caret::trainControl(method="repeatedcv", 
                                       number=nfolds, 
                                       repeats=cvrepeats,
                                       index=caret::createFolds(y=X$cover,
                                                                k=nfolds,
                                                                returnTrain=FALSE,
                                                                list=TRUE),
                                       search='grid',
                                       allowParallel=TRUE)
  cat('\ntraining model...')
  model <- caret::train(form=cover ~., 
                        data=X, 
                        method="rf",
                        tunegrid=expand.grid(mtry=mtryGrd,
                                             ntree=ntreeGrd),
                        nodesize=nodesize,
                        trControl=train.control)
  
  stopCluster(cl)
  cat('\ndone\n')
  
  # save model
  cat('\nsaving model...')
  save(model, file=modelName)
  cat('\ndone\n')
}

# apply model to predict surface type (roof, street, ...)
predictSurfClass <- function(rawdir, modelName, image, predName){
  
  library(raster)
  library(caret)
  
  # set working directory
  setwd(rawdir)
  
  # load model object
  cat('\nloading model...')
  load(modelName)
  cat('\ndone\n')
  
  # load image to be classified
  img <- raster::brick(image)
  
  # predict
  cat('\nmaking predictions...')
  pred <- raster::predict(object=img, 
                          model=model$finalModel, 
                          type='class')
  cat('\ndone\n')

  # write predictions as raster
  cat('\nwriting output...')
  raster::writeRaster(pred, filename=predName, overwrite=TRUE)
  cat('\ndone\n')
}
