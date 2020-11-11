# image segmentation with random forest based on spectral signatures

# build image classification model. overlay object is named 'ov'
buildClassMod <- function(rawdir, image, groundTruth,
                          groundTruthValues,
                          overlayExists, spectrSigName, 
                          modelName, nCores, mtryGrd, ntreeGrd,
                          nfolds, nodesize, cvrepeats){
  
  # Wrapper function for fitting a random forest using a multi-band image with the purpose of
  # classifying pixels into roof, street, and pervious (green areas, water surface), etc. These 
  # categories are defined by the user in the ground truth data.
  # 
  # Training and testing are done using repeated cross-validation with package caret
  # 
  # rawdir: path to directory containing the image to be classified and the ground truth data.
  # image: The image to be classified. Supported formats are given in the raster package's brick
  #        function.
  # groundTruth: shapefile containing polygons indicating the observed classes of a sample of 
  #              pixels. These classes must be contained in a column named 'cover' in the shape-
  #              file's attribute table. The table may contain further columns.
  # overlayExists: if FALSE, the function overlays the ground truth data and the image (time 
  #                consuming) and saves an R object containing the former's spectral signatures
  #                with name spectrSigName (overlay object). If TRUE, the function will skip this
  #                overlay operation and read an existing overlay object with name 'spectrSigName'.
  # spectrSigName: File name of overlay object, either for saving a new or load an existing file.
  # modelName: File name for saving the fitted random forest model
  # nCores: no. of cores for running in parallel mode (uses library 'doParallel')
  # mtryGrd, ntreeGrd, nodesize: grids for randomForest::randomForest parameters mtry and ntree.
  #                              For nodesize, a single value (not included in grid search).
  # nfolds cvrepeats: number of folds and repeats in repeated cross validation (caret)
  
  library(raster)
  library(caret)
  library(doParallel)
  
  # set working directory
  setwd(rawdir)
  
  # load image and ground truth data
  cat('\nloading spatial data...')
  img <- raster::brick(image)
  ground <- raster::shapefile(groundTruth)
  
  ground@data$cover <- kwb.utils::multiSubstitute(
    ground@data$cover, groundTruthValues)
  
  cat('\ndone\n')
  
  if(overlayExists){
    
    cat('\nreading overlay object with spectral signatures of groundtruth areas...')
    load(spectrSigName)
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
