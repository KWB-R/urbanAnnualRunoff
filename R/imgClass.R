# image segmentation with random forest based on spectral signatures

#' build image classification model (random forest)
#' @description Wrapper function for fitting a random forest using a multi-band
#' image with the purpose of classifying pixels into roof, street, and pervious
#' (green areas, water surface), etc. These categories are defined by the user
#' in the ground truth data. Training and testing are done using repeated
#' cross-validation with package caret
#' @param rawdir path to directory containing the image to be classified and the
#' ground truth data.
#' @param image The image to be classified. Supported formats are given in the
#' raster package's brick function.
#' @param groundTruth shapefile containing polygons indicating the observed
#' classes of a sample of pixels. These classes must be contained in a column
#' named 'cover' in the shape-file's attribute table. The table may contain
#' further columns.
#' @param groundTruthValues list with key value pairs (default: list('roof' = 1,
#' 'street' = 2, 'pervious' = 3, 'shadow' = 4, 'water' = 5))
#' @param overlayExists if FALSE, the function overlays the ground truth data
#' and the image (time consuming) and saves an R object containing the former's
#' spectral signatures with name spectrSigName (overlay object). If TRUE, the
#' function will skip this overlay operation and read an existing overlay object
#' with name 'spectrSigName'.(default: FALSE)
#' @param spectrSigName File name of overlay object, either for saving a new or
#' load an existing file.
#' @param modelName File name for saving the fitted random forest model
#' @param nCores  no. of cores for running in parallel mode (uses library
#' 'doParallel'), (default: parallel::detectCores() - 1)
#' @param mtryGrd Number of trees to grow. In the random forests literature, this
#' is referred to as the ntree parameter. Larger number of trees produce more
#' stable models and covariate importance estimates, but require more memory and
#' a longer run time. For small datasets, 50 trees may be sufficient. For larger
#' datasets, 500 or more may be required. Please consult the random forests
#' literature for extensive discussion of this parameter (e.g. Cutler et al., 2007;
#' Strobl et al., 2007; Strobl et al., 2008).
#' @param ntreeGrd Number of variables available for splitting at each tree node.
#' In the random forests literature, this is referred to as the mtry parameter.
#' There is extensive discussion in the literature about the influence of mtry.
#' Cutler et al. (2007) reported that different values of mtry did not affect
#' the correct classification rates of their model and that other performance
#' metrics (sensitivity, specificity, kappa, and ROC AUC) were stable under
#' different values of mtry. On the other hand, Strobl et al. (2008) reported
#' that mtry had a strong influence on predictor variable importance estimates.
#' @param nfolds number of folds in repeated cross validation (caret),
#' (default: 3)
#' @param nodesize a single value (not included in grid search), (default: 3)
#' @param cvrepeats number of repeats in repeated cross validation (caret),
#' (default: 2)
#'
#' @return writes a lot of output files to different folders ??? (details!)
#' @export
#' @importFrom caret createFolds train trainControl
#' @importFrom doParallel registerDoParallel
#' @importFrom kwb.utils multiSubstitute
#' @importFrom parallel makePSOCKcluster stopCluster
#' @importFrom raster brick extract shapefile
#'
buildClassMod <- function(rawdir,
                          image,
                          groundTruth,
                          groundTruthValues = list('roof' = 1,
                                                   'street' = 2,
                                                   'pervious' = 3,
                                                   'shadow' = 4,
                                                   'water' = 5),
                          overlayExists = FALSE,
                          spectrSigName,
                          modelName,
                          nCores = parallel::detectCores() - 1,
                          mtryGrd,
                          ntreeGrd,
                          nfolds = 3,
                          nodesize = 3,
                          cvrepeats = 2){

  # load image and ground truth data
  cat('\nloading spatial data...')
  img <- raster::brick(file.path(rawdir, image))
  ground <- raster::shapefile(file.path(rawdir, groundTruth))

  ground@data$cover <- kwb.utils::multiSubstitute(
    ground@data$cover, groundTruthValues)

  cat('\ndone\n')

  if(overlayExists){

    cat('\nreading overlay object with spectral signatures of groundtruth areas...')
    load(file.path(rawdir, spectrSigName))
    cat('\ndone\n')

  } else {

    # extract spectral values of ground truth areas
    cat('\nextracting spectral signatures of ground truth areas...')
    ov <- raster::extract(x=img, y=ground)
    cat('\ndone\n')

    # save resulting list containing overlay (ov)
    if(!is.na(spectrSigName))
      cat('\nsaving overlay object...')
      save(ov, file=file.path(rawdir, spectrSigName))
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

  cl <- parallel::makePSOCKcluster(nCores)
  doParallel::registerDoParallel(cl)

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

  parallel::stopCluster(cl)
  cat('\ndone\n')

  # save model
  cat('\nsaving model...')
  save(model, file=file.path(paths$gis, modelName))
  cat('\ndone\n')
}

# apply model to predict surface type (roof, street, ...)
#' apply model to predict surface type (roof, street, ...)
#'
#' @param rawdir path to raw data directory
#' @param modelName modelName
#' @param image image
#' @param predName name of prediction file
#'
#' @return write raster in "rawdir" with file name "predName"
#' @export
#' @importFrom raster brick predict writeRaster
#'
predictSurfClass <- function(rawdir, modelName, image, predName){

  # load model object
  cat('\nloading model...')
  load(file.path(rawdir, modelName))
  cat('\ndone\n')

  # load image to be classified
  img <- raster::brick(file.path(rawdir, image))

    # predict
  cat('\nmaking predictions...')
  pred <- raster::predict(object=img,
                          model=model$finalModel,
                          type='class')
  cat('\ndone\n')

  # write predictions as raster
  cat('\nwriting output...')
  raster::writeRaster(pred, filename=file.path(rawdir, predName), overwrite=TRUE)
  cat('\ndone\n')
}
