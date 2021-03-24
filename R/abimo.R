#' spatial overlay of subcatchments and raster holding information required by ABIMO
#'
#' @param rawdir Path to data directory.
#' @param rasterData Name of raster file containing classified image.
#' @param subcatchmSPobject Spatial dataset containing subcatchment polygons
#' (ABIMO Blockteilflächen) (sp object type, R package sp).
#' @param overlayName Name of output overlay object.
#' @param subcatchmNamesCol Name of column in the attribute table of
#' subcatchmSPobject that contains the subcatchment identifiers. This
#' is used for naming the elements of the resulting list
#' @return save overlay as .Rdata in directory "rawdir" with filename defined in
#' @export
#' @importFrom raster extract raster
makeOverlay <- function(rawdir, rasterData, subcatchmSPobject,
                        overlayName, subcatchmNamesCol){

  # load data
  surf <- raster::raster(file.path(rawdir, rasterData))

  cat('\noverlaying data...')
  assign(x=overlayName, value=raster::extract(x=surf, y=subcatchmSPobject))
  names(surfType) <- subcatchmSPobject@data[, subcatchmNamesCol]

  cat('\nsaving overlay...')
  save(list=overlayName, file=file.path(rawdir, paste0(overlayName, '.Rdata')))
  cat('\ndone\n')
}


#' helper function: pad CODE column of ABIMO table
#'
#' @param string string with CODE identifier
#'
#' @return padded CODE identifier (with leading "0" depending of maximium character
#' length)
#' @export

padCODE <- function(string) {
  nchi <- nchar(string)
  nchmax <- max(nchi)

  return(
  sapply(X=string, FUN=function(a){
    npad <- nchmax - nchar(a)
    paste0(paste(rep(0, times=npad), collapse=''), a)
  }, USE.NAMES=FALSE)
  )
}

#' compute ABIMO variable FLGES (block area without street area)
#'
#' @param subcatchmSPobject subcatchmSPobject
#'
#' @return ???
#' @export
#'
makeFLGES <- function(subcatchmSPobject){

  sapply(
    X = seq_len(length(subcatchmSPobject)),
    FUN = function(i){
      return(subcatchmSPobject@polygons[[i]]@Polygons[[1]]@area)
    })
}

#' compute ABIMO variable PROBAU (covered sealed area)
#'
#' @param rawdir rawdir
#' @param rasterData rasterData
#' @param overlayName overlayName
#' @param targetValue targetValue
#'
#' @return ???
#' @export
#'
#' @importFrom raster raster res
makePROBAU <- function(rawdir, rasterData, overlayName, targetValue){

  # load data
  cat('\nreading classified image...')
  surf <- raster::raster(file.path(rawdir, rasterData))

  # grab overlay object
  cat('\nreading overlay object...')
  load(file.path(rawdir, paste0(overlayName, '.Rdata')))
  cat('\ndone\n')

  # compute raster cell area
  rasterRes <- raster::res(surf)
  cellsize <- rasterRes[1]*rasterRes[2]

  # count pixels with target cover type (target value)
  count <- sapply(X=get(overlayName), FUN=function(a) sum(a==targetValue))
  ncellsubc <- sapply(X=get(overlayName), FUN=length)

  # compute % coverage
  res <- 100*count/ncellsubc

  # return results
  return(res)
}


#' compute ABIMO variable STR_FLGES (street area of block area)
#'
#' @param rawdir rawdir
#' @param subcatchmSPobject subcatchmSPobject
#' @param rasterData rasterData
#' @param overlayName overlayName
#' @param targetValue targetValue
#' @param mask mask
#'
#' @return STR_FLGES
#' @export
#'
#' @importFrom  raster mask shapefile raster res
makeSTR_FLGES <- function(rawdir, subcatchmSPobject, rasterData,
                          overlayName, targetValue, mask){


  # load data
  cat('\nreading mask and raster data...')
  surf <- raster::raster(file.path(rawdir, rasterData))
  mask <- raster::shapefile(file.path(rawdir, mask))
  cat('\ndone\n')

  # grab overlay object
  cat('\nreading overlay object...')
  load(file.path(rawdir, paste0(overlayName, '.Rdata')))
  cat('\ndone\n')

  # make cells outside mask = NA
  cat('\nmasking cells outside study area...')
  surf2 <- raster::mask(surf, mask)
  cat('\ndone\n')

  # compute raster cell area
  rasterRes <- raster::res(surf)
  cellsize <- rasterRes[1]*rasterRes[2]

  # compute internal street area (streets within subcatchment polygons)
  streetAreaIn <- as.numeric(sapply(X=get(overlayName),
                                    FUN=function(a) sum(a==targetValue)*cellsize))

  # as is done for Berlin, street area outside of the subcatchment polygons is distributed
  # among the polygons in proportion to their area. thus:
  # street area of polygon = internal street area + allocated external street area

  # total street area in study area
  sa <- surf2[surf2==targetValue]
  sa <- sa[!is.na(sa)]
  streetAreaTot <- length(sa)*cellsize

  # street area outside subcatchment polygons = streetAreaTot - streetAreaIn
  streetAreaOut <- streetAreaTot - sum(streetAreaIn)

  # distribute streetAreaOut among subcatchments in proportion to their area
  STR_FLGES <- streetAreaIn +
    streetAreaOut*subcatchmSPobject@data$FLGES/sum(subcatchmSPobject@data$FLGES)

  return(STR_FLGES)
}


#' compute ABIMO variable VG (soil sealing percentage)
#' @description based on online global land use data
# set from tsinghua univ. http://data.ess.tsinghua.edu.cn/fromglc10_2017v01.html
#' @param rawdir rawdir
#' @param subcatchmSPobject subcatchmSPobject
#' @param rasterData rasterData
#' @param targetValue targetValue
#'
#' @return ???
#' @export
#'
#' @importFrom raster extract
makeVG <- function(rawdir,
                   subcatchmSPobject,
                   rasterData,
                   targetValue){

  # load raster data
  cat('\nreading raster data...')
  surf <- raster::raster(file.path(rawdir, rasterData))
  cat('\ndone')

  # overlay with subcatchments
  cat('\noverlaying raster and subcatchments...')
  vg <- raster::extract(x=surf, y=subcatchmSPobject)
  cat('\ndone')

  # count pixels with target cover type (target value)
  cat('\ncomputing VG...')
  count <- sapply(X=vg, FUN=function(a) sum(a==targetValue))
  ncellsubc <- sapply(X=vg, FUN=length)

  # compute % coverage
  vg <- 100*count/ncellsubc
  cat('\ndone\n')

  # return results
  return(vg)
}

# read dbf results file, join with input shapefile
#' abimo: postprocess
#' @description read dbf results file and joins with input shapefile
#' @param path_input path of ABIMO input shapefile
#' @param path_output path of ABIMO output DBF file
#' @return joined SpatialPolygonsDataFrame with ABIMO input and
#' output
#' @export
#'
#' @importFrom foreign read.dbf
#' @importFrom raster shapefile
#' @importFrom dplyr left_join
#' @importFrom magrittr "%>%"
#'
postProcessABIMO <- function(path_input,
                             path_output){

  # read files
  ABIMOin <- raster::shapefile(x = path_input, stringsAsFactors=FALSE)
  ABIMOout <- foreign::read.dbf(file = path_output, as.is=TRUE)

  # join
  ABIMOjoined <- ABIMOin
  ABIMOjoined@data <- ABIMOin@data %>%
    dplyr::left_join(ABIMOout, by='CODE')

  ABIMOjoined
}
