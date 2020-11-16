# spatial overlay of subcatchments and raster holding information required by ABIMO
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

# pad CODE column of ABIMO table
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

# compute ABIMO variable FLGES
makeFLGES <- function(subcatchmSPobject){
  
  sapply(
    X = seq_len(length(subcatchmSPobject)),
    FUN = function(i){
      return(subcatchmSPobject@polygons[[i]]@Polygons[[1]]@area)
    })
}

# compute ABIMO variable PROBAU
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

# compute ABIMO variable STR_FLGES
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

# compute ABIMO variable VG (% soil sealing), based on online global land use data
# set from tsinghua univ. http://data.ess.tsinghua.edu.cn/fromglc10_2017v01.html
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

# read dbf results file, join with input shapefile and write output shapefile 
# including ABIMO results
postProcessABIMO <- function(rawdir, nameABIMOin, 
                             nameABIMOout, ABIMOjoinedName){

  # read files
  ABIMOin <- raster::shapefile(x=file.path(rawdir, nameABIMOin), stringsAsFactors=FALSE)
  ABIMOout <- foreign::read.dbf(file=file.path(rawdir, nameABIMOout), as.is=TRUE)
  
  # temporarily store CODE in vector and pad with zeroes to match
  codetemporary <- ABIMOin@data$CODE
  
  # pad CODE in subcatchment data with zeroes to match ABIMOout$CODE
  nchi <- nchar(codetemporary)
  nchmax <- max(nchi)
  codetemporary <- sapply(X=codetemporary, FUN=function(a){
    npad <- nchmax - nchar(a)
    paste0(paste(rep(0, times=npad), collapse=''), a)
  }, USE.NAMES=FALSE)
  
  # change decimal separator from comma to point
  ABIMOin@data <- as.data.frame(apply(X=apply(X=ABIMOin@data,
                                              c(1, 2),
                                              FUN=gsub,
                                              pattern=',',
                                              replacement='.'),
                                      c(1, 2),
                                      FUN=as.numeric),
                                stringsAsFactors = FALSE)
  
  # set CODE in subcatchment data to padded CODE
  ABIMOin$CODE <- codetemporary
  
  # join 
  ABIMOjoined <- ABIMOin
  ABIMOjoined@data <- ABIMOin@data %>%
    dplyr::left_join(ABIMOout, by='CODE')
  
  # write out joined table output as shapefile
  raster::shapefile(x=ABIMOjoined, filename=file.path(rawdir, ABIMOjoinedName), overwrite=TRUE)
}
