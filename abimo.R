# spatial overlay of subcatchments and raster holding information required by ABIMO
makeOverlay <- function(rawdir, rasterData, subcatchmShape, 
                        overlayName, subcatchmNamesCol){
  
  # This function ca be used to overlay subcatchment polygons (shapefile) with two
  # types of raster datasets 
  # - imperviousness: (e.g., http://data.ess.tsinghua.edu.cn/fromglc10_2017v01.html)
  # - classified image containing roof and street pixels
  
  library(raster)
  
  setwd(rawdir)
  
  # load data  
  subc <- raster::shapefile(subcatchmShape, stringsAsFactors=FALSE)
  surf <- raster::raster(rasterData)

  cat('\noverlaying data...')
  assign(x=overlayName, value=raster::extract(x=surf, y=subc))
  names(surfType) <- subc@data[, subcatchmNamesCol]
  
  cat('\nsaving overlay...')
  save(list=overlayName, file=paste0(overlayName, '.Rdata'))
  cat('\ndone\n')
}

# compute ABIMO variables, either PROBAU (%roof), VG (%impervious) or STR_FLGES (street
# area) in each subcatchment based on existing overlay
computeABIMOvariable <- function(rawdir, subcatchmShape, rasterData,
                                 overlayName, targetValue, outDFname,
                                 mask, street){
  
  # This function can be used to generate the data required for following ABIMO columns:
  # - VG: % imperviousness
  # - PROBAU: % roof area
  # - STR_FLGES: street area
  # It reads an overlay object produced by makeOverlay and uses pixel sizes to compute
  # the corresponding areas, as well as %coverage based on subcatchment areas
  
  # Street areas are allocated to each subcatchment based on their size. Since there
  # may be internal and external street area (if the subcatchment polygons have gaps
  # between them), the function first computes the internal street area and then 
  # allocates the external street area to each subcatchment based on its size (as is
  # done in Berlin)
  
  library(raster)
  
  setwd(rawdir)
  
  # load data  
  cat('\nreading mask, subcatchments and raster data...')
  subc <- raster::shapefile(subcatchmShape, stringsAsFactors=FALSE)
  surf <- raster::raster(rasterData)
  mask <- raster::shapefile(mask)
  
  # pad CODE in in subcatchment data with zeroes
  nchi <- nchar(subc@data$CODE)
  nchmax <- max(nchi)
  npad = nchmax - nchi + 1
  for(i in 1:length(npad)){
    subc@data$CODE[i] <- paste0(paste(rep(0, times=npad[i]), collapse=''), 
                               subc@data$CODE[i])
  }
  cat('\ndone\n')
  
  # grab overlay object
  cat('\nreading overlay object...')
  load(paste0(overlayName, '.Rdata'))
  cat('\ndone\n')
  
  # compute raster cell area
  rasterRes <- raster::res(surf)
  cellsize <- rasterRes[1]*rasterRes[2]
  
  # routine for street area
  if(street){
    
    # change decimal separator from comma to point in subcatchment data and convert to
    # numbers
    subc@data$FLGES <- as.numeric(gsub(pattern=',', 
                                       replacement='.', 
                                       x=subc@data$FLGES))
    
    # as is done for Berlin, street area outside of the subcatchment polygons is distributed
    # among the polygons in proportion to their area. thus: 
    # street area of polygon = internal street area + allocated external street area 
    
    # make cells outside mask = NA
    cat('\nmasking cells outside study area...')
    surf2 <- raster::mask(surf, mask)
    cat('\ndone\n')
    
    # street area within subcatchment polygons
    streetAreaIn <- sapply(X=get(overlayName),
                           FUN=function(a) sum(a==targetValue)*cellsize)
    
    # total street area in study area
    sa <- surf2[surf2==targetValue]
    sa <- sa[!is.na(sa)]
    streetAreaTot <- length(sa)*cellsize
    
    # street area outside subcatchment polygons = streetAreaTot - streetAreaIn
    streetAreaOut <- streetAreaTot - sum(streetAreaIn)
    
    # distribute streetAreaOut among subcatchments in proportion to their area
    STR_FLGES <- formatC(streetAreaIn + 
                           streetAreaOut*subc@data$FLGES/sum(subc@data$FLGES),
                         format='d')
    
    # make output data.frame
    cat('\nbuilding output data.frame')
    out <- data.frame(CODE=subc@data$CODE,
                      STR_FLGES=STR_FLGES)
    cat('\ndone\n')
    
  } else {
    
    # count pixels with target cover type (target value)
    count <- sapply(X=get(overlayName), FUN=function(a) sum(a==targetValue))
    ncellsubc <- sapply(X=get(overlayName), FUN=length)
    
    # make output data.frame
    cat('\nbuilding output data.frame')
    out <- data.frame(CODE=subc@data$CODE,
                      area=cellsize*count,
                      perc_cover=100*count/ncellsubc)
    cat('\ndone\n')
  }
  
  # write output file
  cat('\nwriting output data.frame')
  write.table(out, file=outDFname, quote=FALSE, sep=';', row.names=FALSE)
  cat('\ndone\n')
}

# read dbf results file, join with input shapefile and write output shapefile including
# ABIMO results
postProcessABIMO <- function(rawdir, nameABIMOin, nameABIMOout, ABIMOjoinedName){

  library(dplyr)
  library(raster)
  library(foreign)

  setwd(rawdir)
  
  # read files
  ABIMOin <- raster::shapefile(x=nameABIMOin, stringsAsFactors=FALSE)
  ABIMOout <- foreign::read.dbf(file=nameABIMOout, as.is=TRUE)
  
  # change decimal separator from comma to point
  ABIMOin@data <- as.data.frame(apply(X=apply(X=ABIMOin@data,
                                              c(1, 2),
                                              FUN=gsub,
                                              pattern=',',
                                              replacement='.'),
                                      c(1, 2),
                                      FUN=as.numeric),
                                stringsAsFactors = FALSE)
  
  # pad CODE in ABIMOin with zeroes to match CODE in ABIMOout
  ABIMOin$CODE <- formatC(x=ABIMOin$CODE, 
                          width=nchar(ABIMOout$CODE)[1],
                          flag=0)
  
  # join 
  ABIMOjoined <- ABIMOin
  ABIMOjoined@data <- ABIMOin@data %>%
    dplyr::left_join(ABIMOout, by='CODE')
  
  # write out joined table output as shapefile
  raster::shapefile(x=ABIMOjoined, filename=ABIMOjoinedName, overwrite=TRUE)
}

