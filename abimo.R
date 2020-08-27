rawdir='Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP2_SUW_pollution_Jinxi/_DataAnalysis/GIS'

makeOverlay(rawdir='c:/kwb/KEYS/WP2_SUW_pollution_Jinxi/_DataAnalysis/GIS/',
            rasterData='jxClass.img', 
            subcatchmShape='ABIMO_Jinxi_5.shp',
            overlayName='surfType')

computeABIMOcolumn(rawdir='c:/kwb/KEYS/WP2_SUW_pollution_Jinxi/_DataAnalysis/GIS/', 
                   subcatchmShape='ABIMO_Jinxi_5.shp',
                   rasterData='jxClass.img',
                   overlayName='surfType',
                   targetValue=2,
                   outDFname='probauJx.txt')



# % other impervious areas = total impervious % (VG, from global data set) - %roof (PROBAU)
subc@data$PROVGU <- ifelse((subc@data$VG - subc@data$PROBAU) > 0,
                           subc@data$VG - subc@data$PROBAU,
                           0)

# % imperviousness streets
subc@data$VGSTRASSE <- 100

# %cover types in other imperv. areas (PROVGU)
subc@data$BELAG1 <- 100
subc@data$BELAG2 <- 0
subc@data$BELAG3 <- 0
subc@data$BELAG4 <- 0

# %cover types in street areas
subc@data$STR_BELAG1 <- 100
subc@data$STR_BELAG2 <- 0
subc@data$STR_BELAG3 <- 0
subc@data$STR_BELAG4 <- 0

# identifiers
#subc@data$CODE <- formatC(x=subc@data$BSM, format='d', width = 5, flag = '0')
subc@data$BEZIRK <- 1
subc@data$STAGEB <- 1
subc@data$BLOCK <- 1
subc@data$TEILBLOCK <- 1
subc@data$NUTZUNG <- 21
subc@data$TYP <- 21

subc@data$REGENJA <- 800
subc@data$REGENSO <- 500

subc@data$KANAL<- 1
subc@data$KAN_BEB<- 100
subc@data$KAN_VGU<- 100
subc@data$KAN_STR<- 100

subc@data$FELD_30 <- 15
subc@data$FELD_150 <- 15
subc@data$FLUR <- 1

# select required columns and write out new shapefile
requiredCols <- c('CODE', 'BEZIRK', 'STAGEB', 'BLOCK', 'TEILBLOCK','NUTZUNG', 'TYP',
                  'FLGES', 'STR_FLGES',
                  'PROBAU', 'PROVGU', 'VGSTRASSE',
                  'BELAG1', 'BELAG2', 'BELAG3', 'BELAG4',
                  'STR_BELAG1', 'STR_BELAG2', 'STR_BELAG3', 'STR_BELAG4',
                  'KAN_BEB', 'KAN_VGU', 'KAN_STR',
                  'REGENJA', 'REGENSO',
                  'FELD_30', 'FELD_150', 'FLUR')
subc@data <- subc@data[, requiredCols]

# # change decimal separator to point
subc@data <- as.data.frame(apply(X=apply(X=subc@data,
                                         c(1, 2),
                                         FUN=as.character),
                                 c(1, 2),
                                 FUN=gsub,
                                 pattern="\\.",
                                 replacement=","),
                           stringsAsFactors = FALSE)

# write ABIMO input table
raster::shapefile(x=subc, filename='../ABIMO/ABIMO_Jinxi_v1.shp', overwrite=TRUE)


setwd('c:/kwb/KEYS/WP2_SUW_pollution_Jinxi/_DataHIT/')
soil <- readxl::read_xls(path='translate_soil_data.xls',
                         sheet = "111",
                         skip = 1,
                         col_names=T,
                         na=c("","na"),
                         trim_ws = TRUE,
                         .name_repair = "minimal")

soil$`Irrigated Conditions`
soil$Drainage
t(t(unique(soil$`Texture Configuration`)))
"轻壤" -> 'light soil'
"中壤" -> 'middle soil'
"重壤" -> 'heavy earth'
"砂质轻壤" -> 'sandy light soil'
"粘质中壤" -> 'clay medium soil'
"砂质中壤" -> 'sandy medium soil'
"砂壤" -> 'sand soil'
"轻粘土" -> 'light clay'

# spatial overlay of subcatchments and raster holding information required by ABIMO -----
makeOverlay <- function(rawdir, rasterData, subcatchmShape, overlayName){
  
  require(raster)
  
  setwd(rawdir)
  
  # load data  
  subc <- raster::shapefile(subcatchmShape, stringsAsFactors=FALSE)
  surf <- raster::raster(rasterData)

  cat('\noverlaying data...')
  assign(x=overlayName, value=raster::extract(x=surf, y=subc))
  cat('\nsaving overlay...')
  save(list=overlayName, file=paste0(overlayName, '.Rdata'))
  cat('\ndone\n')
}

# compute area and %coverage of specific cover type in each subcatchment -----------------
computeABIMOcolumn <- function(rawdir, subcatchmShape, rasterData,
                               overlayName, targetValue, outDFname){
  
  setwd(rawdir)

  # load data  
  subc <- raster::shapefile(subcatchmShape, stringsAsFactors=FALSE)
  surf <- raster::raster(rasterData)
  
  # grab overlay object
  cat('\nreading existing overlay...')
  load(paste0(overlayName, '.Rdata'), envir=.GlobalEnv)
  cat('\ndone\n')
  
  # compute cell area
  rasterRes <- raster::res(surf)
  cellsize <- rasterRes[1]*rasterRes[2]
  
  # count pixels with target cover type (target value)
  count <- sapply(X=get(overlayName, envir=.GlobalEnv), FUN=function(a) sum(a==targetValue))
  ncellsubc <- sapply(X=get(overlayName, envir=.GlobalEnv), FUN=length)
  
  # make output data.frame
  cat('\nbuilding output data.frame')
  out <- data.frame(CODE=subc@data$CODE,
                    area=cellsize*count,
                    perc_cover=100*count/ncellsubc)
  cat('\ndone\n')
  
  # write output file
  cat('\nwriting output data.frame')
  write.table(out, file=outDFname, quote=FALSE, sep=';', row.names=FALSE)
  cat('\ndone\n')
}

# compute area  of street areas ----------------------------------------------------------
makeABIMOstreet <- function(rawdir, sourceRaster, targetValue, 
                            subcatchmShape, overlayName, outDFname,
                            existingOverlay){
  
  # as is done for Berlin, street area outside of the subcatchment polygons is distributed
  # among the polygons in proportion to their area. thus: 
  # street area of polygon = internal street area + allocated external street area 

  require(raster)
  
  setwd(rawdir)
  
  # load data  
  subc <- raster::shapefile(subcatchmShape, stringsAsFactors=FALSE)
  surf <- raster::raster(sourceRaster)
  
  
  cat('\nreading existing overlay...')
  load(overlayName)
  cat('\ndone\n')
  
  
  
  cat('\noverlaying data...')
  ov <- raster::extract(x=surf, y=subc)
  cat('\nsaving overlay...')
  save(ov, file=overlayName)
  cat('\ndone\n')
  
  
  
  # street area within subcatchment polygons
  streetAreaIn <- sapply(X=ov,
                           FUN=function(a){
                             length(a[a==4])}) # 4 = street
  
  # total street area in study area
  streetAreaTot <- length(surf[surf==4])
  
  # difference = street area outside subcatchment polygons
  streetAreaOut <- streetAreaTot - sum(streetAreaIn)
  
  
  subc@data$STR_FLGES <- formatC(streetAreaIn + 
                                   streetAreaOut*subc@data$FLGES/sum(subc@data$FLGES),
                                 format = 'd')
  
  
  
  
  
}




# read dbf results file, join with input shapefile and write final table -----------------
postProcessABIMO <- function(rawdir, ABIMOin, ABIMOout, ABIMOjoinedName){

  library(dplyr)
  library(raster)
  library(foreign)

  setwd(rawdir)
  
  # read files
  ABIMOin <- raster::shapefile(x=ABIMOin, stringsAsFactors=FALSE)
  ABIMOout <- foreign::read.dbf(file=ABIMOout, as.is=TRUE)
  
  # join 
  ABIMOjoined <- ABIMOin
  ABIMOjoined@data <- ABIMOin@data %>%
    dplyr::left_join(ABIMOout, by='CODE')
  
  # write out joined table output as shapefile
  raster::shapefile(x=ABIMOjoined, filename=ABIMOjoinedName, overwrite=TRUE)
}



