# load scripts.
source('imgClass.R')
source('abimo.R')
source('climate.R')

`%>%` <- magrittr::`%>%` 

path_list <- list(
  root_path = "C:/kwb/projects/keys/Data-Work packages",
  site = "Beijing",
  data = "WP2_SUW_pollution_<site>",
  abimo = "<root_path>/<data>/_DataAnalysis/abimo",
  gis = "<root_path>/<data>/_DataAnalysis/gis",
  climate = "<root_path>/<data>/_DataAnalysis/climate"
)

paths <- kwb.utils::resolve(path_list)
#paths <- kwb.utils::resolve(path_list, site = "Jinxi")

# load shapefile containing subcatchments ('blockteilflächen')
abimo <- raster::shapefile(file.path(paths$gis, 'tz1shifted.shp'))

# make ABIMO 'CODE' column
abimo@data$CODE <- paste(abimo@data$Name, abimo@data$Outlet, sep='_')

# pad CODE with zeroes to match output of ABIMO
abimo@data$CODE <- padCODE(abimo@data$CODE)

# build classification model
buildClassMod(rawdir = paths$gis,
              image = 'tz.tif',
              groundTruth = 'groundtruth.shp',
              groundTruthValues = list('roof' = 1, 
                                       'street' = 2,
                                       'pervious' = 3,
                                       'shadow' = 4,
                                       'water' = 5),
              spectrSigName = 'spectrSigTz.Rdata',
              modelName = 'rForestTz.Rdata',
              overlayExists = FALSE,
              nCores = 2,
              mtryGrd = 1:3, ntreeGrd=seq(80, 150, by=10),
              nfolds = 3, nodesize = 3, cvrepeats = 2)

# check model performance
load(file.path(paths$gis,"rForestTz.Rdata"))
caret::confusionMatrix(data = model$finalModel$predicted, 
                       reference = model$trainingData$.outcome, 
                       mode = 'prec_recall')

# classify image for roofs and streets
predictSurfClass(rawdir = paths$gis,
                 modelName = 'rForestTz.Rdata',
                 image = 'tz.tif',
                 predName = 'tzClass.tif',
                 crsEPSG = '+init=EPSG:4586')

# make overlay object (list where each element is a vector of the pixel values 
# of the classified image for each subcatchment)
makeOverlay(rawdir = paths$gis,
            rasterData = 'tzClass.img',
            subcatchmSPobject = abimo,
            overlayName = 'surfType')

# make ABIMO variable FLGES
abimo@data$FLGES <- makeFLGES(subcatchmSPobject = abimo)

# compute ABIMO variable PROBAU (%roof)
abimo@data$PROBAU <- makePROBAU(rawdir = paths$gis,
                           rasterData = 'tzClass.tif',
                           overlayName = 'surfType',
                           targetValue = 1)

# compute ABIMO variable STR_FLGES (m² street area)
abimo@data$STR_FLGES <- makeSTR_FLGES(rawdir = paths$gis,
                                      subcatchmSPobject = abimo,
                                      mask = 'mask.shp',
                                      rasterData = 'tzClass.tif',
                                      overlayName = 'surfType',
                                      targetValue = 2)

# compute ABIMO variable VG (% soil sealing)
abimo@data$VG <- makeVG(rawdir = paths$gis,
                        subcatchmSPobject = abimo,
                        rasterData = 'impervProj.tif',
                        targetValue = 80)

# compute annual and summer rainfall 
computeABIMOclimate(rawdir = paths$climate,
                    fileName ='raw_climateeng_precipitation_daily_Beijing.txt',
                    skip = 6, sep = '', dec = '.',
                    outAnnual = 'precipitation_annual.txt',
                    outSummer ='precipitation_summer.txt')

# compute annual and summer ETP
computeABIMOclimate(rawdir = paths$climate,
                    fileName = 'raw_climateeng_etp_daily_Beijing.txt',
                    skip = 6, sep = '', dec = '.',
                    outAnnual = 'etp_annual.txt',
                    outSummer = 'etp_summer.txt')

# post-process ABIMO output file -> join it with input shape file for 
# visualization in GIS
postProcessABIMO(rawdir = paths$abimo,
                 nameABIMOin = 'ABIMO_Jinxi_v1.shp',
                 nameABIMOout = 'ABIMO_Jinxi_v1out.dbf',
                 ABIMOjoinedName = 'ABIMO_Jinxi_v1outJoined.dbf')

# use raw code to compute and allocate PROVGU and all other ABIMO variables
# raw code ------------------------------------------------------------------------------

abimo@data$CODE <- do_padding(abimo@data$CODE)

# % other impervious areas = total impervious % (VG, from global data 
# set) - %roof (PROBAU)
abimo@data$PROVGU <- ifelse((abimo@data$VG - abimo@data$PROBAU) > 0,
                            abimo@data$VG - abimo@data$PROBAU,
                           0)

# % imperviousness streets
abimo@data$VGSTRASSE <- 100

# %cover types in other imperv. areas (PROVGU)
abimo@data$BELAG1 <- 100
abimo@data$BELAG2 <- 0
abimo@data$BELAG3 <- 0
abimo@data$BELAG4 <- 0

# %cover types in street areas
abimo@data$STR_BELAG1 <- 100
abimo@data$STR_BELAG2 <- 0
abimo@data$STR_BELAG3 <- 0
abimo@data$STR_BELAG4 <- 0

# identifiers
abimo@data$BEZIRK <- 1
abimo@data$STAGEB <- 1
abimo@data$BLOCK <- 1
abimo@data$TEILBLOCK <- 1
abimo@data$NUTZUNG <- 21
abimo@data$TYP <- 21

abimo@data$REGENJA <- 800
abimo@data$REGENSO <- 500

abimo@data$KANAL<- 1
abimo@data$KAN_BEB<- 100
abimo@data$KAN_VGU<- 100
abimo@data$KAN_STR<- 100

# soil field capacity and groundwater level
abimo@data$FELD_30 <- 15
abimo@data$FELD_150 <- 15
abimo@data$FLUR <- 1

# select required columns and write out new shapefile
requiredCols <- c('CODE', 'BEZIRK', 'STAGEB', 'BLOCK', 'TEILBLOCK','NUTZUNG', 'TYP',
                  'FLGES', 'STR_FLGES',
                  'PROBAU', 'PROVGU', 'VGSTRASSE',
                  'BELAG1', 'BELAG2', 'BELAG3', 'BELAG4',
                  'STR_BELAG1', 'STR_BELAG2', 'STR_BELAG3', 'STR_BELAG4',
                  'KAN_BEB', 'KAN_VGU', 'KAN_STR',
                  'REGENJA', 'REGENSO',
                  'FELD_30', 'FELD_150', 'FLUR')
abimo@data <- abimo@data[, requiredCols]

# # change decimal separator to point
abimo@data <- as.data.frame(apply(X=apply(X=abimo@data,
                                         c(1, 2),
                                         FUN=as.character),
                                 c(1, 2),
                                 FUN=gsub,
                                 pattern="\\.",
                                 replacement=","),
                           stringsAsFactors = FALSE)

# write ABIMO input table
raster::shapefile(x=abimo, filename='../ABIMO/ABIMO_Jinxi_v1.shp', overwrite=TRUE)

