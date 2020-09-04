# read Climate Engine data and compute 
# source: https://app.climateengine.org/climateEngine

computeABIMOclimate <- function(rawdir, 
                                fileName, 
                                header,
                                outAnnual, outSummer){
  library(lubridate)
  library(dplyr)
  
  setwd(rawdir)
  
  # read data
  dat <- read.table(fileName, 
                    sep=' ', 
                    colClasses='character',
                    col.names=header,
                    skip=7)
  
  # format columns
  dat[[1]] <- as.Date(dat[[1]], format='%Y-%m-%d')
  dat[[2]] <- as.numeric(dat[[2]])

  # add year, month and whether day is in summer
  dat$year <- lubridate::year(dat$date)
  dat$month <- lubridate::month(dat$date)
  dat$summer <- ifelse(dat$month >=4 & dat$month <=9, 1, 0)
  
  # compute annual total
  annTot <- dat %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(annualETP=sum(ETP))
  
  # compute summer total per year
  summTot <- dat %>%
    dplyr::filter(summer==1) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(summerETP=sum(ETP))
  
  # write output files
  write.table(annTot, outAnnual, quote=FALSE)
  write.table(summTot, outSummer, quote=FALSE)
}
