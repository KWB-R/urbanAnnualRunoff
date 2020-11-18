#' abimo: compute climate
#' @description read Climate Engine data and compute (source:
#' https://app.climateengine.org/climateEngine)
#' @param rawdir rawdir
#' @param fileName fileName
#' @param skip skip
#' @param sep sep
#' @param dec dec
#' @param outAnnual outAnnual
#' @param outSummer outSummer
#'
#' @return ???
#' @export
#'
#' @importFrom lubridate month year
#' @importFrom dplyr group_by summarize
#' @importFrom rlang .data
#' @importFrom utils read.table write.table
computeABIMOclimate <- function(rawdir,
                                fileName,
                                skip,
                                sep,
                                dec,
                                outAnnual, outSummer){

  # read data
  dat <- utils::read.table(file.path(rawdir, fileName),
                    skip = skip,
                    sep = sep,
                    dec = dec,
                    colClasses = 'character',
                    header = FALSE)

  # format columns
  dat[[1]] <- as.Date(dat[[1]], format='%Y-%m-%d')
  dat[[2]] <- as.numeric(dat[[2]])

  # add year, month and whether day is in summer
  dat$year <- lubridate::year(dat[[1]])
  dat$month <- lubridate::month(dat[[1]])
  dat$summer <- ifelse(dat$month >=4 & dat$month <=9, 1, 0)

  # compute annual total
  annualTot <- dat %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarize(annual = sum(.data$V2))

  # compute summer total per year
  summerTot <- dat %>%
    dplyr::filter(.data$summer==1) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarize(summer=sum(.data$V2))

  # write output files
  utils::write.table(annualTot, file.path(rawdir, outAnnual), quote=FALSE)
  utils::write.table(summerTot, file.path(rawdir, outSummer), quote=FALSE)

  cat('\ncheck output files for incomplete years (unusually low annual totals)!\nremove them when computing multiannual average')
}
