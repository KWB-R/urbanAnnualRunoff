#' abimo: compute climate
#' @description read Climate Engine data and compute (source:
#' https://app.climateengine.org/climateEngine)
#' @param rawdir rawdir
#' @param file_inp name of input file
#' @param file_out name of output file to be written in "raw_dir"
#' @param summer_month_start number of month where summer half year starts
#' (default: 4)
#' @param skip skip (default: 6)
#' @param sep sep (default: '')
#' @param dec dec (default: '.')
#' @return data frame with yearly summed measurements (summer half year,
#' total year sum) and also text file written to "raw_dir" with "out_file" name
#' @export
#'
#' @importFrom lubridate month year
#' @importFrom dplyr group_by summarize
#' @importFrom rlang .data
#' @importFrom utils read.table write.table
computeABIMOclimate <- function(rawdir,
                                file_inp,
                                file_out,
                                summer_month_start = 4,
                                skip = 6,
                                sep = '',
                                dec = '.'){

  # read data
  dat <- utils::read.table(file.path(rawdir, file_inp),
                    skip = skip,
                    sep = sep,
                    dec = dec,
                    colClasses = 'character',
                    header = FALSE,
                    col.names = c("date", "value"))

  # format columns
  dat$date <- as.Date(dat$date, format='%Y-%m-%d')
  dat$value <- as.numeric(dat$value)

  # add year, month and whether day is in summer
  dat$year <- lubridate::year(dat$date)
  dat$month <- lubridate::month(dat$date)

  summer_month_end <- as.numeric(summer_month_start) + 5
  dat$summer <- ifelse(dat$month >= summer_month_start & dat$month <= summer_month_end, 1, 0)

  data_points_per_year <- dat %>% dplyr::count(.data$year)

  months_per_year <- 12
  days_per_year <- 365:366
  required_data_points_per_year <- c(months_per_year, days_per_year)
  is_complete_year <- data_points_per_year$n %in% required_data_points_per_year

  complete_years <- data_points_per_year$year[is_complete_year]

  dat <- dat[dat$year %in% complete_years,]

  # compute annual total
  annualTot <- dat %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarize(sum_annual = sum(.data$value))

  # compute summer total per year
  summerTot <- dat %>%
    dplyr::filter(.data$summer==1) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarize(sum_summer=sum(.data$value))

  out <- dplyr::left_join(annualTot, summerTot, by = "year")

  # write output files
  utils::write.table( out, file.path(rawdir,  file_out), quote=FALSE)

  out
}
