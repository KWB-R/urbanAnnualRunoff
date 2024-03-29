#' Emissions: read concentrations from OgRe database
#' @description imports data from OgRe database and selects relevant substances
#' for case study sites (Beijing, Jinxi) and calculates mean concentrations
#' over all structures (column: "mean"). In addition new columns (short_name,
#' unit_load, label_load) are created
#' @param path path to OgRe database file "annual_mean_conc.csv"
#' @return data frame with selected substances and column
#' @export
#' @importFrom kwb.utils multiSubstitute substSpecialChars
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @importFrom dplyr across c_across rowwise across
#' @importFrom tidyselect all_of
#'
read_concentrations <- function(path) {

  # grab AMC from OgRe
  x_conc <- read.table(file = path,
                       sep = ";",
                       dec = ".",
                       stringsAsFactors = FALSE,
                       header = TRUE)

  structures <- c("ALT", "NEU", "STR", "EFH", "GEW", "ANDERE")

  x_conc[x_conc$UnitsAbbreviation == "mg/L",] <-
    x_conc[x_conc$UnitsAbbreviation == "mg/L",] %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::all_of(structures),
                                ~ .x / 1000))

  x_conc[x_conc$UnitsAbbreviation == "ug/L",] <-
    x_conc[x_conc$UnitsAbbreviation == "ug/L",] %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::all_of(structures),
                                ~ .x / 1000000))



  x_conc$UnitsAbbreviation <- kwb.utils::multiSubstitute(
    strings = x_conc$UnitsAbbreviation,
    replacements = list("mg/L" = "kg/m3",
                        "ug/L" = "kg/m3"))

  ### select substances of interest
  shortnames_list <- list("Biologischer Sauerstoffbedarf" = "BOD" ,
                          "Chemischer Sauerstoffbedarf" = "COD",
                          "Abfiltrierbare Stoffe" = "TSS",
                          "Blei$" = "Pb",
                          "Cadmium$" = "Cd",
                          "Chrom$" = "Cr",
                          "Kupfer$" = "Cu",
                          "Nickel$" = "Ni",
                          "Titan$" = "Ti",
                          "Vanadium$" = "Va",
                          "Zink$" = "Zn")

  x_conc$short_name <- kwb.utils::multiSubstitute(
    strings = x_conc$VariableName,
    replacements = shortnames_list)

  short_names <- as.character(unlist(shortnames_list))

  # average across catchment types
  concentrations  <- x_conc %>%
    dplyr::filter(.data$short_name %in% short_names) %>%
    dplyr::mutate(unit_load = kwb.utils::substSpecialChars(.data$UnitsAbbreviation) %>%
                    stringr::str_replace("m3", "yr"),
                  label_load = sprintf("%s.%s",
                                       .data$short_name,
                                       .data$unit_load)) %>%
    dplyr::rowwise(.data$VariableID) %>%
    dplyr::mutate(mean = mean(dplyr::c_across(tidyselect::all_of(structures))))

  concentrations
}

#' Emissions: calculate loads
#' @description  The annual load is calculated with V x c. For for heavy metals
#' -> l/m2-year x ug/l = ug/m2-year;  for BOD/COD/TSS -> l/m2-year x mg/l = mg/m2-year
#' @param abimo_inpout data.frame or SpatialPolygonsDataFrame with ABIMO input and
#' output as retrieved by \code{\link{postProcessABIMO}}
#' @param concentrations concentrations data frame as retrieved by
#' \code{\link{read_concentrations}}
#' @return add calculated loads as additional colums  to abimo_inpout data.frame
#' or SpatialPolygonsDataFrame
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom stats setNames
calculate_loads <- function(abimo_inpout,
                            concentrations) {
  # annual load = V * c (all units normalised to "kg/year)
  loads <- dplyr::bind_rows(
    stats::setNames(lapply(X = concentrations$mean,
                    FUN = function(a){
                      abimo_inpout$FLAECHE*(abimo_inpout$ROW/1000)*a
                    }
    ),
    nm = concentrations$label_load
    )
  )

  # add computed loads to ABIMO dataset
  cbind(abimo_inpout, loads)
}
