#' Get ABIMO Statistics
#'
#' @param abimo_inpout abimo_inpout
#' @return tibble with columns "catchment_km2", "rainfall_cbm", "infiltration_cbm",
#' evapotrans_cbm and "vrr" (1 - runoff_cbm / rainfall_cbm)
#' @export
#'
get_abimo_stats <- function(abimo_inpout) {

  tibble::tibble(
    catchment_km2 = round(sum(as.numeric(abimo_inpout$FLAECHE))/1000000, 2),
    rainfall_cbm = round(sum(as.numeric(abimo_inpout$FLAECHE)*as.numeric(abimo_inpout$REGENJA)/1000),0),
    runoff_cbm = round(sum(as.numeric(abimo_inpout$FLAECHE)*as.numeric(abimo_inpout$ROW)/1000),0),
    infiltration_cbm = round(sum(as.numeric(abimo_inpout$FLAECHE)*as.numeric(abimo_inpout$RI)/1000),0),
    evapotrans_cbm = round(sum(as.numeric(abimo_inpout$FLAECHE)*as.numeric(abimo_inpout$VERDUNSTUN)/1000),0),
    vrr = round(1 - .data$runoff_cbm/.data$rainfall_cbm,3)
  )
}

#' Get Scenario Results
#'
#' @param paths paths to directory containing all ABIMO scenario
#' results
#' @return tibble
#' @export
#' @importFrom tibble as_tibble
#' @importFrom raster shapefile
#' @importFrom tidyselect ends_with
#' @importFrom dplyr across bind_cols bind_rows summarise
get_scenario_results <- function(paths) {

  required_paths <- c("abimo_inp_dbf",
                      "abimo_scenarios",
                      "abimo_out",
                      "emissions_input",
                      "site")

  is_available <- required_paths %in% names(paths)

  if(!all(is_available)) {
    stop(sprintf("Not all required paths available!
Missing path(s): %s",
paste0(required_paths[is_available], collapse = ", ")))
  }

scenario_dirs <- list.dirs(paths$abimo_scenarios)[-1]

scenario_results_list <- lapply(scenario_dirs, function(scenario_dir) {

  scenario_name <- basename(scenario_dir)

  abimo_inpout <- postProcessABIMO(
    path_input = file.path(scenario_dir, basename(paths$abimo_inp_dbf)),
    path_output = file.path(scenario_dir, paths$abimo_out)
  )

  # write out joined table output as shapefile
  outFile <- file.path(scenario_dir,
                       sprintf("abimo_%s_waterbalance.shp", paths$site))

  raster::shapefile(x = abimo_inpout,
                    filename = outFile,
                    overwrite = TRUE)

  # compute emissions with OgRe AMC (annual mean concentration) database
  conc <- read_concentrations(path = paths$emissions_input)

  abimo_inpout_emissions <- calculate_loads(
    abimo_inpout = abimo_inpout,
    concentrations = conc
  )

  # write out joined table output as shapefile
  outFile <- file.path(scenario_dir,
                       sprintf("abimo_%s_emissions.shp", paths$site))

  raster::shapefile(x = abimo_inpout_emissions,
                    filename = outFile,
                    overwrite = TRUE)

  loads_sum <- abimo_inpout_emissions@data %>%
    dplyr::summarise(dplyr::across(tidyselect::ends_with("kg_yr"),
                                   .fns = sum)) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("kg_yr"),
                                   .fns = round))

  tibble::tibble(scenario_name = scenario_name,
                 get_abimo_stats(abimo_inpout),
                 abimo_inpout = list(tibble::as_tibble(abimo_inpout@data)),
                 abimo_inpout_emissions = list(tibble::as_tibble(abimo_inpout_emissions))) %>%
    dplyr::bind_cols(loads_sum)
})


scenario_results_list %>%
  dplyr::bind_rows()
}
