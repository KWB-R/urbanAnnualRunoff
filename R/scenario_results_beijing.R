#' Results of ABIMO Scenario Analysis For Beijing
#'
#' A dataset for ABIMO modelling results for Beijing case study
#'
#' @format A data.frame with 23 rows and 9 variables:
#' \describe{
#'   \item{scenario_name}{name of scenario}
#'   \item{catchment_km2}{sum of FLGES ans STR_FLGES (in square kilometers)}
#'   \item{rainfall_cbm}{total rianfall in catchment  ABIMO (in cubicmeters/year)}
#'   \item{runoff_cbm}{calculated runoff by ABIMO (in cubicmeter)}
#'   \item{infiltration_cbm}{calculated infiltration by ABIMO (in cubicmeter/year)}
#'   \item{evapotrans_cbm}{calculated evapotranspiration by ABIMO (in cubicmeter/year)}
#'   \item{abimo_inpout}{tibble with ABIMO input/output (only water balance) }
#'   \item{abimo_inpout_emissions}{tibble with ABIMO input/output (water balance + emissions) }
#'   \item{vrr}{calculated volume rainfall retained (1-runoff_cbm/rainfall_cbm) }
#' }
"scenario_results_beijing"
