#' Results of ABIMO Scenario Analysis For Jinxi
#'
#' A dataset for ABIMO modelling results for Jinxi case study
#'
#' @format A data.frame with 3 rows and 20 variables:
#' \describe{
#'   \item{scenario_name}{name of scenario}
#'   \item{catchment_km2}{sum of FLGES ans STR_FLGES (in square kilometers)}
#'   \item{rainfall_cbm}{total rianfall in catchment  ABIMO (in cubicmeters/year)}
#'   \item{runoff_cbm}{calculated runoff by ABIMO (in cubicmeter)}
#'   \item{infiltration_cbm}{calculated infiltration by ABIMO (in cubicmeter/year)}
#'   \item{evapotrans_cbm}{calculated evapotranspiration by ABIMO (in cubicmeter/year)}
#'   \item{vrr}{calculated volume rainfall retained (1-runoff_cbm/rainfall_cbm) }
#'   \item{abimo_inpout}{tibble with ABIMO input/output (only water balance) }
#'   \item{abimo_inpout_emissions}{tibble with ABIMO input/output (water balance + emissions) }
#'   \item{BOD.kg_yr}{Biological Oxygen Demand (in kg/year)}
#'   \item{COD.kg_yr}{Chemical Oxygen Demand (in kg/year)}
#'   \item{TSS.kg_yr}{Total Supended Solid (in kg/year) }
#'   \item{Pb.kg_yr}{Lead (in kg/year) }
#'   \item{Cd.kg_yr}{Cadmium (in kg/year)}
#'   \item{Cr.kg_yr}{Chrome (in kg/year)}
#'   \item{Cu.kg_yr}{Copper (in kg/year) }
#'   \item{Ni.kg_yr}{Nickel (in kg/year) }
#'   \item{Va.kg_yr}{Vanadium (in kg/year) }
#'   \item{Zn.kg_yr}{Zinc (in kg/year) }
#' }
"scenario_results_jinxi"
