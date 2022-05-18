#' Fix ABIMO shares
#'
#' @param abimo abimo object
#'
#' @return fixed percental shares (PROBAU, PROVGU, STR_FLGES)
#' @export
#'
fix_abimo_shares <- function(abimo) {

  abimo@data$PROVGU <- 0

  area_catchment_without_streets <- abimo$FLGES
  area_catchment_with_streets <- abimo$FLGES+abimo$STR_FLGES
  area_streets <- abimo@data$STR_FLGES
  area_roofs <- area_catchment_with_streets*abimo@data$PROBAU/100
  area_sealed <- area_catchment_with_streets*abimo@data$VG/100

  share_roofs <- 100*area_roofs/area_catchment_without_streets


  is_positive <- (area_sealed - area_roofs - area_streets) > 0


  abimo@data$PROVGU[is_positive] <- 100*(area_sealed[is_positive] - area_roofs[is_positive] - area_streets[is_positive])/area_catchment_without_streets[is_positive]

  abimo@data$PROBAU[is_positive] <-  100*area_roofs[is_positive]/area_catchment_without_streets[is_positive]

  if(any(!is_positive)) {
    correction_factor <- abimo@data$VG[!is_positive]/(100*(area_roofs[!is_positive] + area_streets[!is_positive])/area_catchment_with_streets[!is_positive])

    abimo@data$PROBAU[!is_positive] <- 100*correction_factor*area_roofs[!is_positive]/area_catchment_without_streets[!is_positive]

    street_area_old <- area_streets[!is_positive]
    street_area_new <- correction_factor*area_streets[!is_positive]
    abimo@data$STR_FLGES[!is_positive] <-  street_area_new

    flges_old <- abimo@data$FLGES[!is_positive]
    flges_new <- flges_old + street_area_old -  street_area_new
    abimo@data$FLGES[!is_positive] <- flges_new
    abimo@data$PROBAU[!is_positive] <- 100*correction_factor*area_roofs[!is_positive]/flges_new

  }

  abimo
}
