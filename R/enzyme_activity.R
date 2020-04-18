#' generic functions to determin protein activity
#'
#' All Volumes should be given with same metrics
#'
#' @param vol.tot total volume, default is 1
#' @param dfac dilution factor, default is 1
#' @param delta.E Delta E (without default)
#' @param vol.test test volume (without default)
#' @param epsilon molar attenuation coefficient (without default)
#' @param cuvette cuvette factor or path length, default is 1
#'
vol.act <- function (vol.tot =1 , dfac = 1, delta.E, vol.test, epsilon, cuvette = 1 ){

    vol.act <- (( vol.tot * dfac * delta.E ) / ( epsilon * cuvette * vol.test ))
        return(vol.act)

}
