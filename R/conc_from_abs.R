#' Functions to compute fundamental values in absorption-spectroscopy
#'
#' @param abs measured absorption
#' @param epsilon molar attenuation coefficient (without default)
#' @param cuvette cuvette factor, default is 1
#' @param dfac dilution factor, default is  1
#'
#'
#'
#'

conc.from.abs <- function (abs, epsilon, cuvette = 1, dfac = 1) {
        conc <-((abs / ( epsilon * cuvette )) * dfac)
        return(conc)
}
