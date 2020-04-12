#' Functions to compute concentration values in absorption-spectroscopy
#'
#' \code{sum} This function possesses the ability to return the concentration of an unknown sample by comparing its absorption to a linear regression model
#'
#' @param abs Absorption of a measurement of unknown concentration, can be a vector, too
#' @param epsilon Molar attenuation coefficient or absorptivity
#' @param cuvette The cuvette-factor, default is 1
#' @param dfac Dilution-factor of each abs, can be a vector, too
#'
#' @return The concentration
#' @export
#'
conc.from.abs <- function (abs, epsilon, cuvette = 1, dfac = 1) {
    concentration <- (abs / ( epsilon * cuvette )) * dfac #%>%
        # print()
    return(concentration)
}
