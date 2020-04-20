#' Compute the concentration of an competitive ELISA based on the 4-Parameter-logistic function
#'
#' @param std.conc the concentration of the calibrators
#' @param std.resp the measured absorbance of the calibrators
#' @param mes.resp the measured absorbance of the samples of unknown concentration
#'
#'
#' @return the model-fit and the concentration of the samples
#'
#' @export
conc.4PL <- function (std.conc,
                      std.resp,
                      mes.resp) {
    fit <-
        dr4pl::dr4pl( std.conc~std.resp )
    conc <-
        fit$parameters[1] + ((fit$parameters[4] - fit$parameters[1])/(1 + (mes.resp/fit$parameters[2])^fit$parameters[3]))
    return(
           list(
                fit,
                conc
           )
    )
}
