#' A dose~response plot with a summary
#'
#' @param conc the concentrations
#' @param resp the response of the system for conc
#' @param xlab lable for the abscissa
#' @param ylab lable for the ordinate
#'
#' @export
#'
#' @return a plot

dose_response_plot <- function (conc, resp, xlab = "conc", ylab = "resp") {
    fit <-  dr4pl::dr4pl(resp~conc)
    plt  <- graphics::plot(fit,
                text.x = xlab ,
                text.y = ylab ,
                indices.outliner = fit$idx.outliner)
    return(list(fit, plt))
}

