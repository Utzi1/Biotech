#' Generic plotting function for usage with any absorption spectroscopic data
#'
#' create a graph from obtained data of calibration standards
#'
#' @param abs list
#' @param conc list
#'
#' @export
#'
plot_regression <- function(abs, conc){
  ggplot2::ggplot(mapping = ggplot2::aes(abs, conc))+
    ggplot2::geom_point()+
    ggplot2::geom_smooth(method = "lm")
}

