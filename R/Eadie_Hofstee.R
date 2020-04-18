#' Draw an Eadie-Hofstee graph (and compute the y-axis intercept)
#'
#' @param vel Velocity
#' @param sub Substarate concentration
#' @param titleEDH Title of the plot
#' @param xlable lable of the abscissa
#' @param ylable lable of the ordinate
#'
#' @return a Plot, the model itself and a summary of the model
#' @export
#'
#' @examples
#' # simulate values
#' sub <-seq(1,20,1)
#' vel <-((runif(1,14.7,15)*sub)/(runif(1,2.5,3)+sub))+rnorm(20,0,.3)
#' # plot them
#' Eadie_Hofstee(vel = vel, sub = sub)
#'
#'
#'
Eadie_Hofstee <- function(vel, sub, titleEDH = "Eadie-Hostee-Plot",
                          xlable = "vel/sub", ylable = "sub"){
  EHPlot <-   ggplot2::ggplot(mapping = ggplot2::aes(
    x = vel/sub,
    y = vel)
  )+
    ggplot2::geom_point()+
    ggplot2::geom_smooth(method = "lm",
                         fullrange = TRUE)+
    ggplot2::scale_x_continuous(expand=c(0,0), limits=c(0, base::max(vel/sub))) +
    ggplot2::scale_y_continuous(expand=c(0,0), limits=c(0, base::max(vel)+20))+
    ggplot2::ggtitle(titleEDH)+
    ggplot2::xlab(xlable)+
    ggplot2::ylab(ylable)
  EHModel <- stats::lm(vel/sub~vel)
  EHSummary <- base::summary(EHModel)
  return(list(EHPlot=EHPlot, EHmodel=EHModel, EHSummary))
}

