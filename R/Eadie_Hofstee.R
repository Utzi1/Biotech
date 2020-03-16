#' Draw an Eadie-Hofstee graph (and compute the y-axis intercept)
#'
#' @param vel Velocity
#' @param sub Substarate concentration
#' @param titleEDH Title of the plot
#' @param xlable lable of the abscissa
#' @param ylable lable of the ordinate
#'
Eadie_Hofstee <- function(vel, sub, titleEDH = "Eadie-Hostee-Plot",
                          xlable = "vel/sub", ylable = "sub",
                          printfigure=F){
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
  if(printfigure){
    plot(EHPlot)
  }
  EHModel <- stats::lm(vel/sub~vel)
  EHSummary <- base::summary(EHModel)

  return(list(EHPlot=EHPlot, EHmodel=EHModel, EHSummary))
}

