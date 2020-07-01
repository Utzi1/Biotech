#' Plot a Lineweaver-Burk diagram and compute the ordinate intercept
#'
#' @param sub substrate concentration
#' @param vel enzyme velocity
#' @param title title of the plot
#' @param xlab lable of the abscissa
#' @param ylab lable of the ordinate
#'
Lineweaver_Burk <- function(sub, vel, title = "Lineweaver-Burk-Plot", xlab = "1/sub", ylab = "1/vel"){
 LiBuPlt <- (ggplot2::ggplot(mapping = ggplot2::aes(
    x = 1/sub,
    y = 1/vel
  ))+
    ggplot2::geom_point()+
    ggplot2::geom_smooth(
      method = "lm",
      fullrange = TRUE
    )+
   ggplot2::scale_x_continuous(expand=c(0,0), limits=c(0, max(1/sub+ 1))) +
   ggplot2::scale_y_continuous(expand=c(0,0), limits=c(0, max(1/vel + .01))) +
   ggplot2::ggtitle(title)+
   ggplot2::xlab(xlab)+
   ggplot2::ylab(ylab)) 
return(LiBePlt)
#  Velo <-1/vel
#  Subs <- 1/sub
#  stats::coefficients(
#    stats::lm(Velo~Subs)
#  )
}

