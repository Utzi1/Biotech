#' Create a non-linear Regression for a Michaelis-Menten type Enzyme with a little knowlege of its Km and Vmax
#'
#' @param sub substrate concentration
#' @param xlab lable of the abscissa
#' @param ylab lable of the ordinate
#' @param title title of the plot
#' @param velo velocity
#'
#' @return a plot
#' @export
#'
plot_MM_direct <- function(sub, velo, title = "title", xlab = "abscisaa", ylab = "ordinate"){

    # Fitten des SSmicmen-Modell

    fit <- stats::nls(velo ~ SSmicmen(sub, Vm, K))

    # plotten des Fits

       nplot <- ( ggplot2::ggplot(mapping = ggplot2::aes(x = sub, y = velo))+
       ggplot2::geom_point()+
       ggplot2::stat_function(fun = function(sub){ (stats::coef(fit)[[1]] * sub) / ( stats::coef(fit)[[2]] + sub)}, color = "blue")+ # einzeichnen des Fitting
       ggplot2::geom_hline(yintercept = stats::coef(fit)[[1]])+ # Vmax aus coefficents eintragen
       ggplot2::geom_text(ggplot2::aes(0,stats::coef(fit)[[1]],label = round(stats::coef(fit)[[1]], digits = 3), vjust = 1.4), hjust = .3 )+ # Vmax aus coefficents eintragen
       ggplot2::geom_hline(yintercept =(stats::coef(fit)[[1]])/2,)+ # Vmax/2 aus coefficents
       ggplot2::geom_text(ggplot2::aes(0,(stats::coef(fit)[[1]])/2,label = "Vmax/2", vjust = 1.4), hjust = .3)+ # Vmax/2 aus coefficents eintragen
       ggplot2::geom_vline(xintercept =stats::coef(fit)[[2]])+ # Km aus coefficents
       ggplot2::geom_text(ggplot2::aes(0,stats::coef(fit)[[2]],label = round(stats::coef(fit)[[2]], digits = 3), vjust = 1.41), hjust = -2.3)+ # Km aus coefficents eintragen
       ggplot2::xlab(xlab)+
       ggplot2::ylab(ylab)+
       ggplot2::ggtitle(title)
)
return(list(fit, nplot))
}

