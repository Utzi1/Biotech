
# defining the basic fun:

#' Function to analyse a PAGE 
#'
#' @param marker.w weight of marker protein
#' @param marker.d marker-band distance
#' @param unknown.d distance of a unknown sample-band
#' @param plot.PAGE want a plot? Set TRUE or FALSE
#'
#' @return
#' @export
#'
#' @examples
PAGE.gen <- function (marker.w, # Marker weights
                      marker.d, # Marker distance
                      unknown.d, # distance of unknown bands
                      plot.PAGE = FALSE) {
    ##### evals ######################################################################
    RF <- marker.d/max(marker.d)
    mod <- stats::lm(log10(marker.w)~RF)
    unknown.w <- (
                  mod$coefficients[2]* (unknown.d/max(marker.d)) + mod$coefficients[1]
    )
    ##### returns ####################################################################
    if (plot.PAGE == FALSE) {
        return(unknown.w)
    } else if (plot.PAGE == TRUE) {
        return(
               list(
    ggplot2::ggplot(mapping = ggplot2::aes(y = log10(marker.w) ,x = RF))+
        ggplot2::geom_point()+
        ggplot2::geom_smooth(method = "lm")+
        ggplot2::theme_minimal()+
        ggplot2::ylab("log markerweight"),
  unknown.w,
  summary(mod)
               )
        )
    }
}
