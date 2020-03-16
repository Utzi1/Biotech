#' concentration evaluation
#'
#' Hiermit kannst du die Ergebnisse deiner Standardreihe (Absorptionsmessung jeder Art) plotten, bewerten und
#' die Konzentration deiner unbekannten Probe bestimmen.
#' Verdünnungsfaktoren sind noch kein Bestandteil der Funktionen
#'
#' @param abs_P float
#' @param abs_P absorption of a unknown sample
#' @param abs_std absorption of calibrationstandards
#' @param conc_std concentration of calibrationstandards
#'
#' @return float
#'
#' @export
conc_eval <- function(abs_P, abs_std, conc_std){
  (LinMod <- stats::lm(
                       conc_std ~ abs_std)
  )
    base::print(base::summary(LinMod))
  return(
              abs_P * LinMod$coefficients[2] + LinMod$coefficients[1]
  )
}
