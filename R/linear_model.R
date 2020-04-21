#' Linear model for concentration evaluation
#'
#' This must be performed in advanvce of usage of the function conc_eval to produce a linear model suiteable
#' for concentration evaluation.
#'
#' @param abs list
#' @param conc list
#'
#' @return summary of the regression
#' @export
#'
LinMod <- function(abs, conc) {
  mod <- stats::lm(
    conc ~ abs
  ) %>%
    base::summary()
  return(mod)
}
