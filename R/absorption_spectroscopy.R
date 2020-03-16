#' Functions to compute fundamental values in absorption-spectroscopy

conc.from.abs <- function (abs, epsilon, cuvette = 1, dfac = 1) {
    concentration <- (abs / ( epsilon * cuvette )) * dfac #%>%
        # print()
    return(concentration)
}
