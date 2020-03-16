#' A dose~response plot with a summary
#'
#' @param conc the concentrations
#' @param resp the response of the system for conc
#' @param xlab lable for the abscissa
#' @param ylab lable for the ordinate
#'

dose_response_plot <- function (conc, resp, xlab = "conc", ylab = "resp") {
    # declare a modell
    fit <-  dr4pl::dr4pl(resp~conc) # modell
    # plotten des Fits

    plt  <- plot(fit,
                text.x = xlab , # das hier soll noch personalisierbar werde
                text.y = ylab ,       # das hier auch
                indices.outliner = fit$idx.outliner) # outliner einplotten
    return(list(fit, plt))
}

