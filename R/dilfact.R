#' Computing a dilution factor, a sample volume (Vorlagevolumen), concentration after dilution


# Dilution factor
#' Compute the dilution factor
#'
#' @param vol.1 V1 use same metrics 
#' @param vol.2 V2 use same metrics
#'
#' @return FV a dilution factor
#' @export
#'
#' @examples
#' dilfact(1, 100)

dilfact <- function (vol.1, vol.2) {
    dilfact <- (vol.1/vol.2)
    return(dilfact)
}

# sample volume
#' Compute sample (Vorlage) volumes 
#' V_vor = (V_{soll} * c_{soll}) /  c_{ist}
#' @param conc.aim aim concentration (C_soll)
#' @param vol.aim volume that is aimed (V_soll)
#' @param conc.is current concentration (C_ist)
#'
#' @return sample volumes
#' @export
samp.vol <- function (conc.aim, vol.aim = 1, conc.is) {
    samp.vol <- ( (conc.aim * vol.aim) / conc.is)
    return(samp.vol)
}

# concentration after dilution
#' Compute the concentration after a dilution step
#'
#' @param vol.pre volume in advance of the dilution step
#' @param vol.post volume after the dilution step
#' @param conc.pre concentration in advance of the dilution
#'
#' @return current concentration of the dilution after the dilution step
#' @export
conc.per.dil <- function (vol.pre , vol.post, conc.pre) {
    conc.per.dil <- ((conc.pre * vol.pre) / vol.post)
    return(conc.per.dil)
}
