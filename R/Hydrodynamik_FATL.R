# Der Gasgehalt eG
#' Compute gas content  
#' Keep an eye on the units
#' @param VG volume of gas 
#' @param VL volume of liquid
#'
#' @return eG or the gas content
#' @export
#'
#' @examples eG( VG = c(1, 2, 3), VL = c(6, 12, 24) )
eG <- function (VG, VL) {
    eG <- VG / ( VG + VL )
    return(eG)
}

# Berechnung der Dispersionsvolumina
#' Compute the dispersion volume
#' mind the units 
#' @param Ai column cross-section 
#' @param HDi gasified height
#'
#' @return the dispersion volume
#' @export
#'
#' @examples VDi(Ai = 0.45, HDi = 1.33)
VDi <- function (Ai, # Säulenquerschnitt
                 HDi # Höhe im Begasten Zustand
                 ) {
    VDi <- (Ai * HDi)
    return(VDi)
}

# eG des Airliftreaktor im Praktikum
#' compute the total gas content (eGT) of an airlift-reactor with a single riser and a single downer
#'
#' @param eGR gas content of the riser
#' @param eGD gas content of the downer
#' @param VDD dispersion volume of the downer
#' @param VDR dispersion volume of the riser
#'
#' @return total gas content (eGT)
#' @export
#'
eGT <- function (eGR, # Riser
                 eGD, # Downcomer
                 VDD, # Dispersionsvolumen Downcomer
                 VDR # Dispersionsvolumen Riser
                 ) {
    eGT  <- (eGR * VDR + eGD * VDD) / (VDR * VDD)
    return(eGT)
}

# Berücksichtigung des Rotameters
#' compute the actual gas flow (eliminate the error of the rotameter) of either the big airlift reactor or the smaller one used at the Biotechnikum at Forum Seestraße
#'
#' @param velG.mes measured gas velocity (values read of from rotameter)
#' @param p.mes measured current room pressure 
#' @param t.mes measured current room temperature
#' @param reactor either the "big" or "small" reactor (as string)
#'
#' @return actual gas flow
#' @export
#'
#' @examples velG.cal( velG.mes = 1, p.mes = 1.024, t.mes = 299.9, reactor = "small" )
velG.cal <- function (velG.mes,
                      # gemessener Gasvolumenstrom
                      p.mes,
                      # Raumdruck
                      t.mes,
                      # gemessene Raumtemperatur
                      reactor = c("big", "small")) {
    if (reactor == "big") {
        velG.cal <- (velG.mes * sqrt((3.013 + p.mes) * 293.15) / (3.103 * t.mes))
    } else if (reactor == "small") {
        velG.cal <- (velG.mes * sqrt((1.313 + p.mes) * 293.15) / (1.103 * t.mes))
    }
    return(velG.cal)
}


# empirische eG-Vorhersagefunktion
#' Empiric prediction function for the Gas holdup (eG)
#'
#' @param uG superficial velocity
#'
#' @return gas holdup prediction
#' @export
eG.empir <- function(uG){
    eG.empir <- .6 * ((uG) ^ .7)
    return(eG.empir)
}
# Errechnung der Gaslehrrohrgeschwindigkeit
#' compute the superficial velocity
#'
#' @param velG gas velocity
#' @param A flow surface
#'
#' @return the superficial velocity
#' @export
#'
uG <- function (velG, A) {
    velG.empty  <- velG / A
    return(velG.empty)
}

# Berechnung der Mischzeit (mixing time)
#' compute the mixing time
#'
#' @param H height of the reactor
#' @param D diameter of the reactor
#' @param uG superficial velocity
#' @param gforce g-force, default is 9.81
#'
#' @return the mixing time
#' @export
mt <- function (H, D, uG, gforce = 9.81) {
    11 * H/D * ( uG *  gforce * ( D ^ (-2) ) ) ^ ( -.33 )
}
# Der kLa über eG
#' compute kLa via eG (gas holdup)
#'
#' @param eG gas holdup (eG)
#' @param K a specific factor, default is 0.61
#' @param p K a specific factor, default is 0.81
#'
#' @return the kLa via eG
#' @export
kLa.eG <- function (eG, K = .61, p = .83) {
    kLa.eG  <- (K * eG ^ p)
    return(kLa.eG)
}
