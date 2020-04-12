# Leistungseintrag in die Blasensäule

#' Power input of a bubble column reactor
#'
#' @param uGR superficial velocity (riser)
#' @param A.Riser column cross-section  (riser)
#' @param p room pressure
#' @param rho.liq  density of the liquid
#' @param gforce g force
#' @param rho.gas density of the gas
#' @param d.gaser surface area of the gas nozzle
#' @param d.upcol surface area of the riser
#' @param h.undisp.Riser height of the liquid (riser) without gas
#' @param h.undisp.Downer height of the liquid (downer) without gas
#' @param A.Downer column cross-section (downer)
#' @param N count of gas outlets
#'
#' @return power input
#' @export
#'
PGVL <-
  function(uGR,
           A.Riser = 0.00528,
           p,
           rho.liq = 997.048,
           gforce = 9.81,
           rho.gas = 1.2041,
           d.gaser = (10 ^ -3),
           d.upcol,
           h.undisp.Riser,
           h.undisp.Downer,
           A.Downer = 0.00196,
           N = 52) {
    ((uGR * A.Riser) / ( ( A.Riser * h.undisp.Riser ) + ( A.Downer * h.undisp.Downer) )) * p * log(1 + ((rho.liq * h.undisp.Riser * gforce) / p)) + (((uGR ^ 3) * rho.gas * ((d.gaser / d.upcol) ^
                                                                                                                    4) * A.Riser) / (
                                                                                                                      (h.undisp.Riser * A.Riser + h.undisp.Downer * A.Downer) * 2 * (N ^ 2)
                                                                                                                    ))
  }

#' gas content of upstream column for a bubble column reactor
#'
#' @param K1 specific coefficient
#' @param uGR superficial velocity (riser)
#' @param m1 specific coefficient
#'
#' @return gas content upstream column for water
#' @export
#'
eGR_H2O <- function( K1,
                     uGR,
                     m1) {
  eGR <- K1 * (uGR^m1)
  return(eGR)
}

#' gas content upstream column for non nowton fluids
#'
#' @param K2 constant of proportionality
#' @param a2 specific coefficient
#' @param b2 specific coefficient
#' @param c2 specific coefficient
#' @param eta.eff dynamic viscosity
#' @param uGR superficial velocity (riser)
#' @param AD surface area downer
#' @param AR surface area riser
#'
#' @return  eGR gas content upstream column for non nowton fluids
#' @export
#'
eGR_CMC <- function( K2 = .465,
                     a2 = .65,
                     b2 = -1.06,
                     c2 = -.103,
                     eta.eff,
                     uGR,
                     AD,
                     AR) {
  eGR_CMC <- ( K2 * (uGR ^ a2)  * (( 1 + (AD/AR))^b2) * (eta.eff ^c2))
  return(eGR_CMC)
}

#' non-newton-korrelation model for the kLa
#'
#' @param K4 constant of proportionality
#' @param uGR superficial velocity (riser)
#' @param a4 specific coefficient
#' @param DGL diffusion coefficient of the gas within the liquid phase
#' @param b4 specific coefficient
#' @param rho.liq  density of the liquid
#' @param c4 specific coefficient
#' @param AD surface area downer
#' @param AR surface area riser
#' @param d4 specific coefficient
#' @param eta.eff dynamic viscosity
#' @param e4 specific coefficient
#' @param sigma.liq surface tension of the liqud
#' @param f4 specific coefficient
#'
#' @return
#' @export
#'
kLaD.korr.nN <-
  function(K4 = (.5 * (10 ^ -2)),
           uGR,
           a4 = .65,
           DGL = (2.7* 10^(-9)),
           b4 = .5,
           rho.liq = 1020,
           c4 = 1.03,
           AD,
           AR,
           d4 = .85,
           eta.eff,
           e4,
           sigma.liq = .071,
           f4) {
    kLaD.korr  <-
      K4 * (uGR ^ a4) * (DGL ^ b4) * (rho.liq ^ c4) * ((1 + AD / AR) ^ d4) * (eta.eff ^ e4) * (sigma.liq ^ f4)
    return(kLaD.korr)
  }

#' Title
#'
#' @param K3 constant of proportionality
#' @param AD surface area downer
#' @param AR surface area riser
#' @param a3 specific coefficient
#' @param uGR superficial velocity (riser)
#' @param b3 specific coefficient
#'
#' @return
#' @export
#'
#'
kLaD.korr.H2O <- function(K3 = .076,
                          AD, AR,
                          a3 = -2,
                          uGR,
                          b3 = .8) {
  kLaD  <-  K3 * ((1 + AD / AR) ^ a3) * uGR ^ b3
  return(kLaD)
}
#' mixing time of H2O based on korrelation
#'
#' @param K5 constant of proportionality
#' @param uGR superficial velocity (riser)
#' @param m5 specific coefficient
#'
#' @return mixing time of H2O based on korrelation
#' @export
#'
#'
mix.t.H2O <- function(K5, uGR, m5) {
  mix.t.H2O <-  K5 * (uGR ^ m5)
  return(mix.t.H2O)
}


#' Title
#'
#' @param K6 constant of proportionality
#' @param DR diamter of the riser
#' @param a6 specific coefficient
#' @param AD surface area downer
#' @param AR surface area riser
#' @param b6 specific coefficient
#' @param h.disp.Riser height of the liquid (riser) gased
#' @param c6 specific coefficient
#' @param uGR superficial velocity (riser)
#' @param d6 specific coefficient
#' @param eta.eff dynamic viscosity
#' @param e6 specific coefficient
#'
#' @return
#' @export
#'
#'
mix.t.nN <-
  function(K6 = 571,
           DR,
           a6 = -0.5,
           AD,
           AR,
           b6 = -0.16,
           h.disp.Riser,
           c6 = -1.44,
           uGR,
           d6 = -0.46,
           eta.eff,
           e6 = 0.56) {
    mix.t.nN <-
      K6 * (DR ^ a6) * ((AD / AR) ^ b6) * (h.disp.Riser ^ c6) * (uGR ^ d6) * (eta.eff ^ e6)
    return(mix.t.nN)
  }

#' Title
#'
#' @param kLa kLa
#' @param eGt gas content
#'
#' @return diffusion coefficient
#' @export
#'
#'
kLaD <- function(kLa, eGt) {
  kLaD <- kLa*(1-eGt)
  return(kLaD)
}

#' rheological modeling for highly non-newtonian fluids estimate of Ostwald & de Waele
#'
#' @param Kc consistency index
#' @param uGR superficial velocity (riser)
#' @param m index
#'
#' @return dynamic viscosity
#' @export
#'
#'
eta.eff <- function(Kc = (.3 ^ .7), uGR, m) {
  eta.eff <- (Kc * (5000 * uGR) ^(m-1))
  return(eta.eff)
}
