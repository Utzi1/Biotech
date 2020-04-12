time.mes <- c(1.85,1.89,1.93,1.97)
airflow <- 5.99
percent <- c( 20.3, 22.5, 24.9, 27 )
rpm <- c( 1250, 1358, 1467, 1500 )
kLa <- c( 126 , 152 , 182 , 202 )
KV <- c(8.51, 8.52 , 8.54 , 8.57 )
#' The power-input via the motor
#'
#' @param P.perc motor power in percent
#' @param rpm rotation per minute
#' @param F1 armature voltage / rpm
#' @param F2 armature current / motor power in percent
#' @param FC  degree of efficiency
#'
#' @return
#' @export
#'
#' @examples
#' PM.mes <- PM( P.perc = percent, rpm = rpm )
PM <- function (P.perc, rpm, F1 = .107, F2 = .062, FC = .753) {
    PM <- (F1 * rpm) * (F2 * P.perc)
    PM <- PM * FC
    return(PM)
}

#' Compute K and m from Zlokarnik-Korrelation
#'
#' @param kLa kLa
#' @param power power input in Watt
#' @param vol.rea volume of the reactor vessel in m^3
#' @param vel.gas gas velocity in l/min
#' @param rho the density of the fermentation broth
#' @param gforce the g-force
#' @param eta the viscosity
#'
#' @return K and m
#' @export

K.Zlokarnik <- function ( kLa,
power,
vol.rea ,
vel.gas,
rho = 998,
gforce = 9.81,
eta = .724
) {
    x <-
        (log((power / vol.rea) * (1 / (rho * ((eta * gforce) ^ (2 / 3)
        )))))
    y <- ( log( kLa * (vol.rea / vel.gas) ) )
    model <- lm(y~x)
    return(
        list(
            Modell = summary(model),
            ggplot(mapping = aes(x, y))+
                geom_point()+
                geom_smooth(method = "lm"),
            "K und m" = (exp(1)^(coefficients(model)))
        )
    )
}


