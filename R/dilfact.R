#' Computing a dilution factor, a sample volume (Vorlagevolumen), concentration after dilution


# Dilution factor
dilfact <- function (vol.1, vol.2) {
    dilfact <- (vol.1/vol.2)
    return(dilfact)
}

# sample volume
samp.vol <- function (conc.aim, vol.aim = 1, conc.is) {
    samp.vol <- ( (conc.aim * vol.aim) / conc.is)
    return(samp.vol)
}

# concentration after dilution
conc.per.dil <- function (vol.pre , vol.post, conc.pre) {
    conc.per.dil <- ((conc.pre * vol.pre) / vol.post)
    return(conc.per.dil)
}
