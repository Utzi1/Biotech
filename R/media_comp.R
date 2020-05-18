# This file consists of formulas to compute the media compounds for fermentation as suggested by W. K. Bronn: Fermentationstechnik I und II
library(tidyverse)

# Percentage of each element found in dry mass of each organism
Ele <- matrix(c(11, 9, 1.5, 1.2, 2.0, 2.0, 1.2, 1.0, 0.3 ,0.25, 0.2, 0.1, 0.2, 0.004, 0.005, 0.005), nrow = 2)
rownames(Ele) <- c('Bacteria', 'Fungus')
colnames(Ele) <- c('N', 'P', 'K', 'S', 'Mg', 'Na', 'Ca', 'Fe')
# Molecular weight of the Elements
mol.weight <- c(30.97330,14.007, 39.08, 32.067, 24.308, 22.99, 40.078, 55.845)
names(mol.weight) <- c('P', 'N', 'K', 'S', 'Mg', 'Na', 'Ca', 'Fe')
# Molecular weight of the salts used as default sources for the element in the Lab
# This may need to be modified depending of which salts are used
source.mol.weight <- c(115, 132, 74.5, 246)
names(source.mol.weight) <- c('P', 'N', 'K','Mg')

#' Compute the concentration of the source salts for specific elements inside a fermentationbroth
#'
#' @param ds dry substance mass in g/l
#' @param ele element abreveation as string can be list('P', 'N', 'K','Mg')
#' @param org organism as string can be list('Bacteria', 'Fungus')
#'
#' @return concentration of salt within the broth
#' @export
#'
#' @examples
#'
#' # c_any(ds = 50, ele = 'P', org = 'Fungus')
#' # c_any(ds = 50, ele = "N", org = 'Fungus')
c_any  <- function (
                    ds,
                    ele = "P",
                    org = "Bacteria"
                    ) {
    if (ele != 'N' ) {
        c_any <- ( ( ds * Ele[org, ele] * source.mol.weight[ele]) / mol.weight[ele] )
        return((c_any/100))
    } else if ( ele == 'N' ) {
        c.P<- ( ( ds * Ele[org, 'P'] * source.mol.weight['P']) / mol.weight['P'] )
        c.N.P <- c.P / mol.weight[ele]
        c_any <- ( ( ( ds * Ele[org, ele] - c.N.P) * source.mol.weight[ele]) / (2*mol.weight[ele]) )
        return((c_any/100))
    }
}

