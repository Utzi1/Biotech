#' Compute the cell vitality
#'
#' @param unstained cell count of unstained cells
#' @param stained cell count of stained cells
#' @param unstained_prefro in adavance of frosting
#' @param unstained_defro after frosting
#' @param unstained_24h 24h after defrosting (and cultivation)
#' @return vitality

CellVit <- function(unstained, stained){
    return(
          (unstained / (unstained + stained)) * 100
          )
}
MultiRate <- function(unstained_defro, unstained_24h){
    return(
          (unstained_24h/unstained_defro) * 100
    )
}
yield <- function(unstained_24h, unstained_prefro){
    return(
          (unstained_24h/unstained_prefro) * 100
    )
}
vitality <- function(stained, unstained){
    return(
          (unstained/stained) * 100
    )
}
vitrate <- function(unstained_prefro, unstained_defro){
    return(
          (unstained_defro/unstained_prefro)*100
    )
}

