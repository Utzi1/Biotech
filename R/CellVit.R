#' Compute the cell vitality
#'
#' @param unstained cell count of unstained cells
#' @param stained cell count of stained cells
#'
#' @return vitality
#'
#' @export
#'
#' @examples
#' # we use a real dataset:
#' us <- c(55, 65, 49, 57)
#' st <- c(11, 12, 9, 6)
#'
#' CellVit(unstained = us, stained = st)
#'
CellVit <- function(unstained, stained){
    return(
          (unstained / (unstained + stained)) * 100
          )
}
#' Compute the cell vitality after frosting
#'
#' @param unstained_defro cell count of unstained (vital) cells immediately after defrosting
#' @param unstained_24h count of unstained (vital) cells 24 hours after defrosting
#'
#' @return multiplication count after frosting
#' @export
#'
#' @examples
#' # original data is going to be applied
#' ud  <- c(12, 22, 15, 17)
#' u24 <-  c(9, 3, 5, 4)
#'
#' MultiRate(unstained_defro =ud, unstained_24h = u24)
#'
MultiRate <- function(unstained_defro, unstained_24h){
    return(
          (unstained_24h/unstained_defro) * 100
    )
}
#' Compute yield count
#'
#' @param unstained_24h count of unstained (vital) cells 24 hours after defrosting
#' @param unstained_prefro cell count of unstained (vital) cells in advance of frosting
#'
#' @return yield count
#' @export
#'
#' @examples
#' # original data is going to be applied
#' uP  <- c(88, 73, 72, 97)
#' u24 <-  c(9, 3, 5, 4)
#' yield(unstained_prefro = uP, unstained_24h = u24)
yield <- function(unstained_24h, unstained_prefro){
    return(
          (unstained_24h/unstained_prefro) * 100
    )
}
#' Vitality of a cell population
#'
#' @param stained cell count of stained cells
#' @param unstained cell count of unstained cells
#'
#' @return Vitality of the cell population
#' @export
#'
#' @examples
#' vitality(stained = 3, unstained = 77)
vitality <- function(stained, unstained){
    return(
          (unstained/stained) * 100
    )
}
#' Vitality rate of a cell population
#'
#' @param unstained_prefro cell count of unstained cells prior to frosting
#' @param unstained_defro cell count of unstained cells after frosting
#'
#' @return Vitality rate of a cell population 
#' @export
#'
#' @examples
#' vitrate(unstained_prefro = 33, unstained_defro = 12)
vitrate <- function(unstained_prefro, unstained_defro){
    return(
          (unstained_defro/unstained_prefro)*100
    )
}

