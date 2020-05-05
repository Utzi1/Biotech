#' Visualisation of a 96-Wellplate (concentration) including trimming of the first col and row
#' 
#' This method trims of the first first row and col of by default 
#' 
#' @param plate the plate to be visualised as dataframe
#'
#' @return a neat looking matrix with the concentration as color-gradient 
#' @export
#'
#' @examples
#' 
#' \dontrun{concVis96.trimmed(HSA1)}
#' 
concVis96.trimmed <- 
    function(plate) {
    daten <- plate[,2:13] %>% 
    as.matrix() %>% 
    as.vector()
    y  <- paste0(seq(1,12))
    x <- LETTERS[1:8]
    grid.1 <- expand.grid(X = x, Y = y)
    grid.1$Abs <- daten
    vis96 <- ggplot2::ggplot( data = grid.1, mapping = ggplot2::aes( X, Y, fill = Abs ) )+
        ggplot2::geom_tile()
    return(vis96)
}

#' Visualisation of a 96-Wellplate (concentration) without triming of any cols or rows
#' 
#' this method uses a dataframe of 8x12 values to show the measured value of each well as a colorgradient
#' @param plate 
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'  matrix(rnorm(96), nrow = 8, ncol = 12) %>%
#'   as.data.frame() %>%
#'   concVis96()}
#'   
concVis96.untrimmed <- 
    function(plate) {
        daten <- plate[] %>% 
            as.matrix() %>% 
            as.vector()
        y  <- paste0(seq(1,12))
        x <- LETTERS[1:8]
        grid.1 <- expand.grid(X = x, Y = y)
        grid.1$Abs <- daten
        vis96 <- ggplot2::ggplot( data = grid.1, mapping = ggplot2::aes( X, Y, fill = Abs ) )+
            ggplot2::geom_tile()
        return(vis96)
    }

