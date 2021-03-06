% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conc_vis_96.R
\name{concVis96.untrimmed}
\alias{concVis96.untrimmed}
\title{Visualisation of a 96-Wellplate (concentration) without triming of any cols or rows}
\usage{
concVis96.untrimmed(plate)
}
\arguments{
\item{plate}{the plate to be visualised as dataframe}
}
\value{
a neat looking plot with the absorption intensity as color-gradient

a neat looking matrix with the concentration as color-gradient
}
\description{
this method uses a dataframe of 8x12 values to show the measured value of each well as a colorgradient
}
\examples{

\dontrun{
 matrix(rnorm(96), nrow = 8, ncol = 12) \%>\%
  as.data.frame() \%>\%
  concVis96()}
  
}
