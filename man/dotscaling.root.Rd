% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nonLinearDotPlot.R
\name{dotscaling.root}
\alias{dotscaling.root}
\title{Predefined function to use as \code{dotscaling} in the \code{nonLinearDotPlot} function.}
\usage{
dotscaling.root(e = 0.3)
}
\arguments{
\item{e}{determines which root should be used
Default value of e equals 0.3}
}
\value{
Function to calculate dot size with 1 / (c ** e).
}
\description{
dotscaling(c) = 1 / (c**e) is a root function
}
\references{
N. Rodrigues and D. Weiskopf, "Nonlinear Dot Plots",
IEEE Transactions on Visualization and Computer Graphics, vol. 24, no. 1, pp. 616-625, 2018.
Available: \doi{10.1109/TVCG.2017.2744018}
}
