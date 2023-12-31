% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{rescaler}
\alias{rescaler}
\title{Rescale each column of a matrix}
\usage{
rescaler(x, magnitude)
}
\arguments{
\item{x}{a matrix}

\item{magnitude}{a single value or a vector with length equal to the number of columns in x}
}
\value{
a matrix
}
\description{
Rescale each column of a matrix
}
\examples{
rescaler(matrix(1, nrow = 2, ncol = 2), magnitude = c(2,3))
}
