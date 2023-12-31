% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{arrow_head_ellipse}
\alias{arrow_head_ellipse}
\title{Make arrrowhead with ellipse}
\usage{
arrow_head_ellipse(
  a = 1,
  b = 1,
  superellipse = 2,
  rotate = 0,
  rescale = c(1, 1),
  nudge = c(0, 0),
  transformations = c("unitizer", "rotater", "rescaler", "nudger"),
  n = 361,
  plot = FALSE
)
}
\arguments{
\item{a}{width of ellipse}

\item{b}{height of ellipse}

\item{superellipse}{parameter for specifying superellipses. Can be of length 1 or 2}

\item{rotate}{rotation angle in radians}

\item{rescale}{a single value or 2-length vector for scaling in x and y}

\item{nudge}{a single value or 2-length vector for nudging in x and y}

\item{transformations}{a vector of transformation functions}

\item{n}{number of points in polygon}

\item{plot}{plot arrowhead if TRUE}
}
\value{
a matrix
}
\description{
Make arrrowhead with ellipse
}
\examples{
ellipsehead <- arrow_head_ellipse(plot = TRUE, b = .5)
ellipsehead_spaced <- arrow_head_ellipse(
  plot = TRUE,
  b = .5,
  rescale = .45,
  nudge = c(.55, 0)
)
# Make regular polygon with n - 1 sides
pentagon <- arrow_head_ellipse(n = 6, plot = TRUE)
# make a superellipses
star4 <- arrow_head_ellipse(superellipse = .5, plot = TRUE)
squircle <- arrow_head_ellipse(superellipse = 3, plot = TRUE, rotate = pi / 4)
longboat <- arrow_head_ellipse(plot = TRUE, b = 1, a = 4, superellipse = c(3,.5))
}
