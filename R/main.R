


#' Fit matrix to unit circle
#'
#' @param x matrix
#' @param center center of matrix
#'
#' @return matrix
#' @export
#'
#' @examples
#' matrix2unitcircle(
#'   A <- matrix(c(1, 2,
#'                 -8,6,
#'                 9,5),
#'               ncol = 2,
#'               byrow = T))
#' cA <- matrix2unitcircle(A, center = colMeans(A))
#' plot(cA, xlim = c(-1, 1), ylim = c(-1, 1))
#' t <- seq(0,2*pi, length.out = 361)
#' lines(cos(t), sin(t))
matrix2unitcircle <- function(x, center = rep(0, ncol(x))) {
  nr <- nrow(x)
  nc <- ncol(x)
  centered <- x - matrix(rep(center,nr), ncol = nc, byrow = T)

  maxdist <- max(apply(centered, 1, \(a) sqrt(sum(a ^ 2))))
  centered / maxdist

}


#' Nudge columns of a matrix by fixed amounts
#'
#' @param x a matrix
#' @param nudge a single value or a vector with length equal to the number of columns in x
#'
#' @return matrix
#' @export
#'
#' @examples
#' nudger(matrix(0, nrow = 2, ncol = 2), nudge = c(0,1))
nudger <- function(x, nudge) {
  if (length(nudge) == 1) nudge <- rep(nudge, ncol(x))
  if(length(nudge) != ncol(x)) stop("The nudge parameter must be a single value or a vector the same length as the number of columns in x.")

  x <- x + matrix(rep(nudge, nrow(x)), ncol = ncol(x), byrow = TRUE)
  x
}


#' Rescale each column of a matrix
#'
#' @param x a matrix
#' @param magnitude a single value or a vector with length equal to the number of columns in x
#'
#' @return a matrix
#' @export
#'
#' @examples
#' rescaler(matrix(1, nrow = 2, ncol = 2), magnitude = c(2,3))
rescaler <- function(x, magnitude) {
  if (length(magnitude) == 1) magnitude <- rep(magnitude, ncol(x))
  if(length(magnitude) != ncol(x)) stop("The magnitude parameter must be a single value or a vector the same length as the number of columns in x.")
  x %*% diag(magnitude)

}


rev_matrix_rows <- function(x) {
  x[seq(nrow(x), 1),]
}

rev_matrix_cols <- function(x) {
  x[, seq(ncol(x), 1)]
}

#' Rotate a 2-column matrix
#'
#' @param x a 2-column matrix
#' @param theta angle
#' @param degrees if TRUE, theta is in degrees instead of radians
#' @param center point of rotation
#'
#'
#' @return a rotated 2-column matrix
#' @export
#'
#' @examples
#' x <- matrix(seq(10), ncol = 2)
#' rotater(x, pi)
rotater <- function(x, theta, center = c(0,0), degrees = FALSE) {
  if (!is.numeric(x)) stop("x must be numeric")
  if (!is.numeric(center)) stop("center must be numeric")

  if ("matrix" %in% class(x)) {
    if (ncol(x) != 2) stop("x must be a 2-column matrix or a length-2 vector")
  } else {
    if (length(x) == 2) x = matrix(x, ncol = 2, nrow = 1) else stop("x must be a 2-column matrix or a length-2 vector")
  }

  if ("matrix" %in% class(center)) {
    if (ncol(center) != 2) stop(
      "center must be a 2-column matrix or a length-2 vector")
    if (nrow(center == 1)) center <- matrix(center, nrow = nrow(x), ncol = 2, byrow = TRUE)
    if (nrow(center) != nrow(x)) stop(
      "center must have 1 row or the same number of rows as x")
  } else {
    if (length(center) == 2) center <- matrix(center, nrow = nrow(x), ncol = 2, byrow = TRUE) else stop(
      "center must be a 2-column matrix or a length-2 vector")
  }

  if (degrees) theta <- theta * pi / 180

  # https://www.wikiwand.com/en/Rotation_matrix

  ((x - center) %*%  matrix(c(cos(theta),
                              -sin(theta),
                              sin(theta),
                              cos(theta)),
                            nrow = 2, ncol = 2)) + center
}

plot_arrowhead <- function(x) {
  plot(
    x,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    pch = 16,
    type = "n"
  )
  graphics::polygon(x,
                    border = NA,
                    col = grDevices::adjustcolor("black", alpha.f = .5))
  graphics::rect(
    -1,
    -.1,
    0,
    .1,
    alpha = .2,
    border = NA,
    col = grDevices::adjustcolor("orchid", alpha.f = .5)
  )
  graphics::points(c(0, 1), c(0, 0), pch = 16, col = "red")
}

#' Convert a vector to a matrix
#'
#' @param x vector
#' @param ncol number of columns
#' @param byrow logical. convert by row
v2matrix <- function(x, ncol = 2, byrow = TRUE) {
  matrix(
    x,
    ncol = ncol,
    byrow = byrow,
    dimnames = list(NULL, paste0("x", seq(1, ncol)))
  )
}



#' make arrowhead from list of bezier control points
#'
#' @param x list of control points (as vectors or matrices)
#' @param t sequence of points (usually 0 to 1)
#' @param plot plot arrowhead
#'
#' @return a matrix
#' @export
#'
#' @examples
#' bezier2matrix(list(c( 1,  0,
#'                       1, .5,
#'                      .2,  1),
#'                    c(.2,  1,
#'                     -.5, .5,
#'                       0,  0)),
#'               plot = TRUE)
bezier2matrix <- function(x,
                          t = seq(0, 1, .01),
                          plot = FALSE) {
  xy <- lapply(x, function(p) {
    if (class(p) != "matrix")
      p <- v2matrix(p)

    if (nrow(p) > 2)
      bezier::bezier(t, p = p)
    else
      p
  }) |>
    do.call(what = rbind) |>
    `colnames<-`(c("x", "y"))

  if (plot) {
    plot_arrowhead(xy)
    lapply(x, v2matrix) |>
      purrr::walk(points, pch = 16, col = "dodgerblue")
  }

  xy
}



#' Make arrrowhead with ellipse
#'
#' @param a width of ellipse
#' @param b height of ellipse
#' @param nudge length 2 vector for nudging x and y
#' @param rescale length 2 vector for rescaling x and y
#' @param rotate radians for rotating
#' @param n number of points in ellipse
#' @param plot plot arrowhead
#'
#' @return a matrix
#' @export
#'
#' @examples
#' arrowhead_ellipse(plot = T, b = .5)
#' arrowhead_ellipse(
#'   plot = T,
#'   b = .5,
#'   rescale = .45,
#'   nudge = c(.55, 0)
#' )
arrowhead_ellipse <-
  function(a = 1,
           b = 1,
           nudge = c(0, 0),
           rescale = 1,
           rotate = 0,
           n = 361,
           plot = FALSE) {
    t <- seq(0, 2 * pi, length.out = n)
    xy <- cbind(x = a * cos(t),
                y = b * sin(t)) |>
      matrix2unitcircle() |>
      rescaler(rescale) |>
      nudger(nudge = nudge) |>
      rotater(rotate) |>
      `colnames<-`(c("x", "y"))
    if (plot) {
      plot_arrowhead(xy)
    }
    xy
  }

