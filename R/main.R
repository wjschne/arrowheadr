#' Fit matrix to unit circle
#'
#' @param x matrix
#' @param center center of matrix
#'
#' @return matrix
#' @export
#'
#' @examples
#' A = matrix(c(1, 2,
#'             -8,6,
#'              9,5),
#'             ncol = 2,
#'             byrow = TRUE)
#' unitizer(A)
#' cA <- unitizer(A, center = colMeans(A))
#' plot(cA, xlim = c(-1, 1), ylim = c(-1, 1))
#' t <- seq(0,2*pi, length.out = 361)
#' lines(cos(t), sin(t))
unitizer <- function(x, center = rep(0, ncol(x))) {
  # Number of rows and columns
  nr <- nrow(x)
  nc <- ncol(x)
  # Centered matrix
  centered <- x - matrix(rep(center, nr), ncol = nc, byrow = TRUE)

  # The maximum distance from the center
  maxdist <- max(apply(centered, 1, function(a) sqrt(sum(a ^ 2))))
  # Rescale by the maximum distance
  result <- centered / maxdist
  colnames(result) <- colnames(x)
  result

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
  if (length(nudge) == 1)
    nudge <- rep(nudge, ncol(x))
  if (length(nudge) != ncol(x))
    stop(
      "The nudge parameter must be a single value or a vector the same length as the number of columns in x."
    )
  result <- x + matrix(rep(nudge, nrow(x)), ncol = ncol(x), byrow = TRUE)
  colnames(result) <- colnames(x)
  result
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
  if (length(magnitude) == 1)
    magnitude <- rep(magnitude, ncol(x))
  if (length(magnitude) != ncol(x))
    stop(
      "The magnitude parameter must be a single value or a vector the same length as the number of columns in x."
    )
  result <- x %*% diag(magnitude)
  colnames(result) <- colnames(x)
  result

}

#' make a reflection of a matrix on the y axis
#'
#' Good for making symmetrical arrowheads
#'
#' @param x matrix
#' @param add_reflection add to x in reverse order
#'
#' @return a matrix with y reversed sign and rows in reverse order
#' @export
#'
#' @examples
#' reflecter(diag(c(1,2)))
reflecter <- function(x, add_reflection = TRUE) {
  reflection <- rev_matrix_rows(x) %*% diag(c(1, -1))
  if (add_reflection) {
    reflection <- rbind(x, reflection)
  }
  colnames(reflection) <- colnames(x)
  reflection
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
rotater <- function(x,
                    theta,
                    center = c(0, 0),
                    degrees = FALSE) {
  if ("matrix" %in% class(x)) {
    if (ncol(x) != 2)
      stop("x must be a 2-column matrix or a length-2 vector")
  } else {
    if (length(x) == 2)
      x <- matrix(x, ncol = 2, nrow = 1)
    else
      stop("x must be a 2-column matrix or a length-2 vector")
  }

  if ("matrix" %in% class(center)) {
    if (ncol(center) != 2)
      stop("center must be a 2-column matrix or a length-2 vector")
    if (nrow(center == 1))
      center <- matrix(center,
                       nrow = nrow(x),
                       ncol = 2,
                       byrow = TRUE)
    if (nrow(center) != nrow(x))
      stop("center must have 1 row or the same number of rows as x")
  } else {
    if (length(center) == 2)
      center <- matrix(center,
                       nrow = nrow(x),
                       ncol = 2,
                       byrow = TRUE)
    else
      stop("center must be a 2-column matrix or a length-2 vector")
  }

  if (degrees)
    theta <- theta * pi / 180

  # https://www.wikiwand.com/en/Rotation_matrix
  rotation_matrix <- matrix(c(cos(theta),
                              -sin(theta),
                              sin(theta),
                              cos(theta)),
                            nrow = 2,
                            ncol = 2)

  result <- ((x - center) %*% rotation_matrix) + center
  colnames(result) <- colnames(x)
  result
}

#' Do transformations in a desired order
#'
#' @param x a 2-column matrix
#' @param rescale a single value or a vector with length equal to the number of columns in x
#' @param rotate angle in radians unless degrees is true
#' @param nudge a single value or a vector with length equal to the number of columns in x
#' @param center a single value or a vector with length equal to the number of columns in x
#' @param degrees if TRUE, angles are degrees instead of radians
#' @param transformations a vector of transformation functions
#'
#' @return a matrix
#' @export
#'
#' @examples
#'
#' xy <- matrix(c(0,0,1,1), nrow = 2)
#' transformer(xy, transformations = "rotater", rotate = pi)
transformer <- function(x,
                        rescale = c(1, 1),
                        rotate = 0,
                        nudge = 0,
                        center = c(0, 0),
                        degrees = FALSE,
                        transformations = c("unitizer",
                                            "rescaler",
                                            "nudger",
                                            "rotater")) {
  cnames <- colnames(x)

  for (f in transformations) {
    args <- list(x = x)
    if (f == "unitizer")
      args <- append(args, list(center = center))
    if (f == "rescaler")
      args <- append(args, list(magnitude = rescale))
    if (f == "nudger")
      args <- append(args, list(nudge = nudge))
    if (f == "rotater")
      args <- append(args, list(
        theta = rotate,
        center = center,
        degrees = degrees
      ))

    x <- do.call(what = f, args = args)
  }
  colnames(x) <- cnames
  x

}

#' Convert a vector to a matrix
#'
#' @param x vector
#' @param ncol number of columns
#' @param byrow logical. convert by row
#'
#' @return a matrix
#' @export
#'
#' @examples
#' v2matrix(c(1,2,3,4))
v2matrix <- function(x, ncol = 2, byrow = TRUE) {
  if (ncol == 2)
    vnames <- c("x", "y")
  else
    vnames <- paste0("x", seq(ncol))

  xy <- matrix(x, ncol = ncol, byrow = byrow)
  xy <- `colnames<-`(xy, vnames)
  xy
}


#' reverses the order of rows or columns in a matrix
#'
#' @param x matrix
#'
#' @return a matrix
#' @export
#'
#' @examples
#' rev_matrix_rows(diag(c(1,2)))
rev_matrix_rows <- function(x) {
  x[seq(nrow(x), 1), ]
}

#' @export
#' @rdname rev_matrix_rows
rev_matrix_cols <- function(x) {
  x[, seq(ncol(x), 1)]
}




#' Plot arrowhead
#'
#' @param x 2-column matrix
#' @param displayline plot the display line
#' @param displaypoints plot the 0,0 point and the 1,0 point
#'
#' @return plot
#' @export
plot_arrowhead <- function(x,
                           displayline = TRUE,
                           displaypoints = TRUE) {
  # plot axes
  plot(
    x,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    pch = 16,
    type = "n",
    asp = 1
  )
  # plot polygon
  graphics::polygon(x,
                    border = NA,
                    col = grDevices::adjustcolor("black", alpha.f = .5))

  if (displayline) {
    # arrow line as rectangle
    graphics::rect(
      xleft =  -1,
      ybottom =       -.1,
      xright = 0,
      ytop = .1,
      border = NA,
      col = grDevices::adjustcolor("orchid", alpha.f = .5)
    )
  }

  if (displaypoints) {
    # center and arrow tip
    graphics::points(c(0, 1), c(0, 0), pch = 16, col = "red")
  }
}


#' Does nothing but provide defaults for documentation
#'
#' @param rotate rotation angle in radians
#' @param rescale a single value or 2-length vector for scaling in x and y
#' @param nudge a single value or 2-length vector for nudging in x and y
#' @param transformations a vector of transformation functions
#' @param n number of points in polygon
#' @param plot plot arrowhead if TRUE
#'
#' @keywords internal
#'
#' @return a matrix
#' @export
arrow_head_default <- function(rotate = 0,
                               rescale = c(1, 1),
                               nudge = c(0, 0),
                               transformations = c("unitizer",
                                                   "rotater",
                                                   "rescaler",
                                                   "nudger"),
                               n = 100,
                               plot = FALSE) {

}



#' make arrowhead from list of bezier control points
#'
#' @param x list of control points (as vectors or matrices)
#' @inheritParams arrow_head_default
#' @param n number of points in each bezier curve
#' @param show_controls show control points if plot = TRUE
#'
#' @return a matrix
#' @export
#'
#' @examples
#' curved_arrowhead <- arrow_head_bezier(list(
#'   c(1,  0,
#'     .5, .5,
#'     .2, .5),
#'   c(.2, .5,
#'     .2, .1,
#'     -.1, .25,
#'     -.3, .25),
#'   c(-.3, .25,
#'     0, 0,
#'     -.3, -.25),
#'   c(-.3, -.25,
#'     -.1, -.25,
#'     .2,  -.1,
#'     .2, -.5),
#'   c(.2, -.5,
#'     .5, -.5,
#'     1,  0)
#' ),
#' plot = TRUE)
arrow_head_bezier <- function(x,
                              rotate = 0,
                              rescale = c(1, 1),
                              nudge = c(0, 0),
                              transformations = c("rotater",
                                                  "rescaler",
                                                  "nudger"),
                              n = 101,
                              plot = FALSE,
                              show_controls = TRUE) {
  t <- seq(0, 1, length.out = n)

  controls <- purrr::map(x, function(p) {
    if (!is.matrix(p)) {
      p <- v2matrix(p)
    }
    p
  })

  xy <- purrr::map(controls, function(p) {
    if (nrow(p) > 2)
      bezier::bezier(t, p = p)
    else
      p
  })

  xy <- do.call(xy, what = rbind)

  xy <- `colnames<-`(xy, c("x", "y"))

  xy <- transformer(xy,
      rescale = rescale,
      rotate = rotate,
      nudge = nudge,
      transformations = transformations
    )

  if (plot) {
    plot_arrowhead(xy)
    if (show_controls) {
      xy_controls <- do.call(controls, what = rbind)
      xy_controls <- `colnames<-`(xy_controls, c("x", "y"))
      transformer(xy_controls,
          rescale = rescale,
          rotate = rotate,
          nudge = nudge,
          transformations = transformations
        )
      graphics::points(
        xy_controls,
        pch = 16,
        col = "dodgerblue",
        xlim = c(-1, 1)
      )
      graphics::lines(xy_controls,
                      col = "dodgerblue",
                      xlim = c(-1, 1))
    }

  }

  xy
}



#' Make arrrowhead with ellipse
#'
#' @param a width of ellipse
#' @param b height of ellipse
#' @param superellipse parameter for specifying superellipses. Can be of length 1 or 2
#' @inheritParams arrow_head_default
#'
#' @return a matrix
#' @export
#'
#' @examples
#' ellipsehead <- arrow_head_ellipse(plot = TRUE, b = .5)
#' ellipsehead_spaced <- arrow_head_ellipse(
#'   plot = TRUE,
#'   b = .5,
#'   rescale = .45,
#'   nudge = c(.55, 0)
#' )
#' # Make regular polygon with n - 1 sides
#' pentagon <- arrow_head_ellipse(n = 6, plot = TRUE)
#' # make a superellipses
#' star4 <- arrow_head_ellipse(superellipse = .5, plot = TRUE)
#' squircle <- arrow_head_ellipse(superellipse = 3, plot = TRUE, rotate = pi / 4)
#' longboat <- arrow_head_ellipse(plot = TRUE, b = 1, a = 4, superellipse = c(3,.5))
arrow_head_ellipse <- function(a = 1,
                               b = 1,
                               superellipse = 2,
                               rotate = 0,
                               rescale = c(1, 1),
                               nudge = c(0, 0),
                               transformations = c("unitizer", "rotater", "rescaler", "nudger"),
                               n = 361,
                               plot = FALSE) {
  t <- seq(0, 2 * pi, length.out = n)
  if (length(superellipse == 1)) {
    superellipse <- rep(superellipse, 2)
  } else if (!(length(superellipse) %in% c(1, 2))) {
    stop("superellipse must be of length 1 or 2")
  }
  xy <- cbind(x = a * sign(cos(t)) * (abs(cos(t)) ^ (2 / superellipse[1])),
              y = b * sign(sin(t)) * (abs(sin(t)) ^ (2 / superellipse[2])))

  xy <- transformer(xy,
      rescale = rescale,
      rotate = rotate,
      nudge = nudge,
      transformations = transformations
    )
  xy <- `colnames<-`(xy, c("x", "y"))
  if (plot) {
    plot_arrowhead(xy)
  }
  xy
}

#' Make a harpoon arrowhead
#'
#' @param point_angle angle of harpoon point
#' @param barb_angle angle of harpoon barb
#' @param degrees if TRUE, angles are in degrees instead of radians
#' @inheritParams arrow_head_default
#'
#' @return a matrix
#' @export
#'
#' @examples
#' xy <- arrow_head_harpoon(plot = TRUE)
arrow_head_harpoon <- function(point_angle = 30,
                               barb_angle = 20,
                               degrees = TRUE,
                               rotate = 0,
                               rescale = c(1, 1),
                               nudge = c(0, 0),
                               transformations = c("unitizer",
                                                   "rotater",
                                                   "rescaler",
                                                   "nudger"),
                               plot = FALSE) {
  if (degrees) {
    barb_angle <- barb_angle * pi / 180
    point_angle <- point_angle * pi / 180
  }
  p1 <- c(1, 0)
  p2 <- p1 + c(cos(pi - point_angle), sin(pi - point_angle))
  p3_y <- 0
  m1 <- tan(-(point_angle + barb_angle))

  l23_intercept <- p2[2] -  m1 * p2[1]
  p3_x <- (p3_y - l23_intercept) / m1
  p3 <- c(p3_x, p3_y)

  xy <- c(p1, p2, p3)
  xy <- v2matrix(xy)
  xy <- nudger(xy, c(-p3[1], 0))
  xy <- rescaler(xy, 1 / (1 - p3[1]))
  xy <- `colnames<-`(xy, c("x", "y"))
  xy <- transformer(xy,
      rescale = rescale,
      rotate = rotate,
      nudge = nudge,
      transformations = transformations
    )

  if (plot) {
    plot_arrowhead(xy)
  }

  xy



}



#' Make spirograph arrowheads
#'
#' @param r cycling circle radius
#' @param R fixed circle radius
#' @param d pen distance
#' @param windings windings
#' @inheritParams arrow_head_default
#'
#' @return a matrix
#' @export
#'
#' @examples
#' star5 <- arrow_head_hypotrochoid(plot = TRUE, rotate = pi)
#' star5_long <- arrow_head_hypotrochoid(
#'    plot = TRUE,
#'    r = 4,
#'    R = 3,
#'    rotate = pi,
#'    rescale = c(1, .4)
#'    )
#' deltoid_long <- arrow_head_deltoid(plot = TRUE, rescale = c(1,1))
#' deltoid_long <- arrow_head_deltoid(plot = TRUE)
#' deltoid_spaced <- arrow_head_deltoid(plot = TRUE,
#'                                     rescale = c(.6,.3),
#'                                     nudge = c(.4, 0))
arrow_head_hypotrochoid <- function(r = 4,
                                    R = 3,
                                    d = r,
                                    windings = r,
                                    rotate = 0,
                                    rescale = c(1, 1),
                                    nudge = c(0, 0),
                                    transformations = c("unitizer",
                                                        "rotater",
                                                        "rescaler",
                                                        "nudger"),
                                    n = 361,
                                    plot = FALSE) {
  theta <- seq(0, windings * 2 * pi, length.out = abs(n * windings))
  x <- (R - r) * cos(theta) + d * cos(theta * (R - r) / r)
  y <- (R - r) * sin(theta) + d * sin(theta * (R - r) / r)

  xy <- cbind(x = x, y = y)
  xy <- transformer(xy,
      rescale = rescale,
      rotate = rotate,
      nudge = nudge,
      transformations = transformations
    )

  if (plot) {
    plot_arrowhead(xy)
  }

  xy

}

#' @export
#' @rdname arrow_head_hypotrochoid
arrow_head_deltoid <- function(d = 2.6,
                               rotate = pi,
                               rescale = c(1, .5),
                               nudge = c(0, 0),
                               transformations = c("unitizer",
                                                   "rotater",
                                                   "rescaler",
                                                   "nudger"),
                               n = 361,
                               plot = FALSE) {
  arrow_head_hypotrochoid(
    r = 2,
    R = 1,
    d = d,
    rescale = rescale,
    rotate = rotate,
    nudge = nudge,
    transformations = transformations,
    n = n,
    plot = plot
  )
}


#' Make arrowhead with Wittengenstein's Rod
#'
#' See https://en.wikipedia.org/wiki/Wittgenstein's_rod
#'
#' @param fixed_point x and y coordinates of a point
#' @param rod_length Length of rod
#' @inheritParams arrow_head_default
#'
#' @return a matrix
#' @export
#'
#' @examples
#' candleflame <- arrow_head_wittgenstein_rod(
#'   fixed_point = c(-2.75, 0),
#'   rod_length = 3.75,
#'   nudge = c(1, 0),
#'   rescale = .95,
#'   plot = TRUE
#' )
#'
#'
#' rocket <- arrow_head_wittgenstein_rod(
#'   fixed_point = c(1.1, 0),
#'   rod_length = 2.1,
#'   plot = TRUE,
#'   nudge = c(.1, 0),
#'   rescale = c(.90, .25)
#' )
arrow_head_wittgenstein_rod <- function(
    fixed_point = c(1.1, 0),
    rod_length = 2.1,
    rotate = 0,
    rescale = c(1, 1),
    nudge = c(0, 0),
    transformations = c("unitizer", "rotater", "rescaler", "nudger"),
    n = 361,
    plot = FALSE) {
  t <- seq(0, 2 * pi, length.out = n)
  cx <- cos(t)
  cy <- sin(t)
  fpx <- fixed_point[1]
  fpy <- fixed_point[2]
  d <- sqrt((cx - fpx) ^ 2 + (cy - fpy) ^ 2)
  x <- ((fpx - cx) / d) * rod_length  + cx
  y <- ((fpy - cy) / d) * rod_length + cy
  xy <- cbind(x = x - fpx, y = y - fpy)
  xy <- transformer(xy,
      rescale = rescale,
      rotate = rotate,
      nudge = nudge,
      transformations = transformations
    )

  if (plot) {
    plot_arrowhead(xy)
  }

  xy
}

#' Make trefoil arrowhead
#'
#' @inheritParams arrow_head_default
#'
#' @return a matrix
#' @export
#'
#' @examples
#' trefoil <- arrow_head_trefoil(plot = TRUE)
arrow_head_trefoil <- function(rotate = 0,
                               rescale = c(1, 1),
                               nudge = c(0, 0),
                               transformations = c("unitizer",
                                                   "rotater",
                                                   "rescaler",
                                                   "nudger"),
                               n = 361,
                               plot = FALSE) {
  t <- seq(0, 2 * pi, length.out = n)
  x <- sin(t) + 2 * sin(2 * t)
  y <- cos(t) - 2 * cos(2 * t)
  xy <- cbind(x = x, y = y)
  xy <- rotater(xy, theta = pi / 2)
  xy <- transformer(xy,
      rescale = rescale,
      rotate = rotate,
      nudge = nudge,
      transformations = transformations
    )

  if (plot) {
    plot_arrowhead(xy)
  }

  xy

}


#' Make catenary arrowhead
#'
#' @param a peakedness of the arch (near 0 is more flat, large like parabola)
#' @param base_width width of the base of the arch
#' @param thickness thickness of the top of the arch
#' @param closed if TRUE, closed arch
#' @inheritParams arrow_head_default
#'
#' @return a matrix
#' @export
#'
#' @examples
#' catenary <- arrow_head_catenary(plot = TRUE)
#' stlouis <-
#'   arrow_head_catenary(
#'     plot = TRUE,
#'     a = 0.4,
#'     base_width = 0.2,
#'     thickness = .09
#'   )
#'
#' bluntnosed_catenary <-
#'   arrow_head_catenary(
#'     plot = TRUE,
#'     a = .2,
#'     thickness = 1.2
#'   )
arrow_head_catenary <- function(a = 1,
                                base_width = 0,
                                thickness = 1.2,
                                closed = FALSE,
                                rotate = 0,
                                rescale = c(1, 1),
                                nudge = c(0, 0),
                                transformations = c("rotater", "rescaler", "nudger"),
                                n = 361,
                                plot = FALSE) {
  x <- seq(-1, 1, length.out = n)
  y <- a * cosh(x / a)
  y <- 1 - (y - min(y)) / (max(y) - min(y))

  if (!closed) {
    lb <- -1 + base_width
    ub <- 1 - base_width
    x1 <- rev(seq(lb, ub, length.out = n))
    y1 <- rev(y) * (1 - thickness / 2)
    x <- c(x, x1)
    y <- c(y, y1)

  }
  xy <- cbind(x = 2 * y - 1, y = x)
  xy <- transformer(xy,
      rescale = rescale,
      rotate = rotate,
      nudge = nudge,
      transformations = transformations
    )

  if (plot) {
    plot_arrowhead(xy)
  }

  xy

}

#' Make latex arrowhead
#'
#' Mimics tikz's latex arrowheads, but can make any arrowhead with 2 side curves and an underside.
#'
#' @param point length-2 vector for point coordinates
#' @param sidecontrols vector of coordinates for control points on sides
#' @param p_barb length-2 vector for barb coordinates
#' @param undercontrols vector of coordinates for control points on underside
#' @inheritParams arrow_head_default
#'
#' @return a matrix
#' @export
#'
#' @examples
#' latex_prime <- arrow_head_latex(plot = TRUE)
#' latex_prime_spaced <-
#'   arrow_head_latex(nudge = c(.45, 0),
#'                   rescale = .55,
#'                   plot = TRUE)
#' latex_regular <- arrow_head_latex(undercontrols = NULL, plot = TRUE)
#'
#'
#' latex_flat <- arrow_head_latex(sidecontrols = NULL, plot = TRUE)
#' latex_pincer <- arrow_head_latex(
#'   sidecontrols = c(-.5,1, -.5, 2),
#'   undercontrols = c(.2,1.5),
#'   p_barb = c(-1,.5),
#'   nudge = c(.35,0),
#'   rescale = c(.65,.4),
#'   plot = TRUE)
arrow_head_latex <- function(point = c(1, 0),
                             sidecontrols = c(7 / 12, 1 / 12, -1 / 6, 1 / 4),
                             p_barb = c(-2 / 3, 5 / 8),
                             undercontrols = c(-1 / 4, 1 / 6),
                             rotate = 0,
                             rescale = c(1, 1),
                             nudge = c(0, 0),
                             transformations = c("rotater", "rescaler", "nudger"),
                             n = 101,
                             plot = FALSE) {
  leftside <- v2matrix(c(point, sidecontrols, p_barb))

  under_side <- reflecter(v2matrix(c(p_barb, undercontrols)))


  rightside <- reflecter(leftside, add_reflection = FALSE)




  controls <- list(leftside = leftside,
                   under_side = under_side,
                   rightside = rightside)

  xy <- arrow_head_bezier(controls)
  xy <- transformer(xy,
      rescale = rescale,
      rotate = rotate,
      nudge = nudge,
      transformations = transformations
    )

  if (plot) {
    plot_arrowhead(xy)
    xy_controls <- do.call(controls, what = rbind)
    xy_controls <- `colnames<-`(xy_controls, c("x", "y"))
    xy_controls <- transformer(xy_controls,
        rescale = rescale,
        rotate = rotate,
        nudge = nudge,
        transformations = transformations
      )
    graphics::lines(xy_controls, col = "dodgerblue")
    graphics::points(xy_controls, pch = 16, col = "dodgerblue")
  }

  xy
}

#' Make arrowhead from preset icon
#'
#' @param x name of icon: eiffel, viper, viper2, nighthawk, pantherxf70
#' @inheritParams arrow_head_default
#'
#' @return a matrix
#' @export
#'
#' @examples
#' starwars_stardestoyer <- arrow_head_icon(x = "stardestoyer", plot = TRUE)
#' starwars_executor <- arrow_head_icon(x = "executor", plot = TRUE)
#' eiffel <- arrow_head_icon(x = "eiffel", plot = TRUE)
#' battlestar_galactica_viper <- arrow_head_icon(x = "viper", plot = TRUE)
#' battlestar_galactica_viper2 <- arrow_head_icon(x = "viper2", plot = TRUE)
#' nighthawk <- arrow_head_icon(x = "nighthawk", plot = TRUE)
#' panther_xf70 <- arrow_head_icon(x = "pantherxf70", plot = TRUE)
arrow_head_icon <- function(x = "stardestoyer",
                            rotate = 0,
                            rescale = c(1, 1),
                            nudge = c(0, 0),
                            transformations = c("rotater", "rescaler", "nudger"),
                            plot = FALSE) {
  xy <- transformer(as.matrix(icons[icons$icon == x, c("x", "y")]),
      rescale = rescale,
      rotate = rotate,
      nudge = nudge,
      transformations = transformations
    )

  if (plot) {
    plot_arrowhead(xy)
  }

  xy
}






#' Make arrowheads with any function
#'
#' @param .fun a function (defaults to dnorm)
#' @param lower_bound lowest value passed to .fun
#' @param upper_bound highest value passed to .fun
#' @param ... arguments passed to .fun
#' @param base_width If closed, size of feet
#' @param thickness If closed, thickness of shape (can be negative)
#' @param closed make polygon closed
#' @param minimum_value smallest value in function
#' @inheritParams arrow_head_default
#'
#' @return a matrix
#' @export
#'
#' @examples
#' # A normal distribution
#' xy <- arrow_head_function(dnorm, plot = TRUE)
#' # if closed = FALSE, set thickness and base_width
#' xy <- arrow_head_function(dnorm, plot = TRUE, closed = FALSE,
#'                     thickness = 1.5,
#'                     base_width = .25)
#'
#' # A cauchy distribution
#' xy <- arrow_head_function(dt, df = 1, plot = TRUE)
#' # open with thickness = 1.5
#' xy <- arrow_head_function(
#'   dt,
#'   df = 1,
#'   plot = TRUE,
#'   closed = FALSE,
#'   thickness = 1.5
#' )
#' # thickness > 2 creates a bulge
#' xy <- arrow_head_function(
#'   dt,
#'   df = 1,
#'   lower_bound = -3.25,
#'   upper_bound = 3.25,
#'   closed = FALSE,
#'   thickness = 2.5,
#'   plot = TRUE,
#'   rescale = 1 / 3,
#'   nudge = c(2 / 3, 0)
#' )
#'
#' # Make a new function
#' mytrident <- function(x, s = 160) {
#'   k <- length(x)
#'   y1 <- dbeta(x, shape1 = s, shape2 = s) * 2
#'   y2 <- dbeta(x, shape1 = s * .9, shape2 = s * .1)
#'   y3 <- dbeta(x, shape1 = s * .1 , shape2 = s * .9)
#'   y1 + y2 + y3
#' }
#'
#' xy <- arrow_head_function(
#'   mytrident,
#'   lower_bound = 0,
#'   upper_bound = 1,
#'   plot = TRUE,
#'   minimum_value = -3,
#'   rescale = .5,
#'   nudge = c(.5, 0)
#' )
arrow_head_function <- function(.fun = stats::dnorm,
                                lower_bound = -4,
                                upper_bound = 4,
                                ...,
                                base_width = 0,
                                thickness = 1.2,
                                closed = TRUE,
                                minimum_value = NA,
                                rotate = 0,
                                rescale = c(1, 1),
                                nudge = c(0, 0),
                                transformations = c("rotater", "rescaler", "nudger"),
                                n = 1001,
                                plot = FALSE) {
  x <- seq(lower_bound, upper_bound, length.out = n)
  y <- .fun(x, ...)
  if (!is.na(minimum_value)) {
    x <- c(lower_bound, x, upper_bound)
    y <- c(minimum_value, y, minimum_value)
  }

  x <- 2 * (x - min(x)) / (max(x) - min(x)) - 1

  y <- (y - min(y)) / (max(y) - min(y))

  if (!closed) {
    lb <- -1 + base_width
    ub <- 1 - base_width
    x1 <- rev(seq(lb, ub, length.out = n))
    y1 <- rev(y) * (1 - thickness / 2)
    x <- c(x, x1)
    y <- c(y, y1)

  }
  y <- 2 * y - 1
  xy <- transformer(cbind(x = y, y = x),
      rescale = rescale,
      rotate = rotate,
      nudge = nudge,
      transformations = transformations
    )

  if (plot) {
    plot_arrowhead(xy)
  }

  xy

}
