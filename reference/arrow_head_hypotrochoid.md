# Make spirograph arrowheads

Make spirograph arrowheads

## Usage

``` r
arrow_head_hypotrochoid(
  r = 4,
  R = 3,
  d = r,
  windings = r,
  rotate = 0,
  rescale = c(1, 1),
  nudge = c(0, 0),
  transformations = c("unitizer", "rotater", "rescaler", "nudger"),
  n = 361,
  plot = FALSE
)

arrow_head_deltoid(
  d = 2.6,
  rotate = pi,
  rescale = c(1, 0.5),
  nudge = c(0, 0),
  transformations = c("unitizer", "rotater", "rescaler", "nudger"),
  n = 361,
  plot = FALSE
)
```

## Arguments

- r:

  cycling circle radius

- R:

  fixed circle radius

- d:

  pen distance

- windings:

  windings

- rotate:

  rotation angle in radians

- rescale:

  a single value or 2-length vector for scaling in x and y

- nudge:

  a single value or 2-length vector for nudging in x and y

- transformations:

  a vector of transformation functions

- n:

  number of points in polygon

- plot:

  plot arrowhead if TRUE

## Value

a matrix

## Examples

``` r
star5 <- arrow_head_hypotrochoid(plot = TRUE, rotate = pi)

star5_long <- arrow_head_hypotrochoid(
   plot = TRUE,
   r = 4,
   R = 3,
   rotate = pi,
   rescale = c(1, .4)
   )

deltoid_long <- arrow_head_deltoid(plot = TRUE, rescale = c(1,1))

deltoid_long <- arrow_head_deltoid(plot = TRUE)

deltoid_spaced <- arrow_head_deltoid(plot = TRUE,
                                    rescale = c(.6,.3),
                                    nudge = c(.4, 0))
```
