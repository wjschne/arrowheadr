# Make latex arrowhead

Mimics tikz's latex arrowheads, but can make any arrowhead with 2 side
curves and an underside.

## Usage

``` r
arrow_head_latex(
  point = c(1, 0),
  sidecontrols = c(7/12, 1/12, -1/6, 1/4),
  p_barb = c(-2/3, 5/8),
  undercontrols = c(-1/4, 1/6),
  rotate = 0,
  rescale = c(1, 1),
  nudge = c(0, 0),
  transformations = c("rotater", "rescaler", "nudger"),
  n = 101,
  plot = FALSE
)
```

## Arguments

- point:

  length-2 vector for point coordinates

- sidecontrols:

  vector of coordinates for control points on sides

- p_barb:

  length-2 vector for barb coordinates

- undercontrols:

  vector of coordinates for control points on underside

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
latex_prime <- arrow_head_latex(plot = TRUE)

latex_prime_spaced <-
  arrow_head_latex(nudge = c(.45, 0),
                  rescale = .55,
                  plot = TRUE)

latex_regular <- arrow_head_latex(undercontrols = NULL, plot = TRUE)



latex_flat <- arrow_head_latex(sidecontrols = NULL, plot = TRUE)

latex_pincer <- arrow_head_latex(
  sidecontrols = c(-.5,1, -.5, 2),
  undercontrols = c(.2,1.5),
  p_barb = c(-1,.5),
  nudge = c(.35,0),
  rescale = c(.65,.4),
  plot = TRUE)
```
