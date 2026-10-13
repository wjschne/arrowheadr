# make arrowhead from list of bezier control points

make arrowhead from list of bezier control points

## Usage

``` r
arrow_head_bezier(
  x,
  rotate = 0,
  rescale = c(1, 1),
  nudge = c(0, 0),
  transformations = c("rotater", "rescaler", "nudger"),
  n = 101,
  plot = FALSE,
  show_controls = TRUE
)
```

## Arguments

- x:

  list of control points (as vectors or matrices)

- rotate:

  rotation angle in radians

- rescale:

  a single value or 2-length vector for scaling in x and y

- nudge:

  a single value or 2-length vector for nudging in x and y

- transformations:

  a vector of transformation functions

- n:

  number of points in each bezier curve

- plot:

  plot arrowhead if TRUE

- show_controls:

  show control points if plot = TRUE

## Value

a matrix

## Examples

``` r
curved_arrowhead <- arrow_head_bezier(list(
  c(1,  0,
    .5, .5,
    .2, .5),
  c(.2, .5,
    .2, .1,
    -.1, .25,
    -.3, .25),
  c(-.3, .25,
    0, 0,
    -.3, -.25),
  c(-.3, -.25,
    -.1, -.25,
    .2,  -.1,
    .2, -.5),
  c(.2, -.5,
    .5, -.5,
    1,  0)
),
plot = TRUE)
```
