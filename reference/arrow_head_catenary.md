# Make catenary arrowhead

Make catenary arrowhead

## Usage

``` r
arrow_head_catenary(
  a = 1,
  base_width = 0,
  thickness = 1.2,
  closed = FALSE,
  rotate = 0,
  rescale = c(1, 1),
  nudge = c(0, 0),
  transformations = c("rotater", "rescaler", "nudger"),
  n = 361,
  plot = FALSE
)
```

## Arguments

- a:

  peakedness of the arch (near 0 is more flat, large like parabola)

- base_width:

  width of the base of the arch

- thickness:

  thickness of the top of the arch

- closed:

  if TRUE, closed arch

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
catenary <- arrow_head_catenary(plot = TRUE)

stlouis <-
  arrow_head_catenary(
    plot = TRUE,
    a = 0.4,
    base_width = 0.2,
    thickness = .09
  )


bluntnosed_catenary <-
  arrow_head_catenary(
    plot = TRUE,
    a = .2,
    thickness = 1.2
  )
```
