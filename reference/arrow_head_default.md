# Does nothing but provide defaults for documentation

Does nothing but provide defaults for documentation

## Usage

``` r
arrow_head_default(
  rotate = 0,
  rescale = c(1, 1),
  nudge = c(0, 0),
  transformations = c("unitizer", "rotater", "rescaler", "nudger"),
  n = 100,
  plot = FALSE
)
```

## Arguments

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
