# Make trefoil arrowhead

Make trefoil arrowhead

## Usage

``` r
arrow_head_trefoil(
  rotate = 0,
  rescale = c(1, 1),
  nudge = c(0, 0),
  transformations = c("unitizer", "rotater", "rescaler", "nudger"),
  n = 361,
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

## Examples

``` r
trefoil <- arrow_head_trefoil(plot = TRUE)
```
