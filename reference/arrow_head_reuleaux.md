# Make reuleaux polygons

Make reuleaux polygons

## Usage

``` r
arrow_head_reuleaux(
  n = 3,
  rotate = 0,
  rescale = c(1, 1),
  nudge = c(0, 0),
  transformations = c("unitizer", "rotater", "rescaler", "nudger"),
  plot = FALSE
)
```

## Arguments

- n:

  number of vertices

- rotate:

  rotation angle in radians

- rescale:

  a single value or 2-length vector for scaling in x and y

- nudge:

  a single value or 2-length vector for nudging in x and y

- transformations:

  a vector of transformation functions

- plot:

  plot arrowhead if TRUE

## Value

a matrix

## Examples

``` r
triangle <- arrow_head_reuleaux(plot = TRUE)

diamond <- arrow_head_reuleaux(n = 4, plot = TRUE)

acute_triangle <- triangle <- arrow_head_reuleaux(rescale = c(1, .5), plot = TRUE)

sharp_diamond <- arrow_head_reuleaux(n = 4, rescale = c(1, .4), plot = TRUE)
```
