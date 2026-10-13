# Do transformations in a desired order

Do transformations in a desired order

## Usage

``` r
transformer(
  x,
  rescale = c(1, 1),
  rotate = 0,
  nudge = 0,
  center = c(0, 0),
  degrees = FALSE,
  transformations = c("unitizer", "rescaler", "nudger", "rotater")
)
```

## Arguments

- x:

  a 2-column matrix

- rescale:

  a single value or a vector with length equal to the number of columns
  in x

- rotate:

  angle in radians unless degrees is true

- nudge:

  a single value or a vector with length equal to the number of columns
  in x

- center:

  a single value or a vector with length equal to the number of columns
  in x

- degrees:

  if TRUE, angles are degrees instead of radians

- transformations:

  a vector of transformation functions

## Value

a matrix

## Examples

``` r

xy <- matrix(c(0,0,1,1), nrow = 2)
transformer(xy, transformations = "rotater", rotate = pi)
#>               [,1] [,2]
#> [1,] -1.224647e-16   -1
#> [2,] -1.224647e-16   -1
```
