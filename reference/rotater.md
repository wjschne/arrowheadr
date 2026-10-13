# Rotate a 2-column matrix

Rotate a 2-column matrix

## Usage

``` r
rotater(x, theta, center = c(0, 0), degrees = FALSE)
```

## Arguments

- x:

  a 2-column matrix

- theta:

  angle

- center:

  point of rotation

- degrees:

  if TRUE, theta is in degrees instead of radians

## Value

a rotated 2-column matrix

## Examples

``` r
x <- matrix(seq(10), ncol = 2)
rotater(x, pi)
#>      [,1] [,2]
#> [1,]   -1   -6
#> [2,]   -2   -7
#> [3,]   -3   -8
#> [4,]   -4   -9
#> [5,]   -5  -10
```
