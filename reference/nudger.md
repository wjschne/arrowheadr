# Nudge columns of a matrix by fixed amounts

Nudge columns of a matrix by fixed amounts

## Usage

``` r
nudger(x, nudge)
```

## Arguments

- x:

  a matrix

- nudge:

  a single value or a vector with length equal to the number of columns
  in x

## Value

matrix

## Examples

``` r
nudger(matrix(0, nrow = 2, ncol = 2), nudge = c(0,1))
#>      [,1] [,2]
#> [1,]    0    1
#> [2,]    0    1
```
