# Rescale each column of a matrix

Rescale each column of a matrix

## Usage

``` r
rescaler(x, magnitude)
```

## Arguments

- x:

  a matrix

- magnitude:

  a single value or a vector with length equal to the number of columns
  in x

## Value

a matrix

## Examples

``` r
rescaler(matrix(1, nrow = 2, ncol = 2), magnitude = c(2,3))
#>      [,1] [,2]
#> [1,]    2    3
#> [2,]    2    3
```
