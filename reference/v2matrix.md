# Convert a vector to a matrix

Convert a vector to a matrix

## Usage

``` r
v2matrix(x, ncol = 2, byrow = TRUE)
```

## Arguments

- x:

  vector

- ncol:

  number of columns

- byrow:

  logical. convert by row

## Value

a matrix

## Examples

``` r
v2matrix(c(1,2,3,4))
#>      x y
#> [1,] 1 2
#> [2,] 3 4
```
