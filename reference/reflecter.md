# make a reflection of a matrix on the y axis

Good for making symmetrical arrowheads

## Usage

``` r
reflecter(x, add_reflection = TRUE)
```

## Arguments

- x:

  matrix

- add_reflection:

  add to x in reverse order

## Value

a matrix with y reversed sign and rows in reverse order

## Examples

``` r
reflecter(diag(c(1,2)))
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    2
#> [3,]    0   -2
#> [4,]    1    0
```
