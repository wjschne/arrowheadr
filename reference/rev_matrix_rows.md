# reverses the order of rows or columns in a matrix

reverses the order of rows or columns in a matrix

## Usage

``` r
rev_matrix_rows(x)

rev_matrix_cols(x)
```

## Arguments

- x:

  matrix

## Value

a matrix

## Examples

``` r
rev_matrix_rows(diag(c(1,2)))
#>      [,1] [,2]
#> [1,]    0    2
#> [2,]    1    0
```
