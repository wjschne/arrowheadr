# Fit matrix to unit circle

Fit matrix to unit circle

## Usage

``` r
unitizer(x, center = rep(0, ncol(x)))
```

## Arguments

- x:

  matrix

- center:

  center of matrix

## Value

matrix

## Examples

``` r
A = matrix(c(1, 2,
            -8,6,
             9,5),
            ncol = 2,
            byrow = TRUE)
unitizer(A)
#>             [,1]      [,2]
#> [1,]  0.09712859 0.1942572
#> [2,] -0.77702869 0.5827715
#> [3,]  0.87415728 0.4856429
cA <- unitizer(A, center = colMeans(A))
plot(cA, xlim = c(-1, 1), ylim = c(-1, 1))
t <- seq(0,2*pi, length.out = 361)
lines(cos(t), sin(t))
```
