# Make arrowhead with Wittengenstein's Rod

See https://en.wikipedia.org/wiki/Wittgenstein's_rod

## Usage

``` r
arrow_head_wittgenstein_rod(
  fixed_point = c(1.1, 0),
  rod_length = 2.1,
  rotate = 0,
  rescale = c(1, 1),
  nudge = c(0, 0),
  transformations = c("unitizer", "rotater", "rescaler", "nudger"),
  n = 361,
  plot = FALSE
)
```

## Arguments

- fixed_point:

  x and y coordinates of a point

- rod_length:

  Length of rod

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
candleflame <- arrow_head_wittgenstein_rod(
  fixed_point = c(-2.75, 0),
  rod_length = 3.75,
  nudge = c(1, 0),
  rescale = .95,
  plot = TRUE
)



rocket <- arrow_head_wittgenstein_rod(
  fixed_point = c(1.1, 0),
  rod_length = 2.1,
  plot = TRUE,
  nudge = c(.1, 0),
  rescale = c(.90, .25)
)
```
