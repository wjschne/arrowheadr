# Make a harpoon arrowhead

Make a harpoon arrowhead

## Usage

``` r
arrow_head_harpoon(
  point_angle = 30,
  barb_angle = 20,
  degrees = TRUE,
  rotate = 0,
  rescale = c(1, 1),
  nudge = c(0, 0),
  transformations = c("unitizer", "rotater", "rescaler", "nudger"),
  plot = FALSE
)
```

## Arguments

- point_angle:

  angle of harpoon point

- barb_angle:

  angle of harpoon barb

- degrees:

  if TRUE, angles are in degrees instead of radians

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
xy <- arrow_head_harpoon(plot = TRUE)
```
