# Make arrowhead from preset icon

Make arrowhead from preset icon

## Usage

``` r
arrow_head_icon(
  x = "stardestoyer",
  rotate = 0,
  rescale = c(1, 1),
  nudge = c(0, 0),
  transformations = c("rotater", "rescaler", "nudger"),
  plot = FALSE
)
```

## Arguments

- x:

  name of icon: chippenham, eiffel, enterprise, executor, flame,
  gateway, latex, latexprime, leaf, lozenge, nighthawk, pantherxf70,
  saxon, spade, stardestoyer, vikingleaf, viper, viper2

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
starwars_stardestoyer <- arrow_head_icon(x = "stardestoyer", plot = TRUE)

starwars_executor <- arrow_head_icon(x = "executor", plot = TRUE)

eiffel <- arrow_head_icon(x = "eiffel", plot = TRUE)

battlestar_galactica_viper <- arrow_head_icon(x = "viper", plot = TRUE)

battlestar_galactica_viper2 <- arrow_head_icon(x = "viper2", plot = TRUE)

nighthawk <- arrow_head_icon(x = "nighthawk", plot = TRUE)

panther_xf70 <- arrow_head_icon(x = "pantherxf70", plot = TRUE)

leaf <- arrow_head_icon(x = "leaf", plot = TRUE)

lozenge <- arrow_head_icon(x = "lozenge", plot = TRUE)

spade <- arrow_head_icon(x = "spade", plot = TRUE)
```
