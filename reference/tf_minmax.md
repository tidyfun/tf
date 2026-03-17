# Depth-based minimum, maximum and range for `tf` vectors

By default, `min`, `max`, and `range` compute **pointwise** extremes
(the existing behaviour). When a `depth` argument is supplied, they
instead return the most extreme / most central observation according to
the chosen depth. For the default `"MHI"` depth this gives the lowest /
highest function in an up-down sense.

## Usage

``` r
# S3 method for class 'tf'
min(..., na.rm = FALSE, depth = NULL)

# S3 method for class 'tf'
max(..., na.rm = FALSE, depth = NULL)

# S3 method for class 'tf'
range(..., na.rm = FALSE, depth = NULL)
```

## Arguments

- ...:

  `tf` objects (and `na.rm` for the pointwise default).

- na.rm:

  logical; passed on to the pointwise summary or used to filter `NA`s
  before computing depth.

- depth:

  depth method to use. `NULL` (default) gives the pointwise
  min/max/range. Supply a depth name (e.g. `"MHI"`) or a custom depth
  function for depth-based selection.

## Value

a `tf` object.

## See also

[`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md),
[`rank.tf()`](https://tidyfun.github.io/tf/reference/tf_order.md)

Other tidyfun ordering and ranking functions:
[`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md),
[`tf_order`](https://tidyfun.github.io/tf/reference/tf_order.md)

## Examples

``` r
x <- tf_rgp(5) + 1:5
# pointwise (default):
min(x)
#> tfd[1]: [0,1] -> [-1.24565,2.273433] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ▇████▇▇▆▆▅▅▄▃▃▂▂▁▁▁▁▁▁▁▂▂▂
max(x)
#> tfd[1]: [0,1] -> [4.863887,6.122499] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ▃▃▃▃▃▃▄▃▃▂▂▁▁▁▁▁▁▁▂▃▅▇████
# depth-based:
min(x, depth = "MHI")
#> tfd[1]: [0,1] -> [-1.24565,2.382311] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ████▇▇▆▆▆▅▄▄▃▃▂▂▁▁▁▁▁▁▁▁▂▂
max(x, depth = "MHI")
#> tfd[1]: [0,1] -> [4.863887,6.122499] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ▃▃▃▃▃▃▄▃▃▂▂▁▁▁▁▁▁▁▂▃▅▇███▇
```
