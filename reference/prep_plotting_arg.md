# Preprocess evaluation grid for plotting

(internal function exported for re-use in upstream packages)

## Usage

``` r
prep_plotting_arg(f, n_grid)
```

## Arguments

- f:

  a `tf`-object.

- n_grid:

  length of evaluation grid.

## Value

a semi-regular grid rounded down to appropriate resolution.

## See also

Other tidyfun developer tools:
[`ensure_list()`](https://tidyfun.github.io/tf/reference/ensure_list.md),
[`unique_id()`](https://tidyfun.github.io/tf/reference/unique_id.md)

## Examples

``` r
f <- tfd(sin(seq(0, 2 * pi, length.out = 21)), arg = seq(0, 1, length.out = 21))
prep_plotting_arg(f, n_grid = 50)
#>  [1] 0.000 0.020 0.041 0.050 0.061 0.082 0.100 0.102 0.122 0.143 0.150 0.163
#> [13] 0.184 0.200 0.204 0.224 0.245 0.250 0.265 0.286 0.300 0.306 0.327 0.347
#> [25] 0.350 0.367 0.388 0.400 0.408 0.429 0.449 0.450 0.469 0.490 0.500 0.510
#> [37] 0.531 0.550 0.551 0.571 0.592 0.600 0.612 0.633 0.650 0.653 0.673 0.694
#> [49] 0.700 0.714 0.735 0.750 0.755 0.776 0.796 0.800 0.816 0.837 0.850 0.857
#> [61] 0.878 0.898 0.900 0.918 0.939 0.950 0.959 0.980 1.000
```
