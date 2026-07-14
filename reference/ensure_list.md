# Wrap a non-list object in a list

Returns `x` unchanged if it is already a list, otherwise wraps it in a
one-element list. Used internally to normalize `arg` inputs that may be
either a single numeric vector or a list of per-curve vectors.

## Usage

``` r
ensure_list(x)
```

## Arguments

- x:

  any input.

## Value

`x` if it is a list, otherwise `list(x)`.

## See also

Other tidyfun utility functions:
[`in_range()`](https://tidyfun.github.io/tf/reference/in_range.md),
[`tf_arg()`](https://tidyfun.github.io/tf/reference/tfmethods.md),
[`tf_zoom()`](https://tidyfun.github.io/tf/reference/tf_zoom.md),
[`unique_id()`](https://tidyfun.github.io/tf/reference/unique_id.md)

## Examples

``` r
ensure_list(1:3)
#> [[1]]
#> [1] 1 2 3
#> 
ensure_list(list(1:3, 4:6))
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 4 5 6
#> 
```
