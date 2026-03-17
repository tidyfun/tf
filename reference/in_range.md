# Find out if values are inside given bounds

`in_range` and its infix-equivalent `%inr%` return `TRUE` for all values
in the numeric vector `f` that are within the range of values in `r`.

## Usage

``` r
in_range(f, r)

f %inr% r
```

## Arguments

- f:

  a numeric vector.

- r:

  numeric vector used to specify a range, only the minimum and maximum
  of `r` are used.

## Value

a `logical` vector of the same length as `f`.

## See also

Other tidyfun utility functions:
[`tf_arg()`](https://tidyfun.github.io/tf/reference/tfmethods.md),
[`tf_zoom()`](https://tidyfun.github.io/tf/reference/tf_zoom.md)

## Examples

``` r
in_range(1:10, c(3, 7))
#>  [1] FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
1:10 %inr% c(3, 7)
#>  [1] FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
```
