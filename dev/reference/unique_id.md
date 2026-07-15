# Make syntactically valid unique names

Coerces `x` to character and returns syntactically valid, unique
identifiers. Empty strings are replaced with `"NA"` before
deduplication. If `x` already has no duplicates it is returned
unchanged.

## Usage

``` r
unique_id(x)
```

## Arguments

- x:

  any input that can be coerced to character.

## Value

A character vector of unique, syntactically valid names of the same
length as `x`.

## See also

Other tidyfun utility functions:
[`ensure_list()`](https://tidyfun.github.io/tf/dev/reference/ensure_list.md),
[`in_range()`](https://tidyfun.github.io/tf/dev/reference/in_range.md),
[`tf_arg()`](https://tidyfun.github.io/tf/dev/reference/tfmethods.md),
[`tf_zoom()`](https://tidyfun.github.io/tf/dev/reference/tf_zoom.md)

## Examples

``` r
unique_id(c("a", "a", "b"))
#> [1] "a"   "a.1" "b"  
unique_id(c(1, 1, 2))
#> [1] "X1"   "X1.1" "X2"  
```
