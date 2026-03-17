# Make syntactically valid unique names

See above.

## Usage

``` r
unique_id(x)
```

## Arguments

- x:

  any input.

## Value

`x` turned into a list.

## See also

Other tidyfun developer tools:
[`ensure_list()`](https://tidyfun.github.io/tf/reference/ensure_list.md),
[`prep_plotting_arg()`](https://tidyfun.github.io/tf/reference/prep_plotting_arg.md)

## Examples

``` r
unique_id(c("a", "b", "a"))
#> [1] "a"   "b"   "a.1"
```
