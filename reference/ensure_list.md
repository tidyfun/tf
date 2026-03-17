# Turns any object into a list

See above.

## Usage

``` r
ensure_list(x)
```

## Arguments

- x:

  any input.

## Value

`x` turned into a list.

## See also

Other tidyfun developer tools:
[`prep_plotting_arg()`](https://tidyfun.github.io/tf/reference/prep_plotting_arg.md),
[`unique_id()`](https://tidyfun.github.io/tf/reference/unique_id.md)

## Examples

``` r
ensure_list(1:3)
#> [[1]]
#> [1] 1 2 3
#> 
ensure_list(list(1, 2))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
```
