# Tukey's Five Number Summary for `tf` vectors

Computes a depth-based five number summary for functional data: the
observations with minimum, lower-hinge, median, upper-hinge, and maximum
depth values.

## Usage

``` r
fivenum(x, na.rm = FALSE, ...)

# Default S3 method
fivenum(x, na.rm = FALSE, ...)

# S3 method for class 'tf'
fivenum(x, na.rm = FALSE, depth = "MHI", ...)
```

## Arguments

- x:

  a `tf` vector (or numeric for the default method).

- na.rm:

  logical; if `TRUE`, `NA` observations are removed first.

- ...:

  passed to
  [`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md).

- depth:

  depth method for ordering. See
  [`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md).
  Defaults to `"MHI"` for an up-down ordering.

## Value

**`fivenum.tf`**: a named `tf` vector of length 5.  
**`fivenum.default`**: see
[`stats::fivenum()`](https://rdrr.io/r/stats/fivenum.html).

## See also

Other tidyfun summary functions:
[`functionwise`](https://tidyfun.github.io/tf/reference/functionwise.md),
[`tfsummaries`](https://tidyfun.github.io/tf/reference/tfsummaries.md)

## Examples

``` r
set.seed(1)
f <- tf_rgp(7)
fivenum(f)
#> tfd[5]: [0,1] -> [-2.191315,2.185118] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> min        : ▅▅▅▅▄▄▃▃▂▂▂▂▂▂▂▃▃▃▄▄▄▄▅▅▅▅
#> lower_hinge: ▆▆▆▆▆▆▅▅▅▄▄▃▃▃▂▂▁▁▁▁▁▁▁▁▂▂
#> median     : ▅▅▅▅▅▅▅▅▅▅▅▅▅▅▅▅▆▆▆▇▇▇████
#> upper_hinge: ▃▄▄▅▆▆▇▇████▇▇▇▆▆▅▅▅▅▅▅▅▆▆
#> max        : ▆▆▆▆▇▆▆▆▆▅▅▅▅▅▅▅▅▅▆▆▆▆▅▅▄▄
```
